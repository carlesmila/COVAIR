#-----------------------------------------------------------------------------#
#                       Modelling algorithm function                          #
#-----------------------------------------------------------------------------#


# Modelling function for all exposures
rf_ranger <- function(data, outcomevar, nthreads, idvar="ID", fmod=F){

  # Required packages
  require("CAST")
  require("caret")
  require("parallel")
  require("ranger")
  require("dplyr")

  # Candidate covariates
  covsnames <- setdiff(names(data), c(outcomevar, idvar, c("date", "x", "y", "res", "yday")))

  # Finetune mtry
  cat("Fine-tuning RF parameters...\n")
  cvindinces <- CreateSpacetimeFolds(data, spacevar=idvar, k=5, seed=1234)
  rangerGrid <- expand.grid(mtry = seq(2, round(length(covsnames)*3/4), 4),
                            min.node.size = 5, splitrule = "variance")
  tunedmod <- train(data[,covsnames], unlist(data[,outcomevar]), method="ranger",
                    num.trees=100, seed=1234,
                    num.threads=nthreads, metric="RMSE",
                    tuneGrid=rangerGrid,
                    trControl=trainControl(method="cv", index = cvindinces$index))

  # Fitting final model with inbag OOB, and variable importance
  cat("Fitting final model...")
  rangerGrid <- expand.grid(mtry = tunedmod$bestTune$mtry,
                            min.node.size = 5, splitrule = "variance")
  if(isTRUE(fmod)){
    finalmod <- train(data[,covsnames], unlist(data[,outcomevar]), method="ranger",
                      num.trees=300, seed=1234,
                      num.threads=nthreads, metric="RMSE",
                      importance="impurity", keep.inbag=T, quantreg=T,
                      tuneGrid=rangerGrid,
                      trControl=trainControl(method="none"))
  }else{
    finalmod <- train(data[,covsnames], unlist(data[,outcomevar]), method="ranger",
                      num.trees=300, seed=1234,
                      num.threads=nthreads, metric="RMSE",
                      tuneGrid=rangerGrid,
                      trControl=trainControl(method="none"))
  }
  return(finalmod)
}

#### Prediction function ----
pred_exposure <- function(ydpreds, mod, kridgedres=NULL){
  
  # Predict response and se from final model
  vars <- row.names(varImp(mod)$importance)
  ydpreds <- dplyr::select(ydpreds, all_of(vars))
  ydpreds_df <- as.data.frame(ydpreds) 
  comppixels <- complete.cases(ydpreds_df)
  ydpreds_df <- ydpreds_df[comppixels,] 
  ydpreds$pred_trend <- NA
  ydpreds$se <- NA
  ydpreds$pred_trend[comppixels] <- predict(mod$finalModel, ydpreds_df)$predictions
  ydpreds$se[comppixels] <- (predict(mod$finalModel, ydpreds_df, type="quantiles", quantiles = 0.841)$predictions -
                               predict(mod$finalModel, ydpreds_df, type="quantiles", quantiles = 0.159)$predictions) /2
  if(!is.null(kridgedres)){
    ydpreds <- c(ydpreds, kridgedres)
    ydpreds <- mutate(ydpreds, pred = pred_trend + pred_res)
    ydpreds <- dplyr::select(ydpreds, pred, se)
  }else{
    ydpreds <- mutate(ydpreds, pred = pred_trend)
    ydpreds <- dplyr::select(ydpreds, pred, se)
  }
  
  # Return 
  return(ydpreds)
}

# Helper: Read daily tif 
read_ydtif <- function(path, yd){
  ydtif <- read_stars(path, RasterIO = list(bands = yd), proxy = F)
  names(ydtif) <- gsub(".tif", "", names(ydtif))
  ydtif
}

# Helper: Kriging of test data for CV
multikrige <- function(trst, tesf, dat, mod){
  krigeday <- krige(res1 ~ 1,
                    trst[trst$date == dat,],
                    newdata = tesf[tesf$date == dat,],
                    model = mod, debug.level = 0)
  cbind(st_drop_geometry(tesf[tesf$date == dat,c("ID", "date")]), 
        st_drop_geometry(krigeday))
}

# Helper: Store stars raster stack as NetCDF
write_stars_nc <- function(starspred, datemin, units, path){
  
  # Define some straightforward dimensions
  x <- ncdim_def("x", "meters", st_get_dimension_values(starspred, "x"))
  y <- ncdim_def("y", "meters", st_get_dimension_values(starspred, "y"))
  t <- ncdim_def("time", paste0("days since ", datemin, " 00:00:00"), 
                 1:length(st_get_dimension_values(starspred, "time")), unlim=TRUE)
  
  # Define  variables
  var_pred <- ncvar_def(name = "pred",
                        units = units,
                        dim = list(x, y, t),
                        missval = -9999,
                        compression = 9)
  var_setrend <- ncvar_def(name = "se",
                           units = units,
                           dim = list(x, y, t),
                           missval = -9999,
                           compression = 9)
  
  # Add the variables to the file
  filename <- path
  ncout <- nc_create(filename, list(var_pred, var_setrend), force_v4 = TRUE)
  
  # Place the raster values in the file
  # need to loop through the layers to get them  to match to correct time index
  for (i in 1:dim(starspred)[3]) { 
    ncvar_put(nc = ncout, 
              varid = var_pred, 
              vals = c(adrop(starspred[1,,,i])[[1]]), 
              start = c(1, 1, i), 
              count = c(-1, -1, 1))
    ncvar_put(nc = ncout, 
              varid = var_setrend, 
              vals = c(adrop(starspred[2,,,i])[[1]]), 
              start = c(1, 1, i), 
              count = c(-1, -1, 1))
  }
  
  # put additional attributes into dimension and data variables
  ncatt_put(ncout, "x", "axis", "X") 
  ncatt_put(ncout,"x","standard_name","projection_x_coordinate")
  ncatt_put(ncout, "y", "axis", "Y")
  ncatt_put(ncout,"y","standard_name","projection_y_coordinate")
  ncatt_put(ncout, "time", "axis", "T")
  
  # Close the netcdf file when finished
  nc_close(ncout)
  
}

# Helper: Krige residuals for a raster
krige_res <- function(residdata, griddata, varmod){
  
  krige_yd <- krige(res ~ 1, 
                    locations = suppressWarnings(st_transform(residdata, crs = st_crs(griddata))), 
                    # Same CRS but some comp are diff 
                    newdata = griddata, model = varmod, debug.level = 0)
  krige_yd <- c(st_rasterize(krige_yd[,1], spstack),
                suppressWarnings(sqrt(st_rasterize(krige_yd[,2], spstack))))
  names(krige_yd) <- c("pred_res", "se_res")
  krige_yd
}
