#-----------------------------------------------------------------------------#
#                       Download MODIS MAIAC AOD and LST                      #
#-----------------------------------------------------------------------------#

# Download
# https://ladsweb.modaps.eosdis.nasa.gov/
library("MODIS")
library("filesstrings")

#### MAIAC ----
# https://lpdaac.usgs.gov/products/mcd19a2v006/

# Define search parameters
product <- "MCD19A2"
collection <- "006"
start_date <- as.Date("2018-01-01")
end_date <- as.Date("2020-01-01")
tileV <- 4
tileH <- 18
outDirPath <- "data/MAIAC/raw_hdf/"
job <- "MODIS_AOD"

# Log in to Earth data
MODIS::EarthdataLogin(usr = "******", pwd = "*****")

# Download HDF files
hdf_files <- getHdf(product=product, begin=start_date, end=end_date,
                    tileV=tileV, tileH=tileH, 
                    collection=collection, job=job, 
                    checkIntegrity=TRUE, forceDownload=TRUE)

# Move them to the right folder
file.move(unlist(hdf_files), outDirPath)


#### LST ----
# MOD11A1 - Terra https://lpdaac.usgs.gov/products/mod11a1v006/
# MYD11A1 - Aqua https://lpdaac.usgs.gov/products/myd11a1v006/

# Define search parameters
product1 <- "MOD11A1"
product2 <- "MYD11A1"
collection <- "006"
start_date <- as.Date("2018-01-01")
end_date <- as.Date("2020-12-31")
tileV <- 4
tileH <- 18
outDirPath <- "data/LST/raw_hdf/"
job <- "MODIS_LST"

# Download HDF files and move them to the right folder
terra_files <- getHdf(product=product1, begin=start_date, end=end_date,
                      tileV=tileV, tileH=tileH, 
                      collection=collection, job=job, 
                      checkIntegrity=TRUE, forceDownload=TRUE)
file.move(unlist(terra_files), outDirPath)

aqua_files <- getHdf(product=product2, begin=start_date, end=end_date,
                     tileV=tileV, tileH=tileH, 
                     collection=collection, job=job, 
                     checkIntegrity=TRUE, forceDownload=TRUE)
file.move(unlist(aqua_files), outDirPath)