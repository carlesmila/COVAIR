#-----------------------------------------------------------------------------#
#                   Preparing harpconvert scripts for TROPOMI                 #
#-----------------------------------------------------------------------------#

#### TROPOMI NO2 ----
no2_files1 <- list.files("data/TROPOMI_NO2/data1", 
                        pattern = ".nc4", recursive = T, full.names = T)
no2_lines1 <- paste0("harpconvert -a 'tropospheric_NO2_column_number_density_validity > 75; latitude > 40 [degree_north]; latitude < 44 [degree_north]; longitude > 0 [degree_east]; longitude < 4 [degree_east]; bin_spatial(161,40,0.025,161,0,0.025); derive(latitude {latitude}); derive(longitude {longitude}); keep(tropospheric_NO2_column_number_density)' ",
                     no2_files1, " ", gsub("/data1/", "/harp/", gsub(".nc4", ".nc", no2_files1)), "\n")
no2_files2 <- list.files("data/TROPOMI_NO2/data2", 
                         pattern = ".nc4", recursive = T, full.names = T)
no2_lines2 <- paste0("harpconvert -a 'tropospheric_NO2_column_number_density_validity > 75; latitude > 40 [degree_north]; latitude < 44 [degree_north]; longitude > 0 [degree_east]; longitude < 4 [degree_east]; bin_spatial(161,40,0.025,161,0,0.025); derive(latitude {latitude}); derive(longitude {longitude}); keep(tropospheric_NO2_column_number_density)' ",
                     no2_files2, " ", gsub("/data2/", "/harp/", gsub(".nc4", ".nc", no2_files2)), "\n")
no2_lines <- c(no2_lines1, no2_lines2)
no2_lines <- gsub("data/TROPOMI_NO2/", "", no2_lines)

sink("data/TROPOMI_NO2/harp_script.txt")
for(i in no2_lines){
  cat(i)
}
sink()


#### TROPOMI O3 ----
O3_files1 <- list.files("data/TROPOMI_O3/data1", 
                         pattern = ".nc4", recursive = T, full.names = T)
O3_lines1 <- paste0("harpconvert -a 'O3_column_number_density_validity>50; latitude > 40 [degree_north]; latitude < 44 [degree_north]; longitude > 0 [degree_east]; longitude < 4 [degree_east]; bin_spatial(161,40,0.025,161,0,0.025); derive(latitude {latitude}); derive(longitude {longitude}); keep(O3_column_number_density)' ",
                     O3_files1, " ", gsub("/data1/", "/harp/", gsub(".nc4", ".nc", O3_files1)), "\n")
O3_files2 <- list.files("data/TROPOMI_O3/data2", 
                         pattern = ".nc4", recursive = T, full.names = T)
O3_lines2 <- paste0("harpconvert -a 'O3_column_number_density_validity>50; latitude > 40 [degree_north]; latitude < 44 [degree_north]; longitude > 0 [degree_east]; longitude < 4 [degree_east]; bin_spatial(161,40,0.025,161,0,0.025); derive(latitude {latitude}); derive(longitude {longitude}); keep(O3_column_number_density)' ",
                     O3_files2, " ", gsub("/data2/", "/harp/", gsub(".nc4", ".nc", O3_files2)), "\n")
O3_files3 <- list.files("data/TROPOMI_O3/data3", 
                         pattern = ".nc4", recursive = T, full.names = T)
O3_lines3 <- paste0("harpconvert -a 'O3_column_number_density_validity>50; latitude > 40 [degree_north]; latitude < 44 [degree_north]; longitude > 0 [degree_east]; longitude < 4 [degree_east]; bin_spatial(161,40,0.025,161,0,0.025); derive(latitude {latitude}); derive(longitude {longitude}); keep(O3_column_number_density)' ",
                    O3_files3, " ", gsub("/data3/", "/harp/", gsub(".nc4", ".nc", O3_files3)), "\n")
O3_lines <- c(O3_lines1, O3_lines2, O3_lines3)
O3_lines <- gsub("data/TROPOMI_O3/", "", O3_lines)

sink("data/TROPOMI_O3/harp_script.txt")
for(i in O3_lines){
  cat(i)
}
sink()