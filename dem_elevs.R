###

#Get elevations for grid points
#Erwin Rottler, University of Potsdam

###


dem_dir <- "D:/nrc_user/rottler/dem_us/"

#prep_dem----

# dem_file_names <- list.files(paste0(dem_dir, "raw/"), full.names = T)
# 
# dem_file_names_n41 <- dem_file_names[which(grepl("n41", dem_file_names))]
# dem_file_names_n42 <- dem_file_names[which(grepl("n42", dem_file_names))]
# dem_file_names_n43 <- dem_file_names[which(grepl("n43", dem_file_names))]
# dem_file_names_n44 <- dem_file_names[which(grepl("n44", dem_file_names))]
# dem_file_names_n45 <- dem_file_names[which(grepl("n45", dem_file_names))]
# dem_file_names_n46 <- dem_file_names[which(grepl("n46", dem_file_names))]
# dem_file_names_n47 <- dem_file_names[which(grepl("n47", dem_file_names))]
# dem_file_names_n48 <- dem_file_names[which(grepl("n48", dem_file_names))]
# dem_file_names_n49 <- dem_file_names[which(grepl("n49", dem_file_names))]
# dem_file_names_n50 <- dem_file_names[which(grepl("n50", dem_file_names))]
# dem_file_names_n51 <- dem_file_names[which(grepl("n51", dem_file_names))]
# dem_file_names_n52 <- dem_file_names[which(grepl("n52", dem_file_names))]
# dem_file_names_n53 <- dem_file_names[which(grepl("n53", dem_file_names))]
# 
# dem_list_n41 <- vector("list", length(dem_file_names_n41))
# dem_list_n42 <- vector("list", length(dem_file_names_n42))
# dem_list_n43 <- vector("list", length(dem_file_names_n43))
# dem_list_n44 <- vector("list", length(dem_file_names_n44))
# dem_list_n45 <- vector("list", length(dem_file_names_n45))
# dem_list_n46 <- vector("list", length(dem_file_names_n46))
# dem_list_n47 <- vector("list", length(dem_file_names_n47))
# dem_list_n48 <- vector("list", length(dem_file_names_n48))
# dem_list_n49 <- vector("list", length(dem_file_names_n49))
# dem_list_n50 <- vector("list", length(dem_file_names_n50))
# dem_list_n51 <- vector("list", length(dem_file_names_n51))
# dem_list_n52 <- vector("list", length(dem_file_names_n52))
# dem_list_n53 <- vector("list", length(dem_file_names_n53))
# 
# for(i in 1:length(dem_file_names_n41)){
#   
#   dem_sel <- raster::raster(dem_file_names_n41[i])
#   dem_list_n41[[i]] <- dem_sel
#   
# }
# for(i in 1:length(dem_file_names_n42)){
#   
#   dem_sel <- raster::raster(dem_file_names_n42[i])
#   dem_list_n42[[i]] <- dem_sel
#   
# }
# for(i in 1:length(dem_file_names_n43)){
#   
#   dem_sel <- raster::raster(dem_file_names_n43[i])
#   dem_list_n43[[i]] <- dem_sel
#   
# }
# for(i in 1:length(dem_file_names_n44)){
#   
#   dem_sel <- raster::raster(dem_file_names_n44[i])
#   dem_list_n44[[i]] <- dem_sel
#   
# }
# for(i in 1:length(dem_file_names_n45)){
#   
#   dem_sel <- raster::raster(dem_file_names_n45[i])
#   dem_list_n45[[i]] <- dem_sel
#   
# }
# for(i in 1:length(dem_file_names_n46)){
#   
#   dem_sel <- raster::raster(dem_file_names_n46[i])
#   dem_list_n46[[i]] <- dem_sel
#   
# }
# for(i in 1:length(dem_file_names_n47)){
#   
#   dem_sel <- raster::raster(dem_file_names_n47[i])
#   dem_list_n47[[i]] <- dem_sel
#   
# }
# for(i in 1:length(dem_file_names_n48)){
#   
#   dem_sel <- raster::raster(dem_file_names_n48[i])
#   dem_list_n48[[i]] <- dem_sel
#   
# }
# for(i in 1:length(dem_file_names_n49)){
#   
#   dem_sel <- raster::raster(dem_file_names_n49[i])
#   dem_list_n49[[i]] <- dem_sel
#   
# }
# for(i in 1:length(dem_file_names_n50)){
#   
#   dem_sel <- raster::raster(dem_file_names_n50[i])
#   dem_list_n50[[i]] <- dem_sel
#   
# }
# for(i in 1:length(dem_file_names_n51)){
#   
#   dem_sel <- raster::raster(dem_file_names_n51[i])
#   dem_list_n51[[i]] <- dem_sel
#   
# }
# for(i in 1:length(dem_file_names_n52)){
#   
#   dem_sel <- raster::raster(dem_file_names_n52[i])
#   dem_list_n52[[i]] <- dem_sel
#   
# }
# for(i in 1:length(dem_file_names_n53)){
#   
#   dem_sel <- raster::raster(dem_file_names_n53[i])
#   dem_list_n53[[i]] <- dem_sel
#   
# }
# 
# us_dem_n41 <- do.call(merge, dem_list_n41)
# us_dem_n42 <- do.call(merge, dem_list_n42)
# us_dem_n43 <- do.call(merge, dem_list_n43)
# us_dem_n44 <- do.call(merge, dem_list_n44)
# us_dem_n45 <- do.call(merge, dem_list_n45)
# us_dem_n46 <- do.call(merge, dem_list_n46)
# us_dem_n47 <- do.call(merge, dem_list_n47)
# us_dem_n48 <- do.call(merge, dem_list_n48)
# us_dem_n49 <- do.call(merge, dem_list_n49)
# us_dem_n50 <- do.call(merge, dem_list_n50)
# us_dem_n51 <- do.call(merge, dem_list_n51)
# us_dem_n52 <- do.call(merge, dem_list_n52)
# us_dem_n53 <- do.call(merge, dem_list_n53)
# 
# us_dem_list <- list(us_dem_n41, us_dem_n42, us_dem_n43, us_dem_n44, us_dem_n45,
#                            us_dem_n46, us_dem_n47, us_dem_n48, us_dem_n49, us_dem_n50,
#                            us_dem_n51, us_dem_n52, us_dem_n53)
# 
# us_dem_list$filename <- paste0(dem_dir, "processed/us_dem_1arcs.tif")
# us_dem <- do.call(merge, us_dem_list)

#get_elevs----

us_dem <- raster(paste0(dem_dir, "processed/us_dem_1arcs.tif"))

tictoc::tic()
elevs_sel <- raster::extract(us_dem, grid_points_sel, buffer = 500, na.rm = T, fun = mea_na)
tictoc::toc()

#save_data----

save(elevs_sel, 
     file = paste0(daymet_dir, "processed/elevs_sel.RData"))

