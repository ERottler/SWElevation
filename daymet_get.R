###

#Daymet SWE Download
#Erwin Rottler, University of Potsdam

###

library(devtools)
library(sp)
library(daymetr)
library(ncdf4)
library(ncdf4.helpers)
# devtools::install_github("https://github.com/ERottler/meltimr")
library(meltimr)
library(parallel)
library(doParallel)
library(raster)
library(rgdal)
library(RSAGA)
library(gdalUtils)
library(viridis)

daymet_dir <- "D:/nrc_user/rottler/daymet/clear_water/"
grdc_dir <- "D:/nrc_user/rottler/grdc/grdc_03_2021/"

crswgs84 <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

#get_daymet----

# grdc_catch_1 <- rgdal::readOGR(paste0(grdc_dir, "watersheds/stationbasins_1.geojson"))
grdc_catch_2 <- rgdal::readOGR(paste0(grdc_dir, "watersheds/stationbasins_2.geojson"))
# grdc_catch_3 <- rgdal::readOGR(paste0(grdc_dir, "watersheds/stationbasins_3.geojson"))
# grdc_catch_4 <- rgdal::readOGR(paste0(grdc_dir, "watersheds/stationbasins_4.geojson"))
# grdc_catch_5 <- rgdal::readOGR(paste0(grdc_dir, "watersheds/stationbasins_5.geojson"))

basin_sel <- grdc_catch_2[which(grdc_catch_2@data$grdc_no == 4116301), ] #Clear Water River, Orofino (GRDC-ID: 4116301)
# basin_sel <- grdc_catch_2[which(grdc_catch_2@data$grdc_no == 4115201), ] #Beaver Army Terminal (GRDC-ID: 4115201)

raster::plot(basin_sel)
basin_sel@bbox
lat1 <- basin_sel@bbox[4]
lon1 <- basin_sel@bbox[1]
lat2 <- basin_sel@bbox[2]
lon2 <- basin_sel@bbox[3]

#Minimum temperature
tictoc::tic()
download_daymet_ncss(location = c(lat1, lon1, lat2, lon2),
                     start = 1980,
                     end = 2019,
                     frequency = "daily",
                     param = c("tmin"),
                     path = paste0(daymet_dir, "tmin"),
                     silent = TRUE)
tictoc::toc()

#Maximum temperature
tictoc::tic()
download_daymet_ncss(location = c(lat1, lon1, lat2, lon2),
                     start = 1980,
                     end = 2019,
                     frequency = "daily",
                     param = c("tmax"),
                     path = paste0(daymet_dir, "tmax"),
                     silent = TRUE)
tictoc::toc()

#Precipitation
tictoc::tic()
download_daymet_ncss(location = c(lat1, lon1, lat2, lon2),
                     start = 1980,
                     end = 2019,
                     frequency = "daily",
                     param = c("prcp"),
                     path = paste0(daymet_dir, "prcp"),
                     silent = TRUE)
tictoc::toc()

#Solar radiation
tictoc::tic()
download_daymet_ncss(location = c(lat1, lon1, lat2, lon2),
                     start = 1980,
                     end = 2019,
                     frequency = "daily",
                     param = c("srad"),
                     path = paste0(daymet_dir, "srad"),
                     silent = TRUE)
tictoc::toc()

#Vapour pressure
tictoc::tic()
download_daymet_ncss(location = c(lat1, lon1, lat2, lon2),
                     start = 1980,
                     end = 2019,
                     frequency = "daily",
                     param = c("vp"),
                     path = paste0(daymet_dir, "vapo"),
                     silent = TRUE)
tictoc::toc()

#pre_meteo----

#select watershed
basin_swe <- grdc_catch_2[which(grdc_catch_2@data$grdc_no == 4116301), ]
raster::crs(basin_swe) <- crswgs84

#list paths to nc-files
tmin_nc_paths <- list.files(paste0(daymet_dir, "tmin/"), full.names = T)
tmax_nc_paths <- list.files(paste0(daymet_dir, "tmax/"), full.names = T)
prcp_nc_paths <- list.files(paste0(daymet_dir, "prcp/"), full.names = T)
srad_nc_paths <- list.files(paste0(daymet_dir, "srad/"), full.names = T)
vapo_nc_paths <- list.files(paste0(daymet_dir, "vapo/"), full.names = T)
  
tmin_all <- NULL
tmax_all <- NULL
prcp_all <- NULL
srad_all <- NULL
vapo_all <- NULL
date_met <- NULL

for(i in 1:length(tmin_nc_paths)){
  
  print(paste(i, "of", length(tmin_nc_paths), Sys.time()))
  
  nc_tmin_sel <- ncdf4::nc_open(tmin_nc_paths[i])
  nc_tmax_sel <- ncdf4::nc_open(tmax_nc_paths[i])
  nc_prcp_sel <- ncdf4::nc_open(prcp_nc_paths[i])
  nc_srad_sel <- ncdf4::nc_open(srad_nc_paths[i])
  nc_vapo_sel <- ncdf4::nc_open(vapo_nc_paths[i])
  
  #get longitude, latitude and date
  lon <- ncdf4::ncvar_get(nc_tmin_sel, varid = "lon")
  lat <- ncdf4::ncvar_get(nc_tmin_sel, varid = "lat")
  date_met_sel <- as.Date(as.character(nc.get.time.series(nc_tmin_sel, time.dim.name = "time")))
  
  #spatial grid points from lat/lon
  grid_points_cube_84 <-  sp::SpatialPoints(data.frame(lon = c(lon), lat = c(lat)),
                                            proj4string =  crswgs84)

  #grid points inside watersheds
  inside_sel <- !is.na(sp::over(grid_points_cube_84, as(basin_swe, "SpatialPolygons")))

  grid_points_sel <- grid_points_cube_84[which(inside_sel == T)]

  #Select cells for Basel/Cochem watershed
  lat_in_sel <- grid_points_sel@coords[, 2]

  get_cube_index_col <- function(val_in, coor_in = lon2D, col_or_row = "col"){

    if(col_or_row == "col"){
      index_out <- which(round(coor_in, digits =6) == round(val_in, digits =6), arr.ind = T)[1,1]
    }

    if(col_or_row == "row"){
      index_out <- which(round(coor_in, digits =6) == round(val_in, digits =6), arr.ind = T)[1,2]
    }

    return(index_out)

  }
  get_cube_index_row <- function(val_in, coor_in = lon2D, col_or_row = "row"){
    if(col_or_row == "col"){
      index_out <- which(round(coor_in, digits =6) == round(val_in, digits =6), arr.ind = T)[1,1]
    }

    if(col_or_row == "row"){
      index_out <- which(round(coor_in, digits =6) == round(val_in, digits =6), arr.ind = T)[1,2]
    }

    return(index_out)
  }

  my_get_cube_col <- function(val_in, coor_in = lat){

    get_cube_index_col(val_in = val_in, coor_in = coor_in)

  }
  my_get_cube_row <- function(val_in, coor_in = lat){

    get_cube_index_row(val_in = val_in, coor_in = coor_in)

  }

  #get index in cube from points inside sub-basins
  index_col_sel <- sapply(lat_in_sel, my_get_cube_col)
  index_row_sel <- sapply(lat_in_sel, my_get_cube_row)

  #data cubes
  tmin_cube <- ncvar_get(nc_tmin_sel, start = c(1, 1, 1), count = c(nrow(lon), ncol(lon), length(date_met_sel)), varid = "tmin")
  tmax_cube <- ncvar_get(nc_tmax_sel, start = c(1, 1, 1), count = c(nrow(lon), ncol(lon), length(date_met_sel)), varid = "tmax")
  prcp_cube <- ncvar_get(nc_prcp_sel, start = c(1, 1, 1), count = c(nrow(lon), ncol(lon), length(date_met_sel)), varid = "prcp")
  srad_cube <- ncvar_get(nc_srad_sel, start = c(1, 1, 1), count = c(nrow(lon), ncol(lon), length(date_met_sel)), varid = "srad")
  vapo_cube <- ncvar_get(nc_vapo_sel, start = c(1, 1, 1), count = c(nrow(lon), ncol(lon), length(date_met_sel)), varid = "vp")
  
  #Make cluster for parallel computing
  n_cores <- 2
  my_clust <- makeCluster(n_cores)
  clusterEvalQ(my_clust, pacman::p_load(zoo, zyp, alptempr, raster))
  clusterExport(my_clust, c("index_col_sel", "index_row_sel"))
  registerDoParallel(my_clust)

  tmin_sel <- foreach(i = 1:length(index_col_sel), .combine = 'cbind') %dopar% {
    tmin_cube[index_col_sel[i], index_row_sel[i], ]
  }
  tmax_sel <- foreach(i = 1:length(index_col_sel), .combine = 'cbind') %dopar% {
    tmax_cube[index_col_sel[i], index_row_sel[i], ]
  }
  prcp_sel <- foreach(i = 1:length(index_col_sel), .combine = 'cbind') %dopar% {
    prcp_cube[index_col_sel[i], index_row_sel[i], ]
  }
  srad_sel <- foreach(i = 1:length(index_col_sel), .combine = 'cbind') %dopar% {
    srad_cube[index_col_sel[i], index_row_sel[i], ]
  }
  vapo_sel <- foreach(i = 1:length(index_col_sel), .combine = 'cbind') %dopar% {
    vapo_cube[index_col_sel[i], index_row_sel[i], ]
  }

  stopCluster(my_clust)

  tmin_all <- rbind(tmin_all, tmin_sel)
  tmax_all <- rbind(tmax_all, tmax_sel)
  prcp_all <- rbind(prcp_all, prcp_sel)
  srad_all <- rbind(srad_all, srad_sel)
  vapo_all <- rbind(vapo_all, vapo_sel)
  date_met <- c(date_met, as.character(date_met_sel))
  
}

#calc_tmea----

#average from tmax and tmin
tmea_all <- (tmax_all+tmin_all)/2


#get_elevs----

dem_dir <- "D:/nrc_user/rottler/dem_us/"

#Paths DEM files
path_dem_46_115 <- paste0(dem_dir, "raw/USGS_13_n46w115.tif")
path_dem_46_116 <- paste0(dem_dir, "raw/USGS_13_n46w116.tif")
path_dem_46_117 <- paste0(dem_dir, "raw/USGS_13_n46w117.tif")
path_dem_47_115 <- paste0(dem_dir, "raw/USGS_13_n47w115.tif")
path_dem_47_116 <- paste0(dem_dir, "raw/USGS_13_n47w116.tif")
path_dem_47_117 <- paste0(dem_dir, "raw/USGS_13_n47w117.tif")

#load DEM files
dem_46_115 <- raster(path_dem_46_115)
dem_46_116 <- raster(path_dem_46_116)
dem_46_117 <- raster(path_dem_46_117)
dem_47_115 <- raster(path_dem_47_115)
dem_47_116 <- raster(path_dem_47_116)
dem_47_117 <- raster(path_dem_47_117)

dem_list <- list(dem_46_115, dem_46_116, dem_46_117, dem_47_115, dem_47_116, dem_47_117)
# add filename to same a .tif
dem_list$filename <- paste0(dem_dir, "processed/us_dem_30.tif")
us_dem_30 <- do.call(merge, dem_list)
# us_dem_30 <- raster(paste0(dem_dir, "processed/us_dem_30.tif"))

elevs_sel <- raster::extract(us_dem_30, grid_points_sel, na.rm = T)


#save_data----

save(tmin_all, tmax_all, tmea_all, prcp_all, srad_all, vapo_all, 
     elevs_sel, date_met, 
     file = paste0(daymet_dir, "processed/daymet_meteo_prep.RData"))


