###

#Main file snow simulations US
#Erwin Rottler, University of Potsdam

###

#settings----

pacman::p_load(ncdf4, ncdf4.helpers, PCICt, dplyr, readr, tidyr, rgeos, ggplot2, 
               sp, viridis, rgdal, leaflet, ggmap, zoo, zyp, alptempr, lmomco, 
               raster, foreach, rfs, dismo, XML, parallel, doParallel, Lmoments,
               shape, devtools, pbapply, profvis, RColorBrewer, viridis, Rcpp, rEchseSnow,
               Rlibeemd, xts, emdbook, rfs, meltimr, readr, tmap, sf, hydroGOF,
               daymetr, RSAGA, gdalUtils, tictoc)

#select GRDC station
# id_sel <- "4116301" # Orofino, Clearwater River
# id_sel <- "4116300" # Spalding, Clearwater River
# id_sel <- "4116325" # Near Lowell, ID, Lochsa River
# id_sel <- "4116320" # Near Lowell, ID, Selway River
# id_sel <- "4115450" # Near Missoula, MT, Bitterroot River
# id_sel <- "4115101" # Portland, OR, Willamette River
is_sel <- "4115231" # Monitor, WA, Wenatchee River

#set_paths
base_dir <- "U:/SWElelevation/R/SWElevation/"
daymet_dir <- paste0("D:/nrc_user/rottler/daymet/", id_sel, "/")
grdc_catc_dir <- "D:/nrc_user/rottler/grdc/grdc_03_2021/watersheds/"

#GRDC watersheds boundaries
grdc_catch_1 <- rgdal::readOGR(paste0(grdc_catc_dir, "/stationbasins_1.geojson"))
grdc_catch_2 <- rgdal::readOGR(paste0(grdc_catc_dir, "/stationbasins_2.geojson"))
grdc_catch_3 <- rgdal::readOGR(paste0(grdc_catc_dir, "/stationbasins_3.geojson"))
grdc_catch_4 <- rgdal::readOGR(paste0(grdc_catc_dir, "/stationbasins_4.geojson"))
grdc_catch_5 <- rgdal::readOGR(paste0(grdc_catc_dir, "/stationbasins_5.geojson"))

grdc_catch <- rbind(grdc_catch_1, grdc_catch_2, grdc_catch_3, grdc_catch_4, grdc_catch_5)
basin_sel <- grdc_catch[which(grdc_catch@data$grdc_no == id_sel), ] 
crswgs84 <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
raster::crs(basin_sel) <- crswgs84

#number of cores used for parallel computing
n_cores <- 20

#create directory
dir.create(daymet_dir)
dir.create(paste0(daymet_dir, "raw"))
dir.create(paste0(daymet_dir, "raw/tmin"))
dir.create(paste0(daymet_dir, "raw/tmax"))
dir.create(paste0(daymet_dir, "raw/srad"))
dir.create(paste0(daymet_dir, "raw/vapo"))
dir.create(paste0(daymet_dir, "raw/prcp"))
dir.create(paste0(daymet_dir, "processed"))

#source_files----

#get daymet data
Sys.time()
source(paste0(base_dir, "daymet_get.R"))

#prepare daymet data
Sys.time()
source(paste0(base_dir, "daymet_prep.R"))

#get elevations
Sys.time()
source(paste0(base_dir, "dem_elevs.R"))

#swe simulations
Sys.time()
source(paste0(base_dir, "swe_simu.R"))

#analyse input/ouput simulatins
Sys.time()
source(paste0(base_dir, "res_analy.R"))

#prepare data for dashboard
Sys.time()
source(paste0(base_dir, "res_dashb.R"))


#clean up
rm(list = ls())
.rs.restartR()


