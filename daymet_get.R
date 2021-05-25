###

#Daymet SWE Download
#Erwin Rottler, University of Potsdam

###

#get_daymet----

#GRDC watersheds boundaries
#total data set downloaded in five chunks
grdc_catch_1 <- rgdal::readOGR(paste0(grdc_catc_dir, "/stationbasins_1.geojson"))
grdc_catch_2 <- rgdal::readOGR(paste0(grdc_catc_dir, "/stationbasins_2.geojson"))
grdc_catch_3 <- rgdal::readOGR(paste0(grdc_catc_dir, "/stationbasins_3.geojson"))
grdc_catch_4 <- rgdal::readOGR(paste0(grdc_catc_dir, "/stationbasins_4.geojson"))
grdc_catch_5 <- rgdal::readOGR(paste0(grdc_catc_dir, "/stationbasins_5.geojson"))

grdc_catch <- rbind(grdc_catch_1, grdc_catch_2, grdc_catch_3, grdc_catch_4, grdc_catch_5)

basin_sel <- grdc_catch[which(grdc_catch@data$grdc_no == id_sel), ] 

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
                     path = paste0(daymet_dir, "raw/tmin"),
                     silent = TRUE)
tictoc::toc()

#Maximum temperature
tictoc::tic()
download_daymet_ncss(location = c(lat1, lon1, lat2, lon2),
                     start = 1980,
                     end = 2019,
                     frequency = "daily",
                     param = c("tmax"),
                     path = paste0(daymet_dir, "raw/tmax"),
                     silent = TRUE)
tictoc::toc()

#Precipitation
tictoc::tic()
download_daymet_ncss(location = c(lat1, lon1, lat2, lon2),
                     start = 1980,
                     end = 2019,
                     frequency = "daily",
                     param = c("prcp"),
                     path = paste0(daymet_dir, "raw/prcp"),
                     silent = TRUE)
tictoc::toc()

#Solar radiation
tictoc::tic()
download_daymet_ncss(location = c(lat1, lon1, lat2, lon2),
                     start = 1980,
                     end = 2019,
                     frequency = "daily",
                     param = c("srad"),
                     path = paste0(daymet_dir, "raw/srad"),
                     silent = TRUE)
tictoc::toc()

#Vapour pressure
tictoc::tic()
download_daymet_ncss(location = c(lat1, lon1, lat2, lon2),
                     start = 1980,
                     end = 2019,
                     frequency = "daily",
                     param = c("vp"),
                     path = paste0(daymet_dir, "raw/vapo"),
                     silent = TRUE)
tictoc::toc()



