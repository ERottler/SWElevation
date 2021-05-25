###

#Main file snow simulations US
#Erwin Rottler, University of Potsdam

###



pacman::p_load(ncdf4, ncdf4.helpers, PCICt, dplyr, readr, tidyr, rgeos, ggplot2, 
               sp, viridis, rgdal, leaflet, ggmap, zoo, zyp, alptempr, lmomco, 
               raster, foreach, rfs, dismo, XML, parallel, doParallel, Lmoments,
               shape, devtools, pbapply, profvis, RColorBrewer, viridis, Rcpp, rEchseSnow,
               Rlibeemd, xts, emdbook, rfs, meltimr, readr, tmap, sf, hydroGOF,
               daymetr, RSAGA, gdalUtils)

#select GRDC station
# id_sel <- "4116301" # Orofino, Clearwater River
# id_sel <- "4116300" # Spalding, Clearwater River
# id_sel <- "4116325" # Near Lowell, ID, Lochsa River
id_sel <- "4116320" # Near Lowell, ID, Selway River

#set_paths
base_dir <- "U:/SWElelevation/R/SWElevation/"
daymet_dir <- paste0("D:/nrc_user/rottler/daymet/", id_sel, "/")
grdc_catc_dir <- "D:/nrc_user/rottler/grdc/grdc_03_2021/watersheds/"

#number of cores used for parallel computing
n_cores <- 10


#get daymet data
source(paste0(base_dir, "daymet_get.R"))

rm(list = ls())
.rs.restartR()

#prepare daymet daty
source(paste0(base_dir, "daymet_get.R"))

rm(list = ls())
.rs.restartR()

#swe simulations
source(paste0(base_dir, "swe_simu.R"))

stopCluster(my_clust)
rm(list = ls())
.rs.restartR()

#analyse input/ouput simulatins
source(paste0(base_dir, "res_analy.R"))

stopCluster(my_clust)
rm(list = ls())
.rs.restartR()

#prepare data for dashboard
source(paste0(base_dir, "res_dashb.R"))

rm(list = ls())
.rs.restartR()


