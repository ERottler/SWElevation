###

#Elevation-dependent compensation effects in snowmelt
#Elevation band data from CAMELS-US
#Erwin Rottler, University of Potsdam

###

pacman::p_load(zyp, meltimr)

base_dir <- "/home/erwin/Nextcloud/pdoc_up/SWElevation/"
data_dir <- "/home/erwin/Documents/storage_research/camels_us/basin_timeseries_v1p2_metForcing_obsFlow/basin_dataset_public_v1p2/"

#mag_doy_slo----

#USGS hydrologic unit code
hu_code <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", 
             "11", "12", "13", "14", "15", "16", "17", "18")

#Calculate trends magnitude and timing annual maximum snowmelt events

swe_slo <- NULL

#loop over hydrological units
for(i in 1:length(hu_code)){
  
  #list meta file paths von hydrologic unit
  list_meta_files <- list.files(paste0(data_dir, "elev_bands_forcing/daymet/", hu_code[i]), pattern = ".list", full.names = T)
  
  #get watershed IDs
  list_meta_names <- list.files(paste0(data_dir, "elev_bands_forcing/daymet/", hu_code[i]), pattern = ".list", full.names = F)
  watershed_IDs <- sub(".list", "", list_meta_names)
  
  #loop over watersheds in hydrological unit
  for(f in 1:length(list_meta_files)){
    
    print(paste("HU:", i, "from 18", " - ", "Basin", f, "from", length(list_meta_files)))
    
    #Basin data incomplete; no calculations possible
    if(i == 6 && f == 2){
      band_res <- c(hu_code[i], watershed_IDs[f], rep(NA, 7)) 
      swe_slo <- rbind(swe_slo, band_res)
    }else{
      
      #get meta information on watershed
      bands_meta <- read.table(list_meta_files[f])
      
      #loop over elevation bands in watershed
      for(k in 1:nrow(bands_meta)){
        
        band_data <- read.table(paste0(data_dir, "elev_bands_forcing/daymet/", hu_code[i], "/", rownames(bands_meta)[k]),  
                                skip = 4, sep = "", header = F, stringsAsFactors = F, na.strings = c(-999), dec = ".")
        
        #get base elevation
        base_elev <- readLines(paste0(data_dir, "elev_bands_forcing/daymet/", hu_code[i], "/", rownames(bands_meta)[k]),  2)[2]
        base_elev <- as.numeric(base_elev)
        
        #get date
        date_band <- as.Date(strptime(paste(band_data$V1, band_data$V2, band_data$V3), "%Y%m%d", tz="UTC"))
        sta_yea <- min_na(format(date_band, "%Y"))
        end_yea <- max_na(format(date_band, "%Y"))
        
        #percentage NA
        na_count <- length(which(is.na(band_data$V8)))
        na_perc <- na_count / length(band_data$V8)
        
        band_data$swe_dif <- c(NA, diff(band_data$V8))
        
        band_data$swe_sum <- zoo::rollapply(data = band_data$swe_dif, width = 10,
                                            FUN = sum_na, align = "center", fill = NA)
        #annual snowmelt maxima
        swe_band_day <- ord_day(date = date_band,
                                data_in = band_data$swe_sum,
                                start_y = sta_yea,
                                end_y = end_yea,
                                break_day = 0,
                                do_ma = F)
        
        f_max_doy <- function(data_in){
          ind_max <- min_na(which(data_in == max_na(data_in)))
          return(ind_max)
        }
        
        f_max_mag <- function(data_in){
          dat_max <- max_na(data_in)
          return(dat_max)
        }
        
        swe_max_mag <- apply(swe_band_day, 1, f_max_mag)
        swe_max_doy <- apply(swe_band_day, 1, f_max_doy)
        
        swe_max_mag_slo <- as.numeric(zyp.trend.vector(swe_max_mag, x = sta_yea:end_yea, method = "zhang", conf.intervals = F)[2])
        swe_max_doy_slo <- as.numeric(zyp.trend.vector(swe_max_doy, x = sta_yea:end_yea, method = "zhang", conf.intervals = F)[2])
        swe_max_mag_mea <- mea_na(swe_max_mag)
        swe_max_doy_mea <- mea_na(swe_max_doy)
        
        band_res <- c(hu_code[i], 
                      watershed_IDs[f],
                      base_elev, 
                      sta_yea, 
                      end_yea,
                      na_perc,
                      swe_max_mag_mea, 
                      swe_max_doy_mea, 
                      swe_max_mag_slo, 
                      swe_max_doy_slo)
        
        swe_slo <- rbind(swe_slo, band_res)
        
        }
    }
  }
}

hu01_ind <- which(swe_slo[, 1] == "01")
hu02_ind <- which(swe_slo[, 1] == "02")
hu03_ind <- which(swe_slo[, 1] == "03")
hu04_ind <- which(swe_slo[, 1] == "04")
hu05_ind <- which(swe_slo[, 1] == "05")
hu06_ind <- which(swe_slo[, 1] == "06")
hu07_ind <- which(swe_slo[, 1] == "07")
hu08_ind <- which(swe_slo[, 1] == "08")
hu09_ind <- which(swe_slo[, 1] == "09")
hu10_ind <- which(swe_slo[, 1] == "10")
hu11_ind <- which(swe_slo[, 1] == "11")
hu12_ind <- which(swe_slo[, 1] == "12")
hu13_ind <- which(swe_slo[, 1] == "13")
hu14_ind <- which(swe_slo[, 1] == "14")
hu15_ind <- which(swe_slo[, 1] == "15")
hu16_ind <- which(swe_slo[, 1] == "16")
hu17_ind <- which(swe_slo[, 1] == "17")
hu18_ind <- which(swe_slo[, 1] == "18")

hu_ind_sel <- hu02_ind

plot(as.numeric(swe_slo[hu_ind_sel, 3]), as.numeric(swe_slo[hu_ind_sel, 7]))
plot(as.numeric(swe_slo[hu02_ind, 9]), as.numeric(swe_slo[hu02_ind, 10]))

abline(h = 0)
abline(v = 0)
boxplot(as.numeric(swe_slo[, 9]))
