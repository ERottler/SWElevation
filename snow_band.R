###

#Elevation-dependent compensation effects in snowmelt
#Elevation band data from CAMELS-US
#Erwin Rottler, University of Potsdam

###

pacman::p_load(zyp, meltimr, plotly, lubridate, plyr)

base_dir <- "U:/SWElelevation/R/SWElevation/"
data_dir <- "D:/nrc_user/rottler/camels_us/basin_timeseries_v1p2_metForcing_obsFlow/basin_dataset_public_v1p2/"
spat_dir <- "D:/nrc_user/rottler/camels_us/basin_set_full_res/"

#mag_doy_slo----

#USGS hydrologic unit code
hu_code <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", 
             "11", "12", "13", "14", "15", "16", "17", "18")

#Calculate trends magnitude and timing annual maximum snowmelt events

res_all <- NULL

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
      band_res <- c(hu_code[i], watershed_IDs[f], rep(NA, 16)) 
      res_all <- rbind(res_all, band_res)
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
        
        #get annual averages
        band_data$year <- floor_date(date_band, "year")
        
        pre_yea <- ddply(band_data, "year", summarise, values = mean(V6))
        sra_yea <- ddply(band_data, "year", summarise, values = mean(V7))
        swe_yea <- ddply(band_data, "year", summarise, values = mean(V8))
        tma_yea <- ddply(band_data, "year", summarise, values = mean(V9))
        tmi_yea <- ddply(band_data, "year", summarise, values = mean(V10))
        
        #mean values
        pre_mea <- mea_na(pre_yea$values)
        sra_mea <- mea_na(sra_yea$values)
        swe_mea <- mea_na(swe_yea$values)
        tma_mea <- mea_na(tma_yea$values)
        tmi_mea <- mea_na(tmi_yea$values)
        
        #linear trends (per decade)
        pre_slo <- as.numeric(zyp.trend.vector(pre_yea$values, x = sta_yea:end_yea, method = "zhang", conf.intervals = F)[2])*10
        sra_slo <- as.numeric(zyp.trend.vector(sra_yea$values, x = sta_yea:end_yea, method = "zhang", conf.intervals = F)[2])*10
        swe_slo <- as.numeric(zyp.trend.vector(swe_yea$values, x = sta_yea:end_yea, method = "zhang", conf.intervals = F)[2])*10
        tma_slo <- as.numeric(zyp.trend.vector(tma_yea$values, x = sta_yea:end_yea, method = "zhang", conf.intervals = F)[2])*10
        tmi_slo <- as.numeric(zyp.trend.vector(tmi_yea$values, x = sta_yea:end_yea, method = "zhang", conf.intervals = F)[2])*10
        
        #snowmelt events
        band_data$swe_dif <- c(NA, diff(band_data$V8))
        
        band_data$swe_sum <- zoo::rollapply(data = band_data$swe_dif, width = 14,
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
                      na_perc,
                      pre_mea, sra_mea, swe_mea, tma_mea, tmi_mea,
                      pre_slo, sra_slo, swe_slo, tma_slo, tmi_slo,
                      swe_max_mag_mea, 
                      swe_max_doy_mea, 
                      swe_max_mag_slo, 
                      swe_max_doy_slo)
        
        res_all <- rbind(res_all, band_res)
        
        }
    }
  }
}

#results to data frame
res_out <- data.frame(hu = res_all[, 1],
                      id = res_all[, 2],
                      base_elev = as.numeric(res_all[, 3]),
                      na_perc = as.numeric(res_all[, 4])*100,
                      pre_mea = as.numeric(res_all[, 5]),
                      sra_mea = as.numeric(res_all[, 6]),
                      swe_mea = as.numeric(res_all[, 7]),
                      tma_mea = as.numeric(res_all[, 8]),
                      tmi_mea = as.numeric(res_all[, 9]),
                      pre_slo = as.numeric(res_all[, 10]),
                      sra_slo = as.numeric(res_all[, 11]),
                      swe_slo = as.numeric(res_all[, 12]),
                      tma_slo = as.numeric(res_all[, 13]),
                      tmi_slo = as.numeric(res_all[, 14]),
                      mag_mea = as.numeric(res_all[, 14]),
                      doy_mea = as.numeric(res_all[, 14]),
                      mag_slo = as.numeric(res_all[, 14]),
                      doy_slo = as.numeric(res_all[, 14]),
                      stringsAsFactors = F
                      )

#Save as .RData
save(res_out, 
     file = paste0(base_dir, "elev_res.RData"))

#Plotly test

x_sel <- res_out$base_elev
y_sel <- res_out$tma_slo

swe_df <- data.frame(x_sel = as.numeric(x_sel),
                     y_sel = as.numeric(y_sel),
                     hu = res_out$hu,
                     id = res_out$id,
                     el = res_out$base_elev)

hu_cols <- c("navyblue", "royalblue3", "steelblue2", "grey25", "slategray2", "cadetblue3", 
             "turquoise4","yellow2", "orange2", "sienna2", "tomato3", "plum2", 
             "orchid4", "violetred2", "red2", "darkred", "limegreen", "seagreen2")

ply_swe <- plot_ly(swe_df,
                   y = ~y_sel,
                   x = ~x_sel,
                   type = 'scatter',
                   mode = "markers",
                   hovertemplate = paste(
                     "HU:", hu, '<br>',
                     "ID", id, '<br>',
                     "Elevaion:", el, '<br>',
                     "y-axis: %{y:.2f}",'<br>',
                     "x-axis: %{x:.0f}",
                     "<extra></extra>"
                       ),
                       color = ~hu,
                       colors = hu_cols,
                       opacity = 0.5,
                       marker = list(size = 15),
                       # symbol = ~forcing,
                       # symbols = c("circle", rep(c('circle', 'square', 'x-dot', 'triangle-up'), 5)),
                       height = '580'
)

title_font <- list(
  size = 18,
  color = "white"
)

tick_font <- list(
  size = 14,
  color = "white",
  ticks = "inside"
)

y_axis <- list(
  title = "Y-axis [ ]",
  showticklabels = TRUE,
  exponentformat = "none",
  tickangle = 270,
  titlefont = title_font,
  tickfont = tick_font,
  showline = F,
  zeroline = F,
  showgrid = T
)

x_axis <- list(
  title = "X-axis [ ]",
  showticklabels = TRUE,
  exponentformat = "none",
  titlefont = title_font,
  tickfont = tick_font,
  showline = F,
  zeroline = F,
  showgrid = T
)

legend_font <- list(
  font = list(
    size = 14,
    color = "white"))

vline <- function(x = 0, color = "white") {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color)
  )
}

ply_swe <- ply_swe %>%
  layout(title = '',
         yaxis = y_axis,
         xaxis = x_axis,
         legend = legend_font,
         plot_bgcolor='transparent',
         paper_bgcolor='transparent',
         margin = list(
           r = 350,
           t = 10,
           b = 10,
           l = 200)
  )


ply_swe
  


