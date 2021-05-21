
###

#Prepare analysis results for dashboard
#Erwin Rottler, University of Potsdam

###

pacman::p_load(meltimr, viridis, zoo, scales)

#select GRDC station
id_sel <- "4116301" # Orofino, Clearwater River
# id_sel <- "4116300" # Spalding, Clearwater River

daymet_dir <- paste0("D:/nrc_user/rottler/daymet/", id_sel, "/") #Spalding, Clearwater River

load(paste0(daymet_dir, "processed/swe_all.RData")) #simulated snow (see swe_simu.R)
load(paste0(daymet_dir, "processed/res_analy.RData")) #results analysis (see res_analy.R)
load(paste0(daymet_dir, "processed/elevs_sel.RData")) #elevations from dem (see daymet_get.R)

#calc_bands----

range(elevs_sel, na.rm = T)
my_elev_bands <- seq(300, 2700, 50)

f_elev_bands <- function(data_in, elev_bands = my_elev_bands, func_aggr = "mean", 
                         meta_dat = elevs_sel){
  
  for(i in 1:(length(elev_bands) - 1)){
    # print(i)
    data_points_range <- which(meta_dat > elev_bands[i] & meta_dat < elev_bands[i+1])
    
    if(length(data_points_range) == 1){
      data_range_sing <- data_in[, data_points_range]
    }else{
      if(func_aggr == "mean"){
        data_range_sing <- apply(data_in[, data_points_range], 1, mea_na)
      }
      if(func_aggr == "medi"){
        data_range_sing <- apply(data_in[, data_points_range], 1, med_na)
      }
      if(func_aggr == "sum"){
        data_range_sing <- apply(data_in[, data_points_range], 1, sum_na)
      }
    }
    
    
    if(i == 1){
      
      data_range <- data_range_sing
      
    }else{
      
      data_range <- cbind(data_range, data_range_sing)
      
    }
    
  }
  
  return(data_range)
  
}

smea_ann_band <- f_elev_bands(data_in = smea_ann, func_aggr = "mean")
vmea_ann_band <- f_elev_bands(data_in = smea_ann, func_aggr = "sum")
sslo_ann_band <- f_elev_bands(data_in = sslo_ann, func_aggr = "mean")
vslo_ann_band <- f_elev_bands(data_in = sslo_ann, func_aggr = "sum")
vdif_ann_band <- f_elev_bands(data_in = sdif_ann, func_aggr = "sum")
vdis_ann_band <- f_elev_bands(data_in = sdis_ann, func_aggr = "sum")
tmea_ann_band <- f_elev_bands(data_in = tmea_ann, func_aggr = "mean")
tslo_ann_band <- f_elev_bands(data_in = tslo_ann, func_aggr = "mean")
pmea_ann_band <- f_elev_bands(data_in = pmea_ann, func_aggr = "mean")
pslo_ann_band <- f_elev_bands(data_in = pslo_ann, func_aggr = "mean")


#save_data----

save(my_elev_bands,
     smea_ann_band, vmea_ann_band,
     sslo_ann_band, vslo_ann_band,
     vdif_ann_band, vdis_ann_band,
     tmea_ann_band, tslo_ann_band,
     pmea_ann_band, pslo_ann_band,
     file = paste0(daymet_dir, "processed/res_dashb.RData"))


#clean_up----

rm(list = ls())

.rs.restartR()

#plot_snow----

plot_snow <- vmea_ann_band 
col_zero <- F #color range center at zero

x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15

my_col <- colorRampPalette(c("white", viridis(9, direction = 1)[c(3,4)], "cadetblue3", "grey80",
                             "yellow2","gold", "orange2", "orangered2"))(200)
my_bre <- seq(min_na(plot_snow), max_na(plot_snow), length.out = length(my_col)+1)

if(col_zero){
  
  n_max <- round(abs(max_na(plot_snow[, ])) / (max_na(plot_snow[, ]) + abs(min_na(plot_snow[, ]))), digits = 2) * 200
  n_min <- 200 - n_max
  
  cols_min <- colorRampPalette(c("darkred", "firebrick4", "firebrick4", "orange3", "darkgoldenrod3", "grey98"))(100)
  cols_max <- colorRampPalette(c("grey98", viridis::viridis(9, direction = 1)[4:1]))(100)
  my_col <- alpha(c(cols_min, cols_max), alpha = 1.0)
  my_bre <- seq(-max_na(abs(plot_snow)), max_na(abs(plot_snow)), length.out = length(my_col)+1)
  
}

par(mar = c(1.6, 3, 0.6, 0))

layout(matrix(c(1,1,1,1,1,1,1,2),
              1, 8), widths=c(), heights=c())

image(x = 1:365,
      y = my_elev_bands[-length(my_elev_bands)],
      z = plot_snow, col =my_col, breaks = my_bre,
      ylab = "", xlab = "", axes = F)
axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
     col = "black", col.axis = "black", tck = -0.06)#plot ticks
axis(1, at = x_axis_lab, c("O","N","D","J","F","M","A","M","J","J","A","S"), tick = FALSE,
     col="black", col.axis="black", mgp=c(3, 0.15, 0))#plot labels
mtext("Elevation [m]", side = 2, line = 1.5, cex = 0.8)
axis(2, mgp=c(3, 0.15, 0), tck = -0.001)
box()

par(mar = c(1.6, 0.5, 0.6, 2.7))

image_scale(as.matrix(plot_snow), col = my_col, breaks = my_bre, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
mtext("Snow", side = 4, line = 1.5, cex = 0.8)

box()


#concurrent----

ssum_band <- f_elev_bands(data_in = swe_all,  func_aggr = "sum")

#calculate timing and magnitude annual 14-day snowmelt maxima
yea_min_mag_bands_14 <- NULL
yea_min_doy_bands_14 <- NULL
for(i in 1:ncol(ssum_band)){
  
  swe_band <- ssum_band[, i]
  
  swe_band_dif <- c(NA, diff(swe_band))
  
  swe_band_dif[which(swe_band_dif > 0)] <- 0
  
  #Moving average filter
  swe_band_dif_ma_14 <- rollapply(data = swe_band_dif, width = 14,
                                  FUN = sum_na, align = "center", fill = NA)
  
  #Order data by day
  data_day_14 <- ord_day(data_in = swe_band_dif_ma_14,
                         date = date_met,
                         start_y = 1980,
                         end_y = 2019,
                         break_day = 274,
                         do_ma = F)
  
  min_doy <- function(data_in){
    
    if(length(which(is.na(data_in))) == length(data_in)){
      
      doy_min <- NA  
      
    }else{
      
      doy_min <- as.numeric(which(data_in == min_na(data_in)))
      
      if(length(doy_min) > 1){
        doy_min <- mea_na(doy_min)
      }
      
    }
    
    
    
    
    return(doy_min)
    
  }
  
  yea_min_mag_14 <- apply(data_day_14, 1, min_na)
  yea_min_doy_14 <- apply(data_day_14, 1, min_doy)
  
  yea_min_mag_bands_14 <- cbind(yea_min_mag_bands_14, yea_min_mag_14)
  yea_min_doy_bands_14 <- cbind(yea_min_doy_bands_14, yea_min_doy_14)
  
}

#Concurrent melt
freq_bands_1 <- NULL
freq_bands_2 <- NULL
freq_diff_all <- NULL
length_bands_all <- NULL
width_over <- 14

for(b in 1:ncol(ssum_band)){
  
  band_sel <- b  
  
  bands_frequ_1 <- NULL
  bands_lengt_1 <- NULL
  for(y in 1:19){
    
    mid_day <- yea_min_doy_bands_14[y, band_sel]
    min_day <- mid_day - width_over
    max_day <- mid_day + width_over
    melt_window <- min_day:max_day
    
    bands_asso_1 <- which(yea_min_doy_bands_14[y, ] > min_day & yea_min_doy_bands_14[y, ] < max_day)
    
    bands_frequ_1 <- c(bands_frequ_1, bands_asso_1)
    bands_lengt_1 <- c(bands_lengt_1, length(bands_asso_1))
    
  }
  hist_1 <- hist(bands_frequ_1, breaks = seq(from = 0.5, to = ncol(ssum_band)+0.5, by = 1), plot = F)
  
  bands_frequ_2 <- NULL
  bands_lengt_2 <- NULL
  for(x in 20:38){
    
    # print(y)
    mid_day <- yea_min_doy_bands_14[x, band_sel]
    min_day <- mid_day - width_over
    max_day <- mid_day + width_over
    melt_window <- min_day:max_day
    
    bands_asso_2 <- which(yea_min_doy_bands_14[x, ] > min_day & yea_min_doy_bands_14[x, ] < max_day)
    
    bands_frequ_2 <- c(bands_frequ_2, bands_asso_2)
    bands_lengt_2 <- c(bands_lengt_2, length(bands_asso_2))
    
  }
  hist_2 <- hist(bands_frequ_2, breaks = seq(from = 0.5, to = ncol(ssum_band)+0.5, by = 1), plot = F)
  
  freq_diff <- hist_2$counts - hist_1$counts
  
  freq_bands_1 <- cbind(freq_bands_1, hist_1$counts)
  freq_bands_2 <- cbind(freq_bands_2, hist_2$counts)
  freq_diff_all <- cbind(freq_diff_all, freq_diff)
  length_bands_all <- cbind(length_bands_all, bands_lengt_1, bands_lengt_2)
  
}

band_test <- 10 #my_elev_bands[band_test]
col_1 <- viridis::viridis(9, direction = 1)[4] 
col_2 <- "darkred"

par(mar = c(2.5, 2.5, 2, 0.2))

plot(freq_bands_1[, band_test], type = "h", col = scales::alpha(col_1, alpha = 0.5), lwd = 7, ylab = "", xlab = "", 
     axes = F, lend = 2, xaxs = "i", yaxs = "i", ylim = c(0, 17), xlim = c(0, ncol(ssum_band)+0.5))
abline(v = band_test, col = "black", lwd = 1.5)
# legend("topleft", c("1985-2014", "1954-1984"), col = c(col_2, col_1), pch = 19)
par(new = T)
plot(freq_bands_2[, band_test], type = "h", col = alpha(col_2, alpha = 0.5), lwd = 7, ylab = "", xlab = "", 
     axes = F, lend = 2, xaxs = "i", yaxs = "i", ylim = c(0, 17), xlim = c(0, ncol(ssum_band)+0.5))
elevs_sel_plot <- c(6, 16, 26, 36, 46, 56)
axis(1, at = elevs_sel_plot, labels = my_elev_bands[elevs_sel_plot], mgp=c(3, 0.15, 0), tck = -0.01, cex.axis = 0.9)
axis(2, mgp=c(3, 0.15, 0), tck = -0.01, cex.axis = 0.9)
mtext("Elevation band [m]", side = 1, adj = 0.5, line = 1.3, cex = 1.1)
mtext("Frequency concurrent melt [-]", side = 2, adj = 0.5, line = 1.3, cex = 1.1)
mtext("Frequency concurrent melt (elevation band 1400-1450 m)", side = 3, adj = 0.0, line = 0.2, cex = 1.4)
graphics::box()


par(mar = c(2.5, 2.5, 2.0, 0.2))

plot(freq_diff_all[band_test, ], type = "n", axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", 
     xlim = c(0, ncol(ssum_band)+0.5), ylim = c(-9, 9))
abline(v = band_test, col = "black", lwd = 1.5)
lines(freq_diff_all[band_test, ], lwd = 2, col = "grey55")
polygon(x = c(0.999, 1:46) , y = c(0, freq_diff_all[band_test, ]), 
        col = alpha("grey55", alpha = 0.7), border = F)
elevs_sel_plot <- c(6, 16, 26, 36, 46, 56)
axis(1, at = elevs_sel_plot, labels = my_elev_bands[elevs_sel_plot], mgp=c(3, 0.15, 0), tck = -0.01, cex.axis = 0.9)
axis(2, mgp=c(3, 0.15, 0), tck = -0.01, cex.axis = 0.9)
abline(h = 0, lty = "dotted")
mtext("Elevation band [m]", side = 1, adj = 0.5, line = 1.3, cex = 1.1)
mtext("Change frequency concurrent melt [-]", side = 2, adj = 0.5, line = 1.3, cex = 1.1)
mtext("Changes in concurrent melt (elevation band 1400-1450 m)", side = 3, adj = 0.0, line = 0.2, cex = 1.4)
graphics::box()




cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "grey90"))(50)
cols_max <- colorRampPalette(c("grey90", "gold3",  "orange3", "orangered4", "orangered4", "darkred"))(50)
cols_mel <- c(cols_min, cols_max)

layout(matrix(c(rep(1, 7), 2),
              1, 8), widths=c(), heights=c())

par(mar = c(3.5, 3.5, 2.5, 0.2))

image(x = 1:nrow(freq_diff_all),
      y = 1:ncol(freq_diff_all),
      z = freq_diff_all, 
      col = cols_mel, breaks = seq(from = -12, to = 12, length.out = 101), axes = F, ylab = "", xlab = "")
elevs_sel_plot <- c(6, 16, 26, 36, 46, 56)
axis(1, at = elevs_sel_plot, labels = my_elev_bands[elevs_sel_plot], mgp=c(3, 0.55, 0), tck = -0.005, cex.axis = 1.4)
axis(2, at = elevs_sel_plot, labels = my_elev_bands[elevs_sel_plot], mgp=c(3, 0.25, 0), tck = -0.005, cex.axis = 1.4)
mtext("Elevation band [m]", side = 1, adj = 0.5, line = 2.1, cex = 1.1)
mtext("Elevation band [m]", side = 2, adj = 0.5, line = 2.1, cex = 1.1)
mtext("Changes in concurrent melt (all elevation bands)", side = 3, adj = 0.0, line = 0.2, cex = 1.4)
mtext("[-]", side = 3, adj = 1.0, line = 0.2, cex = 1.1)
graphics::box()

par(mar = c(3.5, 0.2, 2.5, 4.0))

alptempr::image_scale(as.matrix(freq_diff_all), col = cols_mel, breaks = seq(from = -12, to = 12, length.out = 101), horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, 0.35, 0), tck = -0.08, cex.axis = 1.4)
box()



