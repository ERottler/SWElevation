
###

#Prepare analysis results for dashboard
#Erwin Rottler, University of Potsdam

###

load(paste0(daymet_dir, "processed/swe_all.RData")) #simulated snow (see swe_simu.R)
load(paste0(daymet_dir, "processed/res_analy.RData")) #results analysis (see res_analy.R)
load(paste0(daymet_dir, "processed/elevs_sel.RData")) #elevations from dem (see daymet_get.R)
load(paste0(daymet_dir, "processed/daymet_meteo_prep.RData")) #daymet meteo data for watershed (see daymet_get.R)

#Cluster for parallel computing
my_clust <- parallel::makeCluster(n_cores)#Make cluster for parallel computing
clusterEvalQ(my_clust, pacman::p_load(zoo, zyp, meltimr))
registerDoParallel(my_clust)

#calc_bands----

#get first/last elevation band
elev_bands_all <- seq(0, 6000, 50)

elev_bands_all_dif_min <- elev_bands_all - min(elevs_sel, na.rm = T)
elev_bands_all_dif_max <- elev_bands_all - max(elevs_sel, na.rm = T)
elev_bands_all_dif_min[which(elev_bands_all_dif_min > 0)] <- NA
elev_bands_all_dif_max[which(elev_bands_all_dif_max < 0)] <- NA 

elev_band_min <- elev_bands_all[which(elev_bands_all_dif_min == max(elev_bands_all_dif_min, na.rm = T))]
elev_band_max <- elev_bands_all[which(elev_bands_all_dif_max == min(elev_bands_all_dif_max, na.rm = T))]

# range(elevs_sel, na.rm = T)
my_elev_bands <- seq(elev_band_min, elev_band_max, 50)

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

#scd_maps----

#date swe time series
date_snow <- as.POSIXct(strptime(date_met, "%Y-%m-%d", tz = "UTC"))

#Select validation period
sta_date <- as.POSIXct(strptime("1980-10-01", "%Y-%m-%d", tz = "UTC"))
end_date <- as.POSIXct(strptime("2019-09-30", "%Y-%m-%d", tz = "UTC"))
date_vali <- as.POSIXct(as.Date(seq(sta_date, end_date, by = "day")), tz = "UTC")

#clip validation period from simulations
snow_simu_sel <- which(date_snow %in% date_vali)
swe_all_vali <- swe_all[snow_simu_sel, ]
tem_all_vali <- tmea_all[snow_simu_sel, ]
prc_all_vali <- prcp_all[snow_simu_sel, ]

#calculate sum days with snow for validation period
f_snow2sc <- function(snow_in, snow_thresh = 0.02){
  
  snow_in[which(snow_in >= snow_thresh)] <- 1
  snow_in[which(snow_in <  snow_thresh)] <- 0
  snow_in[which(is.na(snow_in))] <- NA
  
  snow_days_sum <- sum(snow_in, na.rm = T)
  
  return(snow_days_sum)
  
}

scd_simu <- apply(swe_all_vali, 2, f_snow2sc) / (nrow(swe_all_vali)/365)
tem_simu <- apply(tem_all_vali, 2, mea_na)
prc_simu <- apply(prc_all_vali, 2, mea_na) * 365
swe_simu <- apply(swe_all_vali, 2, mea_na) * 100 # [mm]

val2col <- function(val_in, dat_ref, do_log = F, do_bicol = T, col_na = "white"){
  
  if(do_log){
    
    val_in <- log(val_in)
    dat_ref <- log(dat_ref)
    
  }
  
  if(is.na(val_in)){#set NAs to mean to keep script running; later back to NA
    val_in <- mea_na(dat_ref)
    set2NA_1 <- T
  }else{
    set2NA_1 <- F
  }
  
  if(do_bicol){
    
    col_ind <- round((abs(val_in) / max_na(abs(dat_ref))) * 100)
    
    if(val_in < 0){
      my_col  <- colorRampPalette(c("grey80", "lemonchiffon2", "lightgoldenrod2", "gold3", "goldenrod3", "orangered4", "darkred"))(100)
    }else{
      my_col  <- colorRampPalette(c("grey80", "lightcyan3", viridis::viridis(9, direction = 1)[c(4,3,2,1,1)]))(100)
    }
    
  }else{
    col_ind <- round((val_in-min_na(dat_ref)) / (max_na(dat_ref)-min_na(dat_ref)) * 200)  
    my_col <- c(colorRampPalette(c(viridis::viridis(20, direction = -1)))(200))
  }
  
  
  if(is.na(col_ind)){
    set2NA_2 <- T
    col_ind <- 1 #set to one to keep script running; later set to NA color
  }else{
    set2NA_2 = F
  }
  
  if(col_ind == 0){#for minimum and very small values
    
    col_ind <- 1
    
  }
  
  col_out <- my_col[col_ind]
  
  if(length(col_out) < 1){
    
    col_out <- col_na
    
  }
  
  if(set2NA_1 | set2NA_2){
    
    col_out <- col_na
    
  }
  
  return(col_out)
  
}

cols_scd_simu <- foreach(i = 1:length(scd_simu), .combine = 'cbind') %dopar% {
  
  val2col(val_in = scd_simu[i], 
          dat_ref = scd_simu,
          do_log = F,
          do_bicol = F)
  
}
cols_tem_simu <- foreach(i = 1:length(tem_simu), .combine = 'cbind') %dopar% {
  
  val2col(val_in = tem_simu[i], 
          dat_ref = tem_simu,
          do_log = F,
          do_bicol = F)
  
}
cols_prc_simu <- foreach(i = 1:length(prc_simu), .combine = 'cbind') %dopar% {
  
  val2col(val_in = prc_simu[i], 
          dat_ref = prc_simu,
          do_log = F,
          do_bicol = F)
  
}
cols_ele_simu <- foreach(i = 1:length(elevs_sel), .combine = 'cbind') %dopar% {
  
  val2col(val_in = elevs_sel[i], 
          dat_ref = elevs_sel,
          do_log = F,
          do_bicol = F)
  
}
cols_swe_simu <- foreach(i = 1:length(swe_simu), .combine = 'cbind') %dopar% {
  
  val2col(val_in = swe_simu[i], 
          dat_ref = swe_simu,
          do_log = F,
          do_bicol = F)
  
}

cols_sel <- cols_scd_simu
data_sel <- scd_simu

layout(matrix(c(rep(1, 7), 2),
              1, 8), widths=c(), heights=c())

par(mar = c(2.0, 0.5, 0.5, 0.5))
plot(basin_sel)
points(grid_points_sel@coords[, 1], grid_points_sel@coords[, 2], pch = 19, col = cols_sel, cex = 2.0)
plot(basin_sel, add = T)

par(mar = c(2.0, 0.2, 2.5, 3.5))
my_col <- c(colorRampPalette(c(viridis::viridis(20, direction = -1)))(200))
my_bre <- seq(min_na(data_sel), max_na(data_sel), length.out = length(my_col)+1)
alptempr::image_scale(as.matrix(data_sel), col = my_col, breaks = my_bre, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
axis(4, mgp=c(3, 0.45, 0), tck = -0.1, cex.axis = 1.5)
box()


#save_data----

save(my_elev_bands,
     smea_ann_band, vmea_ann_band,
     sslo_ann_band, vslo_ann_band,
     vdif_ann_band, vdis_ann_band,
     tmea_ann_band, tslo_ann_band,
     pmea_ann_band, pslo_ann_band,
     scd_simu, prc_simu, tem_simu, swe_simu,
     cols_scd_simu, cols_prc_simu, cols_tem_simu, cols_ele_simu, cols_swe_simu,
     grid_points_sel, basin_sel,
     file = paste0(daymet_dir, "processed/res_dashb.RData"))

stopCluster(my_clust)

# #plot_snow----
# 
# plot_snow <- smea_ann_band 
# col_zero <- F #color range center at zero
# 
# x_axis_lab <- c(16,46,74,105,135,166,196,227,258,288,319,349)
# x_axis_tic <- c(   46,74,105,135,166,196,227,258,288,319,349)-15
# 
# my_col <- colorRampPalette(c("white", viridis(9, direction = 1)[c(3,4)], "cadetblue3", "grey80",
#                              "yellow2","gold", "orange2", "orangered2"))(200)
# my_bre <- seq(min_na(plot_snow), max_na(plot_snow), length.out = length(my_col)+1)
# 
# if(col_zero){
#   
#   n_max <- round(abs(max_na(plot_snow[, ])) / (max_na(plot_snow[, ]) + abs(min_na(plot_snow[, ]))), digits = 2) * 200
#   n_min <- 200 - n_max
#   
#   cols_min <- colorRampPalette(c("darkred", "firebrick4", "firebrick4", "orange3", "darkgoldenrod3", "grey98"))(100)
#   cols_max <- colorRampPalette(c("grey98", viridis::viridis(9, direction = 1)[4:1]))(100)
#   my_col <- alpha(c(cols_min, cols_max), alpha = 1.0)
#   my_bre <- seq(-max_na(abs(plot_snow)), max_na(abs(plot_snow)), length.out = length(my_col)+1)
#   
# }
# 
# par(mar = c(1.6, 3, 0.6, 0))
# 
# layout(matrix(c(1,1,1,1,1,1,1,2),
#               1, 8), widths=c(), heights=c())
# 
# image(x = 1:365,
#       y = my_elev_bands[-length(my_elev_bands)],
#       z = plot_snow, col =my_col, breaks = my_bre,
#       ylab = "", xlab = "", axes = F)
# axis(1, at = x_axis_tic, c("","","","","","","","","","",""), tick = TRUE,
#      col = "black", col.axis = "black", tck = -0.06)#plot ticks
# axis(1, at = x_axis_lab, c("O","N","D","J","F","M","A","M","J","J","A","S"), tick = FALSE,
#      col="black", col.axis="black", mgp=c(3, 0.15, 0))#plot labels
# mtext("Elevation [m]", side = 2, line = 1.5, cex = 0.8)
# axis(2, mgp=c(3, 0.15, 0), tck = -0.001)
# box()
# 
# par(mar = c(1.6, 0.5, 0.6, 2.7))
# 
# image_scale(as.matrix(plot_snow), col = my_col, breaks = my_bre, horiz=F, ylab="", xlab="", yaxt="n", axes=F)
# axis(4, mgp=c(3, 0.15, 0), tck = -0.08)
# mtext("Snow", side = 4, line = 1.5, cex = 0.8)
# 
# box()
# 
# 
# #concurrent----
# 
# ssum_band <- f_elev_bands(data_in = swe_all,  func_aggr = "sum")
# 
# #calculate timing and magnitude annual 14-day snowmelt maxima
# yea_min_mag_bands_14 <- NULL
# yea_min_doy_bands_14 <- NULL
# for(i in 1:ncol(ssum_band)){
#   
#   swe_band <- ssum_band[, i]
#   
#   swe_band_dif <- c(NA, diff(swe_band))
#   
#   swe_band_dif[which(swe_band_dif > 0)] <- 0
#   
#   #Moving average filter
#   swe_band_dif_ma_14 <- rollapply(data = swe_band_dif, width = 14,
#                                   FUN = sum_na, align = "center", fill = NA)
#   
#   #Order data by day
#   data_day_14 <- ord_day(data_in = swe_band_dif_ma_14,
#                          date = date_met,
#                          start_y = 1980,
#                          end_y = 2019,
#                          break_day = 274,
#                          do_ma = F)
#   
#   min_doy <- function(data_in){
#     
#     if(length(which(is.na(data_in))) == length(data_in)){
#       
#       doy_min <- NA  
#       
#     }else{
#       
#       doy_min <- as.numeric(which(data_in == min_na(data_in)))
#       
#       if(length(doy_min) > 1){
#         doy_min <- mea_na(doy_min)
#       }
#       
#     }
#     
#     
#     
#     
#     return(doy_min)
#     
#   }
#   
#   yea_min_mag_14 <- apply(data_day_14, 1, min_na)
#   yea_min_doy_14 <- apply(data_day_14, 1, min_doy)
#   
#   yea_min_mag_bands_14 <- cbind(yea_min_mag_bands_14, yea_min_mag_14)
#   yea_min_doy_bands_14 <- cbind(yea_min_doy_bands_14, yea_min_doy_14)
#   
# }
# 
# #Concurrent melt
# freq_bands_1 <- NULL
# freq_bands_2 <- NULL
# freq_diff_all <- NULL
# length_bands_all <- NULL
# width_over <- 14
# 
# for(b in 1:ncol(ssum_band)){
#   
#   band_sel <- b  
#   
#   bands_frequ_1 <- NULL
#   bands_lengt_1 <- NULL
#   for(y in 1:19){
#     
#     mid_day <- yea_min_doy_bands_14[y, band_sel]
#     min_day <- mid_day - width_over
#     max_day <- mid_day + width_over
#     melt_window <- min_day:max_day
#     
#     bands_asso_1 <- which(yea_min_doy_bands_14[y, ] > min_day & yea_min_doy_bands_14[y, ] < max_day)
#     
#     bands_frequ_1 <- c(bands_frequ_1, bands_asso_1)
#     bands_lengt_1 <- c(bands_lengt_1, length(bands_asso_1))
#     
#   }
#   hist_1 <- hist(bands_frequ_1, breaks = seq(from = 0.5, to = ncol(ssum_band)+0.5, by = 1), plot = F)
#   
#   bands_frequ_2 <- NULL
#   bands_lengt_2 <- NULL
#   for(x in 20:38){
#     
#     # print(y)
#     mid_day <- yea_min_doy_bands_14[x, band_sel]
#     min_day <- mid_day - width_over
#     max_day <- mid_day + width_over
#     melt_window <- min_day:max_day
#     
#     bands_asso_2 <- which(yea_min_doy_bands_14[x, ] > min_day & yea_min_doy_bands_14[x, ] < max_day)
#     
#     bands_frequ_2 <- c(bands_frequ_2, bands_asso_2)
#     bands_lengt_2 <- c(bands_lengt_2, length(bands_asso_2))
#     
#   }
#   hist_2 <- hist(bands_frequ_2, breaks = seq(from = 0.5, to = ncol(ssum_band)+0.5, by = 1), plot = F)
#   
#   freq_diff <- hist_2$counts - hist_1$counts
#   
#   freq_bands_1 <- cbind(freq_bands_1, hist_1$counts)
#   freq_bands_2 <- cbind(freq_bands_2, hist_2$counts)
#   freq_diff_all <- cbind(freq_diff_all, freq_diff)
#   length_bands_all <- cbind(length_bands_all, bands_lengt_1, bands_lengt_2)
#   
# }
# 
# band_test <- 10 #my_elev_bands[band_test]
# col_1 <- viridis::viridis(9, direction = 1)[4] 
# col_2 <- "darkred"
# 
# par(mar = c(2.5, 2.5, 2, 0.2))
# 
# plot(freq_bands_1[, band_test], type = "h", col = scales::alpha(col_1, alpha = 0.5), lwd = 7, ylab = "", xlab = "", 
#      axes = F, lend = 2, xaxs = "i", yaxs = "i", ylim = c(0, 17), xlim = c(0, ncol(ssum_band)+0.5))
# abline(v = band_test, col = "black", lwd = 1.5)
# # legend("topleft", c("1985-2014", "1954-1984"), col = c(col_2, col_1), pch = 19)
# par(new = T)
# plot(freq_bands_2[, band_test], type = "h", col = alpha(col_2, alpha = 0.5), lwd = 7, ylab = "", xlab = "", 
#      axes = F, lend = 2, xaxs = "i", yaxs = "i", ylim = c(0, 17), xlim = c(0, ncol(ssum_band)+0.5))
# elevs_sel_plot <- c(6, 16, 26, 36, 46, 56)
# axis(1, at = elevs_sel_plot, labels = my_elev_bands[elevs_sel_plot], mgp=c(3, 0.15, 0), tck = -0.01, cex.axis = 0.9)
# axis(2, mgp=c(3, 0.15, 0), tck = -0.01, cex.axis = 0.9)
# mtext("Elevation band [m]", side = 1, adj = 0.5, line = 1.3, cex = 1.1)
# mtext("Frequency concurrent melt [-]", side = 2, adj = 0.5, line = 1.3, cex = 1.1)
# mtext("Frequency concurrent melt (elevation band 1400-1450 m)", side = 3, adj = 0.0, line = 0.2, cex = 1.4)
# graphics::box()
# 
# 
# par(mar = c(2.5, 2.5, 2.0, 0.2))
# 
# plot(freq_diff_all[band_test, ], type = "n", axes = F, ylab = "", xlab = "", xaxs = "i", yaxs = "i", 
#      xlim = c(0, ncol(ssum_band)+0.5), ylim = c(-9, 9))
# abline(v = band_test, col = "black", lwd = 1.5)
# lines(freq_diff_all[band_test, ], lwd = 2, col = "grey55")
# polygon(x = c(0.999, 1:46) , y = c(0, freq_diff_all[band_test, ]), 
#         col = alpha("grey55", alpha = 0.7), border = F)
# elevs_sel_plot <- c(6, 16, 26, 36, 46, 56)
# axis(1, at = elevs_sel_plot, labels = my_elev_bands[elevs_sel_plot], mgp=c(3, 0.15, 0), tck = -0.01, cex.axis = 0.9)
# axis(2, mgp=c(3, 0.15, 0), tck = -0.01, cex.axis = 0.9)
# abline(h = 0, lty = "dotted")
# mtext("Elevation band [m]", side = 1, adj = 0.5, line = 1.3, cex = 1.1)
# mtext("Change frequency concurrent melt [-]", side = 2, adj = 0.5, line = 1.3, cex = 1.1)
# mtext("Changes in concurrent melt (elevation band 1400-1450 m)", side = 3, adj = 0.0, line = 0.2, cex = 1.4)
# graphics::box()
# 
# 
# 
# 
# cols_min <- colorRampPalette(c(viridis::viridis(9, direction = 1)[1:4], "cadetblue3", "grey90"))(50)
# cols_max <- colorRampPalette(c("grey90", "gold3",  "orange3", "orangered4", "orangered4", "darkred"))(50)
# cols_mel <- c(cols_min, cols_max)
# 
# layout(matrix(c(rep(1, 7), 2),
#               1, 8), widths=c(), heights=c())
# 
# par(mar = c(3.5, 3.5, 2.5, 0.2))
# 
# image(x = 1:nrow(freq_diff_all),
#       y = 1:ncol(freq_diff_all),
#       z = freq_diff_all, 
#       col = cols_mel, breaks = seq(from = -12, to = 12, length.out = 101), axes = F, ylab = "", xlab = "")
# elevs_sel_plot <- c(6, 16, 26, 36, 46, 56)
# axis(1, at = elevs_sel_plot, labels = my_elev_bands[elevs_sel_plot], mgp=c(3, 0.55, 0), tck = -0.005, cex.axis = 1.4)
# axis(2, at = elevs_sel_plot, labels = my_elev_bands[elevs_sel_plot], mgp=c(3, 0.25, 0), tck = -0.005, cex.axis = 1.4)
# mtext("Elevation band [m]", side = 1, adj = 0.5, line = 2.1, cex = 1.1)
# mtext("Elevation band [m]", side = 2, adj = 0.5, line = 2.1, cex = 1.1)
# mtext("Changes in concurrent melt (all elevation bands)", side = 3, adj = 0.0, line = 0.2, cex = 1.4)
# mtext("[-]", side = 3, adj = 1.0, line = 0.2, cex = 1.1)
# graphics::box()
# 
# par(mar = c(3.5, 0.2, 2.5, 4.0))
# 
# alptempr::image_scale(as.matrix(freq_diff_all), col = cols_mel, breaks = seq(from = -12, to = 12, length.out = 101), horiz=F, ylab="", xlab="", yaxt="n", axes=F)
# axis(4, mgp=c(3, 0.35, 0), tck = -0.08, cex.axis = 1.4)
# box()
# 


