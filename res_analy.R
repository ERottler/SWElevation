###

#Analysis snow model input + output 
#Erwin Rottler, University of Potsdam

###

load(paste0(daymet_dir, "processed/daymet_meteo_prep.RData")) #daymet meteo data for watershed (see daymet_get.R)
load(paste0(daymet_dir, "processed/swe_all.RData")) #simulated snow (see swe_simu.R)
load(paste0(daymet_dir, "processed/elevs_sel.RData")) #elevations from dem (see daymet_get.R)

#Cluster for parallel computing
my_clust <- parallel::makeCluster(n_cores)#Make cluster for parallel computing
clusterEvalQ(my_clust, pacman::p_load(zoo, zyp, rEchseSnow, meltimr))
registerDoParallel(my_clust)

#swe_band----

block_size <- 1000
block_stas <- c(1, seq(block_size+1, ncol(swe_all), by = block_size))
block_ends <- c(seq(block_size, ncol(swe_all), by = block_size), ncol(swe_all))

date_met <- as.Date(date_met, "%Y-%m-%d")

#SWE: mean average
for(b in 1:length(block_stas)){
  
  snows_calc <- swe_all[, block_stas[b]:block_ends[b]]
  
  print(paste(Sys.time(),"Average mean swe", "Block:", b, "out of", length(block_stas)))
  
  smea_ann_block <- foreach(i = 1:ncol(snows_calc), .combine = 'cbind') %dopar% {
    
    day_ana(snows_calc[, i], 
            date = date_met, 
            start_year = 1980, 
            end_year = 2019,
            break_day = 274,
            do_ma = F,
            window_width = 30, 
            method_ana = "mean") #[m]
    
  }
  
  if(b == 1){
    smea_ann <- smea_ann_block
  }else{
    smea_ann <- cbind(smea_ann, smea_ann_block)
  }
  
}

#SWE: trends 30DMA
for(b in 1:length(block_stas)){
  
  snows_calc <- swe_all[, block_stas[b]:block_ends[b]]
  
  print(paste(Sys.time(),"Trends 30DMA swe", "Block:", b, "out of", length(block_stas)))
  
  sslo_ann_block <- foreach(i = 1:ncol(snows_calc), .combine = 'cbind') %dopar% {
    
    day_ana(snows_calc[, i], 
            date = date_met, 
            start_year = 1980, 
            end_year = 2019,
            break_day = 274,
            do_ma = T,
            window_width = 30, 
            method_ana = "sens_slope") * 10 #[m/dec]
    
  }
  
  if(b == 1){
    sslo_ann <- sslo_ann_block
  }else{
    sslo_ann <- cbind(sslo_ann, sslo_ann_block)
  }
  
}

#Snow accumulation and melt water outflow
sv_diff <- function(snow_volume_in){
  
  sv_diff <- c(NA, diff(snow_volume_in))
  
  return(sv_diff)
  
}

swe_dif <- apply(swe_all, 2, sv_diff)

#SWE diff: mean average
for(b in 1:length(block_stas)){
  
  snows_calc <- swe_dif[, block_stas[b]:block_ends[b]]
  
  print(paste(Sys.time(),"Average mean diff swe", "Block:", b, "out of", length(block_stas)))
  
  sdif_ann_block <- foreach(i = 1:ncol(snows_calc), .combine = 'cbind') %dopar% {
    
    day_ana(snows_calc[, i], 
            date = date_met, 
            start_year = 1980, 
            end_year = 2019,
            break_day = 274,
            do_ma = F,
            window_width = 30, 
            method_ana = "mean") #[m]
    
  }
  
  if(b == 1){
    sdif_ann <- sdif_ann_block
  }else{
    sdif_ann <- cbind(sdif_ann, sdif_ann_block)
  }
  
}

#SWE diff: trends 30DMA
for(b in 1:length(block_stas)){
  
  snows_calc <- swe_dif[, block_stas[b]:block_ends[b]]
  
  print(paste(Sys.time(),"Trends 30 DMA diff swe", "Block:", b, "out of", length(block_stas)))
  
  sdis_ann_block <- foreach(i = 1:ncol(snows_calc), .combine = 'cbind') %dopar% {
    
    day_ana(snows_calc[, i], 
            date = date_met, 
            start_year = 1980, 
            end_year = 2019,
            break_day = 274,
            do_ma = T,
            window_width = 30, 
            method_ana = "sens_slope") * 10#[m/dec]
    
  }
  
  if(b == 1){
    sdis_ann <- sdis_ann_block
  }else{
    sdis_ann <- cbind(sdis_ann, sdis_ann_block)
  }
  
}

#Temperature: mean average
for(b in 1:length(block_stas)){
  
  temps_calc <- tmea_all[, block_stas[b]:block_ends[b]]
  
  print(paste(Sys.time(),"Average mean temperature", "Block:", b, "out of", length(block_stas)))
  
  tmea_ann_block <- foreach(i = 1:ncol(temps_calc), .combine = 'cbind') %dopar% {
    
    day_ana(temps_calc[, i], 
            date = date_met, 
            start_year = 1980, 
            end_year = 2019,
            break_day = 274,
            do_ma = T,
            window_width = 30, 
            method_ana = "mean") #[m]
    
  }
  
  if(b == 1){
    tmea_ann <- tmea_ann_block
  }else{
    tmea_ann <- cbind(tmea_ann, tmea_ann_block)
  }
  
}

#Temperature: trends 30DMA
for(b in 1:length(block_stas)){
  
  temps_calc <- tmea_all[, block_stas[b]:block_ends[b]]
  
  print(paste(Sys.time(),"Trends 30DMA temp", "Block:", b, "out of", length(block_stas)))
  
  tslo_ann_block <- foreach(i = 1:ncol(temps_calc), .combine = 'cbind') %dopar% {
    
    day_ana(temps_calc[, i], 
            date = date_met, 
            start_year = 1980, 
            end_year = 2019,
            break_day = 274,
            do_ma = T,
            window_width = 30, 
            method_ana = "sens_slope") * 10 #[m/dec]
    
  }
  
  if(b == 1){
    tslo_ann <- tslo_ann_block
  }else{
    tslo_ann <- cbind(tslo_ann, tslo_ann_block)
  }
  
}

#Precipitation: mean average
for(b in 1:length(block_stas)){
  
  precs_calc <- prcp_all[, block_stas[b]:block_ends[b]]
  
  print(paste(Sys.time(),"Average mean precipitation", "Block:", b, "out of", length(block_stas)))
  
  pmea_ann_block <- foreach(i = 1:ncol(precs_calc), .combine = 'cbind') %dopar% {
    
    day_ana(precs_calc[, i], 
            date = date_met, 
            start_year = 1980, 
            end_year = 2019,
            break_day = 274,
            do_ma = T,
            window_width = 30, 
            method_ana = "mean") #[m]
    
  }
  
  if(b == 1){
    pmea_ann <- pmea_ann_block
  }else{
    pmea_ann <- cbind(pmea_ann, pmea_ann_block)
  }
  
}

#Precipitation: trends 30DMA
for(b in 1:length(block_stas)){
  
  precs_calc <- prcp_all[, block_stas[b]:block_ends[b]]
  
  print(paste(Sys.time(),"Trends 30DMA prec", "Block:", b, "out of", length(block_stas)))
  
  pslo_ann_block <- foreach(i = 1:ncol(precs_calc), .combine = 'cbind') %dopar% {
    
    day_ana(precs_calc[, i], 
            date = date_met, 
            start_year = 1980, 
            end_year = 2019,
            break_day = 274,
            do_ma = T,
            window_width = 30, 
            method_ana = "sens_slope") * 10 #[m/dec]
    
  }
  
  if(b == 1){
    pslo_ann <- pslo_ann_block
  }else{
    pslo_ann <- cbind(pslo_ann, pslo_ann_block)
  }
  
}

#save_data----

save(smea_ann, sslo_ann,
     sdif_ann, sdis_ann,
     tmea_ann, tslo_ann,
     pmea_ann, pslo_ann,
     file = paste0(daymet_dir, "processed/res_analy.RData"))

stopCluster(my_clust)
