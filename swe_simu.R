###

#Snow simulations + snow/meteo analysis using US Daymet data 
#Erwin Rottler, University of Potsdam

###

#load prepared Daymet meteo data (see daymet_get.R)
load(paste0(daymet_dir, "processed/daymet_meteo_prep.RData"))

#default snow parameter
snow_params <- read.table("U:/SWElelevation/R/SWElevation/snow_param.txt", header = T, sep = ";")

#Cluster for parallel computing
my_clust <- makeCluster(n_cores)
clusterEvalQ(my_clust, pacman::p_load(zoo, zyp, rEchseSnow, meltimr))
registerDoParallel(my_clust)

#snow_simu----

block_size <- 1000
block_stas <- c(1, seq(block_size+1, ncol(tmea_all), by = block_size))
block_ends <- c(seq(block_size, ncol(tmea_all), by = block_size), ncol(tmea_all))

for(b in 1:length(block_stas)){
  
  print(paste(Sys.time(),"Snow simulations", "Block:", b, "out of", length(block_stas)))
  
  temps_simu <- tmea_all[, block_stas[b]:block_ends[b]]
  precs_simu <- prcp_all[, block_stas[b]:block_ends[b]]
  srads_simu <- srad_all[, block_stas[b]:block_ends[b]]
  
  snows <- foreach(k = 1:ncol(temps_simu), .combine = 'cbind') %dopar% {
    
    swe_sim   <- rep(NA, nrow(temps_simu))
    sec_sim   <- rep(NA, nrow(temps_simu))
    alb_sim   <- rep(NA, nrow(temps_simu))
    
    swe_init <- .0
    sec_init <- .0
    alb_init <- snow_params$albedoMax
    
    swe_sim[1] <- swe_init
    sec_sim[1] <- sec_init
    alb_sim[1] <- alb_init
    
    for(i in 2:nrow(temps_simu)){
      
      sim_out <- snowModel_inter(
        #Forcings
        precipSumMM = precs_simu[i, k],
        shortRad = srads_simu[i, k],
        tempAir = temps_simu[i, k],
        pressAir = 1000,
        relHumid = 70,
        windSpeed = 1,
        cloudCoverage = 0.5,
        #Parameters
        precipSeconds = snow_params$precipSeconds,
        a0 = snow_params$a0,
        a1 = snow_params$a1,
        kSatSnow = snow_params$kSatSnow,
        densDrySnow = snow_params$densDrySnow,
        specCapRet = snow_params$specCapRet,
        emissivitySnowMin = snow_params$emissivitySnowMin,
        emissivitySnowMax = snow_params$emissivitySnowMax,
        tempAir_crit = snow_params$tempAir_crit,
        albedoMin = snow_params$albedoMin,
        albedoMax = snow_params$albedoMax,
        agingRate_tAirPos = snow_params$agingRate_tAirPos,
        agingRate_tAirNeg = snow_params$agingRate_tAirNeg,
        soilDepth = snow_params$soilDepth,
        soilDens = snow_params$soilDens,
        soilSpecHeat = snow_params$soilSpecHeat,
        weightAirTemp = snow_params$weightAirTemp,
        tempMaxOff = snow_params$tempMaxOff,
        tempAmpli = snow_params$tempAmpli,
        #States
        snowEnergyCont = sec_sim[i-1],
        snowWaterEquiv = swe_sim[i-1],
        albedo = alb_sim[i-1],
        #Outputs
        TEMP_MEAN = NA,
        TEMP_SURF = NA,
        LIQU_FRAC = NA,
        flux_R_netS = NA,
        flux_R_netL = NA,
        flux_R_soil = NA,
        flux_R_sens = NA,
        stoi_f_prec = NA,
        stoi_f_subl = NA,
        stoi_f_flow = NA,
        flux_M_prec = NA,
        flux_M_subl = NA,
        flux_M_flow = NA,
        rate_G_alb = NA
      )
      
      sec_sim[i] <- sim_out[1]
      swe_sim[i] <- sim_out[2]
      alb_sim[i] <- sim_out[3]
      
    }
    
    # swe_sim
    cbind(swe_sim, sec_sim)
    
  }
  
  if(b == 1){
    swe_all <- snows[, seq(1, length(block_stas[b]:block_ends[b])*2, 2)]
    sec_all <- snows[, seq(2, length(block_stas[b]:block_ends[b])*2, 2)]
  }else{
    swe_all <- cbind(swe_all, snows[, seq(1, length(block_stas[b]:block_ends[b])*2, 2)])
    sec_all <- cbind(sec_all, snows[, seq(2, length(block_stas[b]:block_ends[b])*2, 2)])
  }
  
}  

#save_data----

save(swe_all, file = paste0(daymet_dir, "processed/swe_all.RData"))

stopCluster(my_clust)


