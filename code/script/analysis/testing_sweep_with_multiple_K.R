
  df <- read.csv(paste0("kesi/data/testing.csv"))
  dat <- list()
  years =12
  periods = 24
  dat$K = 4
  z =2
  
  dat$N_obs <- periods
  dat$N_overlap <- dat$N_obs
  
  dat$N_cases <- periods*years
  dat$N_est <- dat$N_cases-dat$N_obs
  dat$obs_ind = ((dat$N_cases-dat$N_obs)+1):dat$N_cases
  dat$est_ind = 1:dat$N_est
  dat$cases_ind  = ((dat$N_cases-dat$N_obs)+1):dat$N_cases
  
  dat$year = rep(1:years, each= periods)
  year_ind = rep(1:years, each= 360)
  
  ut_p = paste(rep(rep(1:periods, each = 15),years), year_ind, sep = "_")
  dat$p = rep(1:periods,years)
  dat$month = rep(rep(1:(periods/2),each = 2), years)
  
  dat$heat <- rep(NA, dat$N_cases)
  dat$wind <- rep(NA, dat$N_cases)
  dat$waves <- rep(NA, dat$N_cases)
  dat$moisture <- rep(NA, dat$N_cases)
  dat$heat <- rep(NA, dat$N_cases)
  dat$heat <- rep(NA, dat$N_cases)
  dat$kesi <- rep(NA, dat$N_cases)
  
  
  dat$y <- as.matrix(df[((dat$N_cases-dat$N_obs)+1):dat$N_cases, 1:4])
  y <- matrix(NA, ncol = dat$K, nrow =dat$N_cases)
  
  
  cnt = 1 
  for(i in unique(ut_p)){
    ind = which(ut_p == i)
    dat$heat[cnt] = mean(df$c1[ind])
    dat$wind[cnt] = mean(df$c2[ind])
    dat$moisture[cnt] =  mean(df$c3[ind])
    dat$waves[cnt] = mean(df$c4[ind])
    dat$kesi[cnt] = round(mean(df$caught[ind]),0)
    y[cnt, 1:4] =  apply(df[ind, 1:dat$K], 2, mean)
    cnt <- cnt +1
  }
  
  dat$y<- as.matrix(y[((dat$N_cases-dat$N_obs)+1):dat$N_cases, ])
  
  dat$gdp = dat$y[,1] # NOTE THIS IS ONLY FOR ONE K
  dat$zscore = rnorm(dat$N_cases, 0, 1) # Just make something fake
  dat$ramadan = rbinom(dat$N_cases, 1, .033)+1 # again just something fake for ramadan
  
  
  ################################################################################
  ################ GP PREP #######################################################
  
  
  
  # Add on additional elements required for KESI Estimation
  # Make matrix for DMAT
  L = 10
  Dmat = matrix(NA, L, L)
  for(i in 1:L){
    for(j in 1:L){
      Dmat[i, j] = abs(i-j)
    }  
  }
  dat$L = L
  dat$DmatX= Dmat
  dat$LP = length(unique(dat$p))
  dat$P1 = 1:length(unique(dat$p))
  
  # DMat for Months
  DmatM = matrix(NA, 12, 12)
  for(i in 1:nrow(DmatM)){
    for(j in 1:nrow(DmatM)){
      DmatM[i, j] = abs(i-j)
    }  
  }
  
  dat$DmatM <- DmatM
  dat$NM <- 12
  dat$M1 <- 1:12
  
  
  # DMat for Periods
  DmatP = matrix(NA, 26, 26)
  for(i in 1:L){
    for(j in 1:L){
      DmatM[i, j] = abs(i-j)
    }  
  }
  
  
  ##########################################################
  ############ FOR BIO PHYSICAL ############################
  
  
  
  start_period = 1 
  imp_period <-c(start_period:periods, rep(1:periods, years))
  imp_month <- dat$month[imp_period]
  n_imp <- length(imp_period)
  K <- ncol(dat$y)
  
  dt <- data.frame(wind = dat$wind,
                   waves = dat$waves,
                   heat = dat$heat,
                   moisture = dat$moisture,
                   zscore = dat$zscore,
                   period = dat$p)
  
  
  wind_speed <-dt %>% group_by(period) %>% select(wind)  %>% summarize(mean = mean(wind))
  soil_moisture <-dt %>% group_by(period) %>% select(moisture)  %>% summarize(mean = mean(moisture))
  rain_zscore <-dt %>% group_by(period) %>% select(zscore)  %>% summarize(mean = mean(zscore))
  wave_height<-dt %>% group_by(period) %>% select(waves)  %>% summarize(mean = mean(waves))
  median_heat<-dt %>% group_by(period) %>% select(heat)  %>% summarize(mean = mean(heat))
  
  
  
  # Get Projections
  # Ensure that we cannot have negative values for wind, waves and moisture
  #wind
  wind_pro_high = ((wind_speed$mean-(mean(wind_speed$mean)))/sd(wind_speed$mean)*z)*sd(wind_speed$mean)+mean(wind_speed$mean)
  wind_pro_high <- ifelse(wind_pro_high < 0, 0, wind_pro_high)
  
  #waves
  wave_pro_high = ((wave_height$mean-(mean(wave_height$mean)))/sd(wave_height$mean)*z)*sd(wave_height$mean)+mean(wave_height$mean)
  wave_pro_high <- ifelse(wave_pro_high < 0, 0, wave_pro_high)
  
  #moisture
  moisture_pro_high = ((soil_moisture$mean-(mean(soil_moisture$mean)))/sd(soil_moisture$mean)*z)*sd(soil_moisture$mean)+mean(soil_moisture$mean)
  moisture_pro_high <- ifelse(moisture_pro_high < 0, 0, moisture_pro_high)
  
  # Note that this is already in Zscore
  zscore_pro_high = ((rain_zscore$mean-(mean(rain_zscore$mean)))/sd(rain_zscore$mean)*z)
  
  #heat
  heat_pro_high  = ((median_heat$mean-(mean(median_heat$mean)))/sd(median_heat$mean)*z)*sd(median_heat$mean)+mean(median_heat$mean)
  heat_pro_high <- ifelse(heat_pro_high < 0, 0, heat_pro_high)
  
  
  # Use Projections to define Matrixes for GP estimation
  #wind
  wind_breaks <-seq( min(wind_pro_high)-2.5, max(wind_pro_high)+2.5, length.out = 17)
  wind_means <- rep(NA, 16)
  for(i in 1:16){
    wind_means[i] = mean(wind_breaks[i], wind_breaks[i+1])
  }
  wind_cat <- cut(dat$wind, breaks = wind_breaks, right = FALSE)
  
  #waves
  wave_breaks <- seq( min(wave_pro_high)-2.5, max(wave_pro_high)+2.5, length.out = 17)
  wave_means <- rep(NA, 16)
  for(i in 1:16){
    wave_means[i] = mean(wave_breaks[i], wave_breaks[i+1])
  }
  wave_cat <- cut(dat$waves, breaks = wave_breaks, right = FALSE)
  
  # moisture
  moisture_breaks <- seq( min(moisture_pro_high)-2.5, max(moisture_pro_high)+2.5, length.out = 17)
  moisture_means <- rep(NA, 16)
  for(i in 1:16){
    moisture_means[i] = mean(moisture_breaks[i], moisture_breaks[i+1])
  }
  moisture_cat <- cut(dat$moisture, breaks = moisture_breaks, right = FALSE)
  
  #Zscore
  zscore_breaks <- seq( min(zscore_pro_high)-2.5, max(zscore_pro_high)+2.5, length.out = 17)
  zscore_means <- rep(NA, 16)
  for(i in 1:16){
    zscore_means[i] = mean(zscore_breaks[i], zscore_breaks[i+1])
  }
  zscore_cat <- cut(dat$zscore, breaks = zscore_breaks, right = FALSE)
  
  #heat
  heat_breaks <- seq( min(heat_pro_high)-2.5, max(heat_pro_high)+2.5, length.out = 17)
  heat_means <- rep(NA, 16)
  for(i in 1:16){
    heat_means[i] = mean(heat_breaks[i], heat_breaks[i+1])
  }
  heat_cat <- cut(dat$heat, breaks = heat_breaks, right = FALSE)
  
  
  # Give the correct Z for the estimate
  wind_cat_pro_high<- cut(wind_pro_high, breaks = wind_breaks, right = FALSE)
  wave_cat_pro_high<- cut(wave_pro_high, breaks = wave_breaks, right = FALSE)
  zscore_cat_pro_high<- cut(zscore_pro_high, breaks = zscore_breaks, right = FALSE)
  moisture_cat_pro_high<- cut(moisture_pro_high, breaks = moisture_breaks, right = FALSE)
  heat_cat_pro_high<- cut(heat_pro_high, breaks = heat_breaks, right = FALSE)
  # dat frame of projected bio-physical dat at chosen z-score
  
  df_high <- data.frame(heat = heat_pro_high[imp_month],
                        heat_cat =  heat_cat_pro_high[imp_month],
                        wind = wind_pro_high[imp_month],
                        wind_cat = wind_cat_pro_high[imp_month],
                        wave = wave_pro_high[imp_month],
                        wave_cat = wave_cat_pro_high[imp_month],
                        moisture = moisture_pro_high[imp_month],
                        moisture_cat = moisture_cat_pro_high[imp_month],
                        zscore = zscore_pro_high[imp_month],
                        zscore_cat = zscore_cat_pro_high[imp_month])
  
  
  # dat frame of observed bio-phsical THIS IS WRONG!
  df_obs = data.frame(heat = dat$heat[imp_month],
                      heat_cat =  heat_cat[imp_month],
                      wind = dat$wind[imp_month],
                      wind_cat = wind_cat[imp_month],
                      wave = dat$waves[imp_month],
                      wave_cat = wave_cat[imp_month],
                      moisture = dat$moisture[imp_month],
                      moisture_cat = moisture_cat[imp_month],
                      zscore = dat$zscore[imp_month],
                      zscore_cat = zscore_cat[imp_month])
  
  ##############################
  ###### BUISNESS AS USUAL #####
  
  # Give the correctr z score for the buisness as ususal condition 
  # Ensure that we cannot have negative values for wind, waves and moisture
  #wind
  wind_pro_bau = ((wind_speed$mean-(mean(wind_speed$mean)))/sd(wind_speed$mean))*sd(wind_speed$mean)+mean(wind_speed$mean)
  wind_pro_bau <- ifelse(wind_pro_bau < 0, 0, wind_pro_bau)
  
  #waves
  wave_pro_bau = ((wave_height$mean-(mean(wave_height$mean)))/sd(wave_height$mean))*sd(wave_height$mean)+mean(wave_height$mean)
  wave_pro_bau <- ifelse(wave_pro_bau < 0, 0, wave_pro_bau)
  
  #moisture
  moisture_pro_bau = ((soil_moisture$mean-(mean(soil_moisture$mean)))/sd(soil_moisture$mean))*sd(soil_moisture$mean)+mean(soil_moisture$mean)
  moisture_pro_bau <- ifelse(moisture_pro_bau < 0, 0, moisture_pro_bau)
  
  # Note that this is already in Zscore
  zscore_pro_bau = ((rain_zscore$mean-(mean(rain_zscore$mean)))/sd(rain_zscore$mean))
  
  #heat
  heat_pro_bau  = ((median_heat$mean-(mean(median_heat$mean)))/sd(median_heat$mean))*sd(median_heat$mean)+mean(median_heat$mean)
  heat_pro_bau <- ifelse(heat_pro_bau < 0, 0, heat_pro_bau)
  
  
  wind_cat_pro_bau<- cut(wind_pro_bau, breaks = wind_breaks, right = FALSE)
  wave_cat_pro_bau<- cut(wave_pro_bau, breaks = wave_breaks, right = FALSE)
  zscore_cat_pro_bau<- cut(zscore_pro_bau, breaks = zscore_breaks, right = FALSE)
  moisture_cat_pro_bau<- cut(moisture_pro_bau, breaks = moisture_breaks, right = FALSE)
  heat_cat_pro_bau<- cut(heat_pro_bau, breaks = heat_breaks, right = FALSE)
  
  # Match the correct date
  
  # dat frame of projected value of biophysical dat at buisness as ususal
  
  df_bau <- data.frame(heat = heat_pro_bau[imp_month],
                       heat_cat = heat_cat_pro_bau[imp_month],
                       wind = wind_pro_bau[imp_month],
                       wind_cat = wind_cat_pro_bau[imp_month],
                       wave = wave_pro_bau[imp_month],
                       wave_cat = wave_cat_pro_bau[imp_month],
                       moisture = moisture_pro_bau[imp_month],
                       moisture_cat = moisture_cat_pro_bau[imp_month],
                       zscore = zscore_pro_bau[imp_month],
                       zscore_cat = zscore_cat_pro_bau[imp_month])
  
  ####################################
  ####### GAUSSSIAN PROCESS PREP #####
  
  # Wind
  
  ind_wind<-as.integer(wind_cat)
  means <- wind_means
  NP2 <- length(means)
  
  DmatWind <- matrix(NA, NP2, NP2)
  for(i in 1:NP2){
    for(j in 1:NP2){
      DmatWind[i,j] <- abs(means[i]-means[j])
    }
  }
  
  dat$DmatWind <- DmatWind
  dat$wind_ind <- ind_wind
  
  
  # waves
  
  ind_waves<-as.integer(wave_cat)
  means <- wave_means
  NP2 <- length(means)
  
  DmatWaves <- matrix(NA, NP2, NP2)
  for(i in 1:NP2){
    for(j in 1:NP2){
      DmatWaves[i,j] <- abs(means[i]-means[j])
    }
  }
  
  dat$DmatWaves <- DmatWaves
  dat$waves_ind <- ind_waves
  
  # Moisture
  
  ind_moisture<-as.integer(moisture_cat)
  means <- moisture_means
  NP2 <- length(means)
  
  
  
  DmatMoisture <- matrix(NA, NP2, NP2)
  for(i in 1:NP2){
    for(j in 1:NP2){
      DmatMoisture[i,j] <- abs(means[i]-means[j])
    }
  }
  
  dat$DmatMoisture <- DmatMoisture
  dat$moisture_ind <- ind_moisture
  
  # Zscore
  
  
  ind_zscore<-as.integer(zscore_cat)
  means <- zscore_means
  NP2 <- length(means)
  
  
  DmatZscore <- matrix(NA, NP2, NP2)
  for(i in 1:NP2){
    for(j in 1:NP2){
      DmatZscore[i,j] <- abs(means[i]-means[j])
    }
  }
  dat$DmatZscore <- DmatZscore
  dat$zscore_ind <- ind_zscore
  
  # Heat 
  
  
  ind_heat<-as.integer(heat_cat)
  means <- heat_means
  NP2 <- length(means)
  
  
  
  Dmatheat <- matrix(NA, NP2, NP2)
  for(i in 1:NP2){
    for(j in 1:NP2){
      Dmatheat[i,j] <- abs(means[i]-means[j])
    }
  }
  dat$DmatHeat <- Dmatheat
  dat$heat_ind <- ind_heat
  
  
  dat$NP = NP2
  dat$y = dat$y
  dat$y2 <- y
  
  dat2 <- list(y2 = as.matrix(df[,1:4]),
               kesi = df$caught,
               N_cases = length(df$caught),
               K = dat$K)
  
  woagg <-stan("kesi/code/stan_models/known.stan", data = dat2, init =0, cores = 4, chains = 4, iter = 500)
  post <- extract.samples(woagg)
  par_woagg <- post$b 
  
  mknown<-stan("kesi/code/stan_models/known.stan", data = dat, init =0, cores = 4, chains = 4, iter = 500)
  # mknown_log<-stan("code/stan_models/known_log.stan", data = dat, init =0, cores = 4, chains = 4, iter = 1000)
  post <- extract.samples(mknown)
  par_known <- post$b 
  # post <- extract.samples(mknown_log)
  # par_known_log <- post$b 
  
  #############################################################################
  ############### CHECK #######################################################
  # THe problem comes in setting smart priors for phi
  recover=stan("kesi/code/stan_models/test_recover.stan", data = dat, init =0, cores = 4, chains = 4, iter = 500)
  # recover_log=stan("code/stan_models/recover_par_log.stan", data = dat, init =0, cores = 4, chains = 4, iter = 500)
  post <- extract.samples(recover)
  precis(recover,2, pars = "bgdp")
  