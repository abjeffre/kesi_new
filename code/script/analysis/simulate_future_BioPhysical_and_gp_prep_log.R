########################################################################
############ SIMULATE BIOPYSICAL DATA FOR FUTURE TIME POINTS ###########
# par(mfcol = c(3, 2))



#################################################
################ GENERATE SIMULATED DATA ########



start_period = data$p[length(data$p)]+1 
imp_period <-c(5:26, rep(1:26, years))
imp_month <- data$month[imp_period]
n_imp <- length(imp_period)
K <- ncol(data$y)

wind_speed <-dpp %>% group_by(period) %>% select(MedianWindSpeed)  %>% summarize(mean = mean(MedianWindSpeed))
soil_moisture <-dpp %>% group_by(period) %>% select(cummRain)  %>% summarize(mean = mean(cummRain))/100
rain_zscore <-dpp %>% group_by(period) %>% select(LagCummRain)  %>% summarize(mean = mean(LagCummRain))
wave_height<-dpp %>% group_by(period) %>% select(MedianWaveHeight)  %>% summarize(mean = mean(MedianWaveHeight))
median_heat<-dpp %>% group_by(period) %>% select(MedianMaxTemp)  %>% summarize(mean = mean(MedianMaxTemp))



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
wind_breaks <-seq( min(wind_pro_high)-.1, max(wind_pro_high)+.1, length.out = 17)
wind_means <- rep(NA, 16)
for(i in 1:16){
  wind_means[i] = mean(wind_breaks[i], wind_breaks[i+1])
}
wind_cat <- cut(data$wind, breaks = wind_breaks, right = FALSE)

#waves
wave_breaks <- seq( min(wave_pro_high)-.1, max(wave_pro_high)+.2, length.out = 17)
wave_means <- rep(NA, 16)
for(i in 1:16){
  wave_means[i] = mean(wave_breaks[i], wave_breaks[i+1])
}
wave_cat <- cut(data$waves, breaks = wave_breaks, right = FALSE)

# moisture
moisture_breaks <- seq( min(moisture_pro_high)-.1, max(moisture_pro_high)+.1, length.out = 17)
moisture_means <- rep(NA, 16)
for(i in 1:16){
  moisture_means[i] = mean(moisture_breaks[i], moisture_breaks[i+1])
}
moisture_cat <- cut(data$moisture, breaks = moisture_breaks, right = FALSE)

#Zscore
zscore_breaks <- seq( min(zscore_pro_high)-.1, max(zscore_pro_high)+.1, length.out = 17)
zscore_means <- rep(NA, 16)
for(i in 1:16){
  zscore_means[i] = mean(zscore_breaks[i], zscore_breaks[i+1])
}
zscore_cat <- cut(data$zscore, breaks = zscore_breaks, right = FALSE)

#heat
heat_breaks <- seq( min(heat_pro_high)-.1, max(heat_pro_high)+.1, length.out = 17)
heat_means <- rep(NA, 16)
for(i in 1:16){
  heat_means[i] = mean(heat_breaks[i], heat_breaks[i+1])
}
heat_cat <- cut(data$heat, breaks = heat_breaks, right = FALSE)


# Give the correct Z for the estimate
wind_cat_pro_high<- cut(wind_pro_high, breaks = wind_breaks, right = FALSE)
wave_cat_pro_high<- cut(wave_pro_high, breaks = wave_breaks, right = FALSE)
zscore_cat_pro_high<- cut(zscore_pro_high, breaks = zscore_breaks, right = FALSE)
moisture_cat_pro_high<- cut(moisture_pro_high, breaks = moisture_breaks, right = FALSE)
heat_cat_pro_high<- cut(heat_pro_high, breaks = heat_breaks, right = FALSE)
# Data frame of projected bio-physical data at chosen z-score

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


# data frame of observed bio-phsical THIS IS WRONG!
df_obs = data.frame(heat = data$heat[imp_month],
                    heat_cat =  heat_cat[imp_month],
                    wind = data$wind[imp_month],
                    wind_cat = wind_cat[imp_month],
                    wave = data$waves[imp_month],
                    wave_cat = wave_cat[imp_month],
                    moisture = data$moisture[imp_month],
                    moisture_cat = moisture_cat[imp_month],
                    zscore = data$zscore[imp_month],
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

# Data frame of projected value of biophysical data at buisness as ususal

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
means <- log1p(wind_means)
NP2 <- length(means)

DmatWind <- matrix(NA, NP2, NP2)
for(i in 1:NP2){
  for(j in 1:NP2){
    DmatWind[i,j] <- abs(means[i]-means[j])
  }
}

data$DmatWind <- DmatWind
data$wind_ind <- ind_wind


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

data$DmatWaves <- DmatWaves
data$waves_ind <- log1p(ind_waves)

# Moisture

ind_moisture<-as.integer(moisture_cat)
means <- log1p(moisture_means)
NP2 <- length(means)



DmatMoisture <- matrix(NA, NP2, NP2)
for(i in 1:NP2){
  for(j in 1:NP2){
    DmatMoisture[i,j] <- abs(means[i]-means[j])
  }
}

data$DmatMoisture <- DmatMoisture
data$moisture_ind <- ind_moisture

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
data$DmatZscore <- DmatZscore
data$zscore_ind <- ind_zscore

# Heat 


ind_heat<-as.integer(heat_cat)
means <- log1p(heat_means)
NP2 <- length(means)



Dmatheat <- matrix(NA, NP2, NP2)
for(i in 1:NP2){
  for(j in 1:NP2){
    Dmatheat[i,j] <- abs(means[i]-means[j])
  }
}
data$DmatHeat <- Dmatheat
data$heat_ind <- ind_heat


data$NP = NP2
