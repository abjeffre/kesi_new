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
lag2_rain <-dpp %>% group_by(period) %>% select(LagCummRain2)  %>% summarize(mean = mean(LagCummRain2))/100
lag3_rain <-dpp %>% group_by(period) %>% select(LagCummRain3)  %>% summarize(mean = mean(LagCummRain3))/100
lag4_rain <-dpp %>% group_by(period) %>% select(LagCummRain4)  %>% summarize(mean = mean(LagCummRain4))/100
lag5_rain <-dpp %>% group_by(period) %>% select(LagCummRain5)  %>% summarize(mean = mean(LagCummRain5))/100
lag5_rain <-dpp %>% group_by(period) %>% select(LagCummRain5)  %>% summarize(mean = mean(LagCummRain5))/100
lag6_rain <-dpp %>% group_by(period) %>% select(LagCummRain6)  %>% summarize(mean = mean(LagCummRain6))/100
lag2_heat <-dpp %>% group_by(period) %>% select(LagAvgHeat2)  %>% summarize(mean = mean(LagAvgHeat2))
lag3_heat <-dpp %>% group_by(period) %>% select(LagAvgHeat3)  %>% summarize(mean = mean(LagAvgHeat3))
lag4_heat <-dpp %>% group_by(period) %>% select(LagAvgHeat4)  %>% summarize(mean = mean(LagAvgHeat4))
lag5_heat <-dpp %>% group_by(period) %>% select(LagAvgHeat5)  %>% summarize(mean = mean(LagAvgHeat5))
lag6_heat <-dpp %>% group_by(period) %>% select(LagAvgHeat6)  %>% summarize(mean = mean(LagAvgHeat6))


rain_zscore <-dpp %>% group_by(period) %>% select(LagCummRain3)  %>% summarize(mean = mean(LagCummRain3))
moisture <- dpp %>% group_by(period) %>% select(cummRain)  %>% summarize(mean = mean(cummRain))/100
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
moisture_pro_high = ((moisture$mean-(mean(moisture$mean)))/sd(moisture$mean)*z)*sd(moisture$mean)+mean(moisture$mean)
moisture_pro_high <- ifelse(moisture_pro_high < 0, 0, moisture_pro_high)

#lag
lag2_rain_pro_high = ((lag2_rain$mean-(mean(lag2_rain$mean)))/sd(lag2_rain$mean)*z)*sd(lag2_rain$mean)+mean(lag2_rain$mean)
lag2_rain_pro_high <- ifelse(lag2_rain_pro_high < 0, 0, lag2_rain_pro_high)

lag3_rain_pro_high = ((lag3_rain$mean-(mean(lag3_rain$mean)))/sd(lag3_rain$mean)*z)*sd(lag3_rain$mean)+mean(lag3_rain$mean)
lag3_rain_pro_high <- ifelse(lag3_rain_pro_high < 0, 0, lag3_rain_pro_high)

lag4_rain_pro_high = ((lag4_rain$mean-(mean(lag4_rain$mean)))/sd(lag4_rain$mean)*z)*sd(lag4_rain$mean)+mean(lag4_rain$mean)
lag4_rain_pro_high <- ifelse(lag4_rain_pro_high < 0, 0, lag4_rain_pro_high)

lag5_rain_pro_high = ((lag5_rain$mean-(mean(lag5_rain$mean)))/sd(lag5_rain$mean)*z)*sd(lag5_rain$mean)+mean(lag5_rain$mean)
lag5_rain_pro_high <- ifelse(lag5_rain_pro_high < 0, 0, lag5_rain_pro_high)

lag6_rain_pro_high = ((lag6_rain$mean-(mean(lag6_rain$mean)))/sd(lag6_rain$mean)*z)*sd(lag6_rain$mean)+mean(lag6_rain$mean)
lag6_rain_pro_high <- ifelse(lag6_rain_pro_high < 0, 0, lag6_rain_pro_high)


# Note that this is already in Zscore
zscore_pro_high = ((rain_zscore$mean-(mean(rain_zscore$mean)))/sd(rain_zscore$mean)*z)

#heat
heat_pro_high  = ((median_heat$mean-(mean(median_heat$mean)))/sd(median_heat$mean)*z)*sd(median_heat$mean)+mean(median_heat$mean)
heat_pro_high <- ifelse(heat_pro_high < 0, 0, heat_pro_high)

#Lag heat
lag2_heat_pro_high = ((lag2_heat$mean-(mean(lag2_heat$mean)))/sd(lag2_heat$mean)*z)*sd(lag2_heat$mean)+mean(lag2_heat$mean)
lag2_heat_pro_high <- ifelse(lag2_heat_pro_high < 0, 0, lag2_heat_pro_high)

lag3_heat_pro_high = ((lag3_heat$mean-(mean(lag3_heat$mean)))/sd(lag3_heat$mean)*z)*sd(lag3_heat$mean)+mean(lag3_heat$mean)
lag3_heat_pro_high <- ifelse(lag3_heat_pro_high < 0, 0, lag3_heat_pro_high)

lag4_heat_pro_high = ((lag4_heat$mean-(mean(lag4_heat$mean)))/sd(lag4_heat$mean)*z)*sd(lag4_heat$mean)+mean(lag4_heat$mean)
lag4_heat_pro_high <- ifelse(lag4_heat_pro_high < 0, 0, lag4_heat_pro_high)

lag5_heat_pro_high = ((lag5_heat$mean-(mean(lag5_heat$mean)))/sd(lag5_heat$mean)*z)*sd(lag5_heat$mean)+mean(lag5_heat$mean)
lag5_heat_pro_high <- ifelse(lag5_heat_pro_high < 0, 0, lag5_heat_pro_high)

lag6_heat_pro_high = ((lag6_heat$mean-(mean(lag6_heat$mean)))/sd(lag6_heat$mean)*z)*sd(lag6_heat$mean)+mean(lag6_heat$mean)
lag6_heat_pro_high <- ifelse(lag6_heat_pro_high < 0, 0, lag6_heat_pro_high)


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


# lag2_rain
lag2_rain_breaks <- seq( min(lag2_rain_pro_high)-.1, max(lag2_rain_pro_high)+2, length.out = 17)
lag2_rain_means <- rep(NA, 16)
for(i in 1:16){
  lag2_rain_means[i] = mean(lag2_rain_breaks[i], lag2_rain_breaks[i+1])
}
lag2_rain_cat <- cut(data$lag2_rain, breaks = lag2_rain_breaks, right = FALSE)

# lag3_rain
lag3_rain_breaks <- seq( min(lag3_rain_pro_high)-.1, max(lag3_rain_pro_high)+2, length.out = 17)
lag3_rain_means <- rep(NA, 16)
for(i in 1:16){
  lag3_rain_means[i] = mean(lag3_rain_breaks[i], lag3_rain_breaks[i+1])
}
lag3_rain_cat <- cut(data$lag3_rain, breaks = lag3_rain_breaks, right = FALSE)

# lag4_rain
lag4_rain_breaks <- seq( min(lag4_rain_pro_high)-.1, max(lag4_rain_pro_high)+2, length.out = 17)
lag4_rain_means <- rep(NA, 16)
for(i in 1:16){
  lag4_rain_means[i] = mean(lag4_rain_breaks[i], lag4_rain_breaks[i+1])
}
lag4_rain_cat <- cut(data$lag4_rain, breaks = lag4_rain_breaks, right = FALSE)

# lag5_rain
lag5_rain_breaks <- seq( min(lag5_rain_pro_high)-.1, max(lag5_rain_pro_high)+2, length.out = 17)
lag5_rain_means <- rep(NA, 16)
for(i in 1:16){
  lag5_rain_means[i] = mean(lag5_rain_breaks[i], lag5_rain_breaks[i+1])
}
lag5_rain_cat <- cut(data$lag5_rain, breaks = lag5_rain_breaks, right = FALSE)

# lag6_rain
lag6_rain_breaks <- seq( min(lag6_rain_pro_high)-.1, max(lag6_rain_pro_high)+4.4, length.out = 17)
lag6_rain_means <- rep(NA, 16)
for(i in 1:16){
  lag6_rain_means[i] = mean(lag6_rain_breaks[i], lag6_rain_breaks[i+1])
}
lag6_rain_cat <- cut(data$lag6_rain, breaks = lag6_rain_breaks, right = FALSE)



#Zscore
zscore_breaks <- seq( min(zscore_pro_high)-.5, max(zscore_pro_high)+.1, length.out = 17)
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

# lag2_heat
lag2_heat_breaks <- seq( min(lag2_heat_pro_high)-.1, max(lag2_heat_pro_high)+.1, length.out = 17)
lag2_heat_means <- rep(NA, 16)
for(i in 1:16){
  lag2_heat_means[i] = mean(lag2_heat_breaks[i], lag2_heat_breaks[i+1])
}
lag2_heat_cat <- cut(data$lag2_heat, breaks = lag2_heat_breaks, right = FALSE)

# lag3_heat
lag3_heat_breaks <- seq( min(lag3_heat_pro_high)-.1, max(lag3_heat_pro_high)+.1, length.out = 17)
lag3_heat_means <- rep(NA, 16)
for(i in 1:16){
  lag3_heat_means[i] = mean(lag3_heat_breaks[i], lag3_heat_breaks[i+1])
}
lag3_heat_cat <- cut(data$lag3_heat, breaks = lag3_heat_breaks, right = FALSE)

# lag4_heat
lag4_heat_breaks <- seq( min(lag4_heat_pro_high)-.1, max(lag4_heat_pro_high)+.1, length.out = 17)
lag4_heat_means <- rep(NA, 16)
for(i in 1:16){
  lag4_heat_means[i] = mean(lag4_heat_breaks[i], lag4_heat_breaks[i+1])
}
lag4_heat_cat <- cut(data$lag4_heat, breaks = lag4_heat_breaks, right = FALSE)

# lag5_heat
lag5_heat_breaks <- seq( min(lag5_heat_pro_high)-.1, max(lag5_heat_pro_high)+.1, length.out = 17)
lag5_heat_means <- rep(NA, 16)
for(i in 1:16){
  lag5_heat_means[i] = mean(lag5_heat_breaks[i], lag5_heat_breaks[i+1])
}
lag5_heat_cat <- cut(data$lag5_heat, breaks = lag5_heat_breaks, right = FALSE)

# lag6_heat
lag6_heat_breaks <- seq( min(lag6_heat_pro_high)-.1, max(lag6_heat_pro_high)+.1, length.out = 17)
lag6_heat_means <- rep(NA, 16)
for(i in 1:16){
  lag6_heat_means[i] = mean(lag6_heat_breaks[i], lag6_heat_breaks[i+1])
}
lag6_heat_cat <- cut(data$lag6_heat, breaks = lag6_heat_breaks, right = FALSE)



# Give the correct Z for the estimate
wind_cat_pro_high<- cut(wind_pro_high, breaks = wind_breaks, right = FALSE)
wave_cat_pro_high<- cut(wave_pro_high, breaks = wave_breaks, right = FALSE)
zscore_cat_pro_high<- cut(zscore_pro_high, breaks = zscore_breaks, right = FALSE)
moisture_cat_pro_high<- cut(moisture_pro_high, breaks = moisture_breaks, right = FALSE)
lag2_rain_cat_pro_high<- cut(lag2_rain_pro_high, breaks = lag2_rain_breaks, right = FALSE)
lag3_rain_cat_pro_high<- cut(lag3_rain_pro_high, breaks = lag3_rain_breaks, right = FALSE)
lag4_rain_cat_pro_high<- cut(lag4_rain_pro_high, breaks = lag4_rain_breaks, right = FALSE)
lag5_rain_cat_pro_high<- cut(lag5_rain_pro_high, breaks = lag5_rain_breaks, right = FALSE)
lag6_rain_cat_pro_high<- cut(lag6_rain_pro_high, breaks = lag6_rain_breaks, right = FALSE)
lag2_heat_cat_pro_high<- cut(lag2_heat_pro_high, breaks = lag2_heat_breaks, right = FALSE)
lag3_heat_cat_pro_high<- cut(lag3_heat_pro_high, breaks = lag3_heat_breaks, right = FALSE)
lag4_heat_cat_pro_high<- cut(lag4_heat_pro_high, breaks = lag4_heat_breaks, right = FALSE)
lag5_heat_cat_pro_high<- cut(lag5_heat_pro_high, breaks = lag5_heat_breaks, right = FALSE)
lag6_heat_cat_pro_high<- cut(lag6_heat_pro_high, breaks = lag6_heat_breaks, right = FALSE)
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
                     lag2_rain = lag2_rain_pro_high[imp_month],
                     lag2_rain_cat = lag2_rain_cat_pro_high[imp_month],
                     lag3_rain = lag3_rain_pro_high[imp_month],
                     lag3_rain_cat = lag3_rain_cat_pro_high[imp_month],
                     lag4_rain = lag4_rain_pro_high[imp_month],
                     lag4_rain_cat = lag4_rain_cat_pro_high[imp_month],
                     lag5_rain = lag5_rain_pro_high[imp_month],
                     lag5_rain_cat = lag5_rain_cat_pro_high[imp_month],
                     lag6_rain = lag6_rain_pro_high[imp_month],
                     lag6_rain_cat = lag6_rain_cat_pro_high[imp_month],
                     lag2_heat = lag2_heat_pro_high[imp_month],
                     lag2_heat_cat = lag2_heat_cat_pro_high[imp_month],
                     lag3_heat = lag3_heat_pro_high[imp_month],
                     lag3_heat_cat = lag3_heat_cat_pro_high[imp_month],
                     lag4_heat = lag4_heat_pro_high[imp_month],
                     lag4_heat_cat = lag4_heat_cat_pro_high[imp_month],
                     lag5_heat = lag5_heat_pro_high[imp_month],
                     lag5_heat_cat = lag5_heat_cat_pro_high[imp_month],
                     lag6_heat = lag6_heat_pro_high[imp_month],
                     lag6_heat_cat = lag6_heat_cat_pro_high[imp_month],
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
                    lag2_rain = data$lag2_rain[imp_month],
                    lag2_rain_cat = lag2_rain_cat[imp_month],
                    lag3_rain = data$lag3_rain[imp_month],
                    lag3_rain_cat = lag3_rain_cat[imp_month],
                    lag4_rain = data$lag4_rain[imp_month],
                    lag4_rain_cat = lag4_rain_cat[imp_month],
                    lag5_rain = data$lag5_rain[imp_month],
                    lag5_rain_cat = lag5_rain_cat[imp_month],
                    lag6_rain = data$lag6_rain[imp_month],
                    lag6_rain_cat = lag6_rain_cat[imp_month],
                    lag2_heat = data$lag2_heat[imp_month],
                    lag2_heat_cat = lag2_heat_cat[imp_month],
                    lag3_heat = data$lag3_heat[imp_month],
                    lag3_heat_cat = lag3_heat_cat[imp_month],
                    lag4_heat = data$lag4_heat[imp_month],
                    lag4_heat_cat = lag4_heat_cat[imp_month],
                    lag5_heat = data$lag5_heat[imp_month],
                    lag5_heat_cat = lag5_heat_cat[imp_month],
                    lag6_heat = data$lag6_heat[imp_month],
                    lag6_heat_cat = lag6_heat_cat[imp_month],
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
moisture_pro_bau = ((moisture$mean-(mean(moisture$mean)))/sd(moisture$mean))*sd(moisture$mean)+mean(moisture$mean)
moisture_pro_bau <- ifelse(moisture_pro_bau < 0, 0, moisture_pro_bau)

#lag2_rain
lag2_rain_pro_bau = ((lag2_rain$mean-(mean(lag2_rain$mean)))/sd(lag2_rain$mean))*sd(lag2_rain$mean)+mean(lag2_rain$mean)
lag2_rain_pro_bau <- ifelse(lag2_rain_pro_bau < 0, 0, lag2_rain_pro_bau)

#lag3_rain
lag3_rain_pro_bau = ((lag3_rain$mean-(mean(lag3_rain$mean)))/sd(lag3_rain$mean))*sd(lag3_rain$mean)+mean(lag3_rain$mean)
lag3_rain_pro_bau <- ifelse(lag3_rain_pro_bau < 0, 0, lag3_rain_pro_bau)

#lag4_rain
lag4_rain_pro_bau = ((lag4_rain$mean-(mean(lag4_rain$mean)))/sd(lag4_rain$mean))*sd(lag4_rain$mean)+mean(lag4_rain$mean)
lag4_rain_pro_bau <- ifelse(lag4_rain_pro_bau < 0, 0, lag4_rain_pro_bau)

#lag5_rain
lag5_rain_pro_bau = ((lag5_rain$mean-(mean(lag5_rain$mean)))/sd(lag5_rain$mean))*sd(lag5_rain$mean)+mean(lag5_rain$mean)
lag5_rain_pro_bau <- ifelse(lag5_rain_pro_bau < 0, 0, lag5_rain_pro_bau)

#lag6_rain
lag6_rain_pro_bau = ((lag6_rain$mean-(mean(lag6_rain$mean)))/sd(lag6_rain$mean))*sd(lag6_rain$mean)+mean(lag6_rain$mean)
lag6_rain_pro_bau <- ifelse(lag6_rain_pro_bau < 0, 0, lag6_rain_pro_bau)


#lag2_heat
lag2_heat_pro_bau = ((lag2_heat$mean-(mean(lag2_heat$mean)))/sd(lag2_heat$mean))*sd(lag2_heat$mean)+mean(lag2_heat$mean)
lag2_heat_pro_bau <- ifelse(lag2_heat_pro_bau < 0, 0, lag2_heat_pro_bau)

#lag3_heat
lag3_heat_pro_bau = ((lag3_heat$mean-(mean(lag3_heat$mean)))/sd(lag3_heat$mean))*sd(lag3_heat$mean)+mean(lag3_heat$mean)
lag3_heat_pro_bau <- ifelse(lag3_heat_pro_bau < 0, 0, lag3_heat_pro_bau)

#lag4_heat
lag4_heat_pro_bau = ((lag4_heat$mean-(mean(lag4_heat$mean)))/sd(lag4_heat$mean))*sd(lag4_heat$mean)+mean(lag4_heat$mean)
lag4_heat_pro_bau <- ifelse(lag4_heat_pro_bau < 0, 0, lag4_heat_pro_bau)

#lag5_heat
lag5_heat_pro_bau = ((lag5_heat$mean-(mean(lag5_heat$mean)))/sd(lag5_heat$mean))*sd(lag5_heat$mean)+mean(lag5_heat$mean)
lag5_heat_pro_bau <- ifelse(lag5_heat_pro_bau < 0, 0, lag5_heat_pro_bau)

#lag6_heat
lag6_heat_pro_bau = ((lag6_heat$mean-(mean(lag6_heat$mean)))/sd(lag6_heat$mean))*sd(lag6_heat$mean)+mean(lag6_heat$mean)
lag6_heat_pro_bau <- ifelse(lag6_heat_pro_bau < 0, 0, lag6_heat_pro_bau)

# Note that this is already in Zscore
zscore_pro_bau = rep(0, max(imp_period))

#heat
heat_pro_bau  = ((median_heat$mean-(mean(median_heat$mean)))/sd(median_heat$mean))*sd(median_heat$mean)+mean(median_heat$mean)
heat_pro_bau <- ifelse(heat_pro_bau < 0, 0, heat_pro_bau)


wind_cat_pro_bau<- cut(wind_pro_bau, breaks = wind_breaks, right = FALSE)
wave_cat_pro_bau<- cut(wave_pro_bau, breaks = wave_breaks, right = FALSE)
zscore_cat_pro_bau<- cut(zscore_pro_bau, breaks = zscore_breaks, right = FALSE)
moisture_cat_pro_bau<- cut(moisture_pro_bau, breaks = moisture_breaks, right = FALSE)
heat_cat_pro_bau<- cut(heat_pro_bau, breaks = heat_breaks, right = FALSE)
lag2_rain_cat_pro_bau<- cut(lag2_rain_pro_bau, breaks = lag2_rain_breaks, right = FALSE)
lag3_rain_cat_pro_bau<- cut(lag3_rain_pro_bau, breaks = lag3_rain_breaks, right = FALSE)
lag4_rain_cat_pro_bau<- cut(lag4_rain_pro_bau, breaks = lag4_rain_breaks, right = FALSE)
lag5_rain_cat_pro_bau<- cut(lag5_rain_pro_bau, breaks = lag5_rain_breaks, right = FALSE)
lag6_rain_cat_pro_bau<- cut(lag6_rain_pro_bau, breaks = lag6_rain_breaks, right = FALSE)
lag2_heat_cat_pro_bau<- cut(lag2_heat_pro_bau, breaks = lag2_heat_breaks, right = FALSE)
lag3_heat_cat_pro_bau<- cut(lag3_heat_pro_bau, breaks = lag3_heat_breaks, right = FALSE)
lag4_heat_cat_pro_bau<- cut(lag4_heat_pro_bau, breaks = lag4_heat_breaks, right = FALSE)
lag5_heat_cat_pro_bau<- cut(lag5_heat_pro_bau, breaks = lag5_heat_breaks, right = FALSE)
lag6_heat_cat_pro_bau<- cut(lag6_heat_pro_bau, breaks = lag6_heat_breaks, right = FALSE)


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
                     lag2_rain = lag2_rain_pro_bau[imp_month],
                     lag2_rain_cat = lag2_rain_cat_pro_bau[imp_month],
                     lag3_rain = lag3_rain_pro_bau[imp_month],
                     lag3_rain_cat = lag3_rain_cat_pro_bau[imp_month],
                     lag4_rain = lag4_rain_pro_bau[imp_month],
                     lag4_rain_cat = lag4_rain_cat_pro_bau[imp_month],
                     lag5_rain = lag5_rain_pro_bau[imp_month],
                     lag5_rain_cat = lag5_rain_cat_pro_bau[imp_month],
                     lag6_rain = lag6_rain_pro_bau[imp_month],
                     lag6_rain_cat = lag6_rain_cat_pro_bau[imp_month],
                     lag2_heat = lag2_heat_pro_bau[imp_month],
                     lag2_heat_cat = lag2_heat_cat_pro_bau[imp_month],
                     lag3_heat = lag3_heat_pro_bau[imp_month],
                     lag3_heat_cat = lag3_heat_cat_pro_bau[imp_month],
                     lag4_heat = lag4_heat_pro_bau[imp_month],
                     lag4_heat_cat = lag4_heat_cat_pro_bau[imp_month],
                     lag5_heat = lag5_heat_pro_bau[imp_month],
                     lag5_heat_cat = lag5_heat_cat_pro_bau[imp_month],
                     lag6_heat = lag6_heat_pro_bau[imp_month],
                     lag6_heat_cat = lag6_heat_cat_pro_bau[imp_month],
                     
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
data$waves_ind <- ind_waves

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

data$DmatMoisture <- DmatMoisture
data$moisture_ind <- ind_moisture

# lag2_rain

ind_lag2_rain<-as.integer(lag2_rain_cat)
means <- lag2_rain_means
NP2 <- length(means)
DmatLagRain2 <- matrix(NA, NP2, NP2)
for(i in 1:NP2){
  for(j in 1:NP2){
    DmatLagRain2[i,j] <- abs(means[i]-means[j])
  }
}
data$DmatLagRain2 <- DmatLagRain2
data$lag2_rain_ind <- ind_lag2_rain

# lag3_rain

ind_lag3_rain<-as.integer(lag3_rain_cat)
means <- lag3_rain_means
NP2 <- length(means)
DmatLagRain3 <- matrix(NA, NP2, NP2)
for(i in 1:NP2){
  for(j in 1:NP2){
    DmatLagRain3[i,j] <- abs(means[i]-means[j])
  }
}
data$DmatLagRain3 <- DmatLagRain3
data$lag3_rain_ind <- ind_lag3_rain

# lag4_rain

ind_lag4_rain<-as.integer(lag4_rain_cat)
means <- lag4_rain_means
NP2 <- length(means)
DmatLagRain4 <- matrix(NA, NP2, NP2)
for(i in 1:NP2){
  for(j in 1:NP2){
    DmatLagRain4[i,j] <- abs(means[i]-means[j])
  }
}
data$DmatLagRain4 <- DmatLagRain4
data$lag4_rain_ind <- ind_lag4_rain

# lag5_rain

ind_lag5_rain<-as.integer(lag5_rain_cat)
means <- lag5_rain_means
NP2 <- length(means)
DmatLagRain5 <- matrix(NA, NP2, NP2)
for(i in 1:NP2){
  for(j in 1:NP2){
    DmatLagRain5[i,j] <- abs(means[i]-means[j])
  }
}
data$DmatLagRain5 <- DmatLagRain5
data$lag5_rain_ind <- ind_lag5_rain

# lag6_rain

ind_lag6_rain<-as.integer(lag6_rain_cat)
means <- lag6_rain_means
NP2 <- length(means)
DmatLagRain6 <- matrix(NA, NP2, NP2)
for(i in 1:NP2){
  for(j in 1:NP2){
    DmatLagRain6[i,j] <- abs(means[i]-means[j])
  }
}
data$DmatLagRain6 <- DmatLagRain6
data$lag6_rain_ind <- ind_lag6_rain


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
means <- heat_means
NP2 <- length(means)



DmatHeat <- matrix(NA, NP2, NP2)
for(i in 1:NP2){
  for(j in 1:NP2){
    DmatHeat[i,j] <- abs(means[i]-means[j])
  }
}
data$DmatHeat <- DmatHeat
data$heat_ind <- ind_heat


# lag2_heat

ind_lag2_heat<-as.integer(lag2_heat_cat)
means <- lag2_heat_means
NP2 <- length(means)
DmatLagHeat2 <- matrix(NA, NP2, NP2)
for(i in 1:NP2){
  for(j in 1:NP2){
    DmatLagHeat2[i,j] <- abs(means[i]-means[j])
  }
}
data$DmatLagHeat2 <- DmatLagHeat2
data$lag2_heat_ind <- ind_lag2_heat

# lag3_heat

ind_lag3_heat<-as.integer(lag3_heat_cat)
means <- lag3_heat_means
NP2 <- length(means)
DmatLagHeat3 <- matrix(NA, NP2, NP2)
for(i in 1:NP2){
  for(j in 1:NP2){
    DmatLagHeat3[i,j] <- abs(means[i]-means[j])
  }
}
data$DmatLagHeat3 <- DmatLagHeat3
data$lag3_heat_ind <- ind_lag3_heat

# lag4_heat

ind_lag4_heat<-as.integer(lag4_heat_cat)
means <- lag4_heat_means
NP2 <- length(means)
DmatLagHeat4 <- matrix(NA, NP2, NP2)
for(i in 1:NP2){
  for(j in 1:NP2){
    DmatLagHeat4[i,j] <- abs(means[i]-means[j])
  }
}
data$DmatLagHeat4 <- DmatLagHeat4
data$lag4_heat_ind <- ind_lag4_heat

# lag5_heat

ind_lag5_heat<-as.integer(lag5_heat_cat)
means <- lag5_heat_means
NP2 <- length(means)
DmatLagHeat5 <- matrix(NA, NP2, NP2)
for(i in 1:NP2){
  for(j in 1:NP2){
    DmatLagHeat5[i,j] <- abs(means[i]-means[j])
  }
}
data$DmatLagHeat5 <- DmatLagHeat5
data$lag5_heat_ind <- ind_lag5_heat

# lag6_heat

ind_lag6_heat<-as.integer(lag6_heat_cat)
means <- lag6_heat_means
NP2 <- length(means)
DmatLagHeat6 <- matrix(NA, NP2, NP2)
for(i in 1:NP2){
  for(j in 1:NP2){
    DmatLagHeat6[i,j] <- abs(means[i]-means[j])
  }
}
data$DmatLagHeat6 <- DmatLagHeat6
data$lag6_heat_ind <- ind_lag6_heat


M = 15
data$NP = NP2


DmatEnv=array(NA, dim =c(M, NP2, NP2))
DmatEnv[1,,] <- DmatWind
DmatEnv[2,,] <- DmatWaves
DmatEnv[3,,] <- DmatMoisture
DmatEnv[4,,] <- DmatLagRain2
DmatEnv[5,,] <- DmatLagRain3
DmatEnv[6,,] <- DmatLagRain4
DmatEnv[7,,] <- DmatLagRain5
DmatEnv[8,,] <- DmatLagRain6
DmatEnv[9,,] <- DmatZscore
DmatEnv[10,,] <- DmatHeat
DmatEnv[11,,] <- DmatLagHeat2
DmatEnv[12,,] <- DmatLagHeat3
DmatEnv[13,,] <- DmatLagHeat4
DmatEnv[14,,] <- DmatLagHeat5
DmatEnv[15,,] <- DmatLagHeat6

data$DmatEnv<-DmatEnv
data$M <- M
data$env_ind <- t(as.matrix(data.frame(ind_wind,
                                     ind_waves,
                                     ind_moisture,
                                     ind_lag2_rain,
                                     ind_lag3_rain,
                                     ind_lag4_rain,
                                     ind_lag5_rain,
                                     ind_lag6_rain,
                                     ind_zscore,
                                     ind_heat,
                                     ind_lag2_heat,
                                     ind_lag3_heat,
                                     ind_lag4_heat,
                                     ind_lag5_heat,
                                     ind_lag6_heat
                          
                                     )))


data$DM <- 2
DmatDEnv=array(NA, dim =c(data$DM, NP2, NP2))
DmatDEnv[1,,] <- DmatHeat
DmatDEnv[2,,] <- DmatMoisture
data$DmatDEnv<-DmatDEnv
data$denv_ind <- t(as.matrix(data.frame(ind_heat,
                                     ind_moisture
                                     )))
