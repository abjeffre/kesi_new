###############################
####### GAUSSSIAN PROCESS #####

# Wind
NP = 10 # Number of percentiles 

ind_wind<-as.integer(cut(data$wind, quantile(data$wind, prob = 0:NP / NP, names = FALSE), include = TRUE))
means <- sapply(split(data$wind, cut(data$wind, quantile(data$wind, prob = 0:NP / NP, names = FALSE), include = TRUE)), mean)
# Make means for posterior simulations
means = c(1, 2, 3, means, 9, 10, 11)
NP2 <- length(means)
ind_wind = ind_wind+3

DmatWind <- matrix(NA, NP2, NP2)
for(i in 1:NP2){
  for(j in 1:NP2){
    DmatWind[i,j] <- abs(means[i]-means[j])
  }
}

data$DmatWind <- DmatWind
data$wind_ind <- ind_wind


# waves
ind_waves<-as.integer(cut(data$waves, quantile(data$waves, prob = 0:NP / NP, names = FALSE), include = TRUE))
means <- sapply(split(data$waves, cut(data$waves, quantile(data$waves, prob = 0:NP / NP, names = FALSE), include = TRUE)), mean)
means <- c(.07, .27, .47, means, 2.1, 2.3, 2.5)
NP2 <- length(means)
ind_waves = ind_waves+3

DmatWaves <- matrix(NA, NP2, NP2)
for(i in 1:NP2){
  for(j in 1:NP2){
    DmatWaves[i,j] <- abs(means[i]-means[j])
  }
}

data$DmatWaves <- DmatWaves
data$waves_ind <- ind_waves

# Moisture

ind_moisture<-as.integer(cut(data$moisture, quantile(data$moisture, prob = 0:NP / NP, names = FALSE), include = TRUE))
means <- sapply(split(data$moisture, cut(data$moisture, quantile(data$moisture, prob = 0:NP / NP, names = FALSE), include = TRUE)), mean)
means <- c(.4, .8, 1.2, means, 22, 30, 40)
NP2 <- length(means)
ind_mositure = ind_moisture+3
DmatMoisture <- matrix(NA, NP2, NP2)
for(i in 1:NP2){
  for(j in 1:NP2){
    DmatMoisture[i,j] <- abs(means[i]-means[j])
  }
}

data$DmatMoisture <- DmatMoisture
data$moisture_ind <- ind_moisture

# Zscore

ind_zscore<-as.integer(cut(data$zscore, quantile(data$zscore, prob = 0:NP / NP, names = FALSE), include = TRUE))
means <- sapply(split(data$zscore, cut(data$zscore, quantile(data$zscore, prob = 0:NP / NP, names = FALSE), include = TRUE)), mean)
means <- c(-2.5, -2, -1.6, means, 2.5, 3, 3.5)
NP2 <- length(means)
ind_zscore = ind_zscore+3


DmatZscore <- matrix(NA, NP2, NP2)
for(i in 1:NP2){
  for(j in 1:NP2){
    DmatZscore[i,j] <- abs(means[i]-means[j])
  }
}
data$DmatZscore <- DmatZscore
data$zscore_ind <- ind_zscore

# Heat 

ind_heat<-as.integer(cut(data$heat, quantile(data$heat, prob = 0:NP / NP, names = FALSE), include = TRUE))
means <- sapply(split(data$heat, cut(data$heat, quantile(data$heat, prob = 0:NP / NP, names = FALSE), include = TRUE)), mean)
means <- c(20, 21.5, 23, means, 31, 32.5, 34)
NP2 <- length(means)
ind_heat = ind_heat+3


Dmatheat <- matrix(NA, NP2, NP2)
for(i in 1:NP2){
  for(j in 1:NP2){
    Dmatheat[i,j] <- abs(means[i]-means[j])
  }
}
data$DmatHeat <- Dmatheat
data$heat_ind <- ind_heat


data$NP = NP2