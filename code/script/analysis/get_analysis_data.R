################################################################
############## GET SECTOR DATA #################################

#######################
###### PACKAGES #######


###############################
####### LOAD MAIN CSV #########

 dpp<-read.csv("data/FullPeriodizedData.csv")

# dpp<-read.csv("data/FullPeriodizedData_only_trees.csv")

sectors <- c("housing", "retail", "fishing",  "manufacturing", "agriculture", "cloves", "ntfp", 
             "government", "livestock", "agroforestry")#,  "rice", "sweet_potato")#, "private.sales.and.rentals") 

sectors2 <- c("housing", "retail", "fishing",  "manufacturing", "agriculture", "cloves", "ntfp", 
             "government", "livestock", "agroforestry", "seaweed", "transport", "services")#,  "rice", "sweet_potato")#, "private.sales.and.rentals") 


sectors3 <- c("housing", "retail", "fishing",  "manufacturing", "agriculture", "cloves", "ntfp", 
              "government", "livestock", "agroforestry", "seaweed", "services")#, "rice", "sweet_potato")#, "private.sales.and.rentals") 




tonumeric <- c("X", "cummRain", "period", "year","LagCummRain2","LagCummRain3","LagCummRain4",
               "LagCummRain5", "LagCummRain6","zRain", "LagAvgHeat2", "LagAvgHeat3",
               "LagAvgHeat4", "LagAvgHeat5", "LagAvgHeat6", "MedianWaveHeight", "MedianWindSpeed", "cases", sectors)
dpp[, tonumeric] <- apply(dpp[,tonumeric], 2, as.numeric) 
# GET IN PROPER ORDER
dpp <- dpp[order(dpp$X),]
names(dpp)

# Construct total income
dpp[,sectors2]<-dpp[,sectors2]/scaling_factor # Note that the division by 8 just helps the scaling - all post processing must adjust 

dpp$total_inc <- rowSums(dpp[,sectors2])
# dpp[,"agriculture"] <- dpp$cloves + dpp$agriculture
dpp[,"services"] <- dpp$services + dpp$transport

# For only time periods where we have Kesi data
#dpp <- dpp[!is.na(dpp$cases),]

# Define periods for training
start_obs = which(dpp$PeriodStart=="2022-02-26")
end_obs = which(dpp$PeriodStart=="2023-02-12")

# Define periods for imputation
start_est = which(dpp$PeriodStart=="2011-01-01")
end_est = start_obs-1
N_cases = sum(!is.na(dpp$cases))

# Limit Full Model period
dpp<-dpp[1:end_obs, ]




data = list(N_cases = N_cases,
            y = as.matrix(dpp[start_obs:end_obs, sectors3]), 
            K = length(sectors3),
            p = dpp$period,
            n = dpp[start_obs:end_obs, "n"],
            N_obs = length(start_obs:end_obs),
            N_est = length(start_est:end_est),
            sver = ifelse(dpp$X < which(dpp$PeriodStart=="2022-10-08"), 1, 2)[start_obs:end_obs], # This gives the survey vers
            wind = (dpp$MedianWindSpeed[start_est:end_obs]),
            waves = (dpp$MedianWaveHeight[start_est:end_obs]),
            lag2_rain = (dpp$LagCummRain2[start_est:end_obs]/100),
            lag3_rain = (dpp$LagCummRain3[start_est:end_obs]/100),
            lag4_rain = (dpp$LagCummRain4[start_est:end_obs]/100),
            lag5_rain = (dpp$LagCummRain5[start_est:end_obs]/100),
            lag6_rain = (dpp$LagCummRain6[start_est:end_obs]/100),
            moisture = (dpp$cummRain[start_est:end_obs]/100),
            heat = (dpp$MedianMaxTemp[start_est:end_obs]),
            lag2_heat = (dpp$LagAvgHeat2[start_est:end_obs]),
            lag3_heat = (dpp$LagAvgHeat3[start_est:end_obs]),
            lag4_heat = (dpp$LagAvgHeat4[start_est:end_obs]),
            lag5_heat = (dpp$LagAvgHeat5[start_est:end_obs]),
            lag6_heat = (dpp$LagAvgHeat6[start_est:end_obs]),
            zscore = (dpp$zRain[start_est:end_obs]),
            kesi = dpp$cases[start_est:N_cases],
            year = as.integer(as.factor(year(dpp$PeriodStart[start_est:end_obs]))),
            month = as.integer(as.factor(month(dpp$PeriodStart[start_est:end_obs]))),
            obs_ind = start_obs:end_obs,
            est_ind = start_est:end_est,
            cases_ind = start_obs:N_cases,
            gdp = dpp$total_inc[start_obs:N_cases],
            N_overlap = N_cases-end_est
            )

# Add on additional elements required for KESI Estimation
# Make matrix for DMAT
L = length(unique(2011:2022))
Dmat = matrix(NA, L, L)
for(i in 1:L){
  for(j in 1:L){
    Dmat[i, j] = abs(i-j)
  }  
}
data$L = L
data$DmatX= Dmat
data$LP = length(unique(data$p))
data$P1 = 1:length(unique(data$p))

# DMat for Months
DmatM = matrix(NA, 12, 12)
for(i in 1:L){
  for(j in 1:L){
    DmatM[i, j] = abs(i-j)
  }  
}

data$DmatM <- DmatM
data$NM <- 12
data$M1 <- 1:12


# DMat for Periods
DmatP = matrix(NA, 26, 26)
for(i in 1:L){
  for(j in 1:L){
    DmatM[i, j] = abs(i-j)
  }  
}

# data$DmatP <- DmatP

#####################################
########## HARD BACK CAST GDP #######




## CALCULATE RAMADATHAN ##

when_is_ramadan <- function(years){
  seq_of_dates <- seq_days(
    from = paste0(min(years), "-1-1"),
    to = paste0(max(years), "-12-31")
  )
  idates <- sapply(seq_of_dates, ummalqura_algorithm)
  holiday_dates <- seq_of_dates[stringr::str_detect(idates, "^1-9-....") == TRUE]
  return(holiday_dates)
}

ramadan =c()
for(i in 1:(nrow(dpp)-1)){
  pstart = as.Date(dpp$PeriodStart[i])
  pend = as.Date(dpp$PeriodStart[i+1])-1
  prange = pstart:pend
  # NOT THIS WILL NOT WORK IN 10 years or SO
  rstart=when_is_ramadan(year(pstart))
  rend = rstart+31
  rrange = rstart:rend
  ramadan = c(ramadan, ifelse(any(prange %in% rrange), 2, 1))
}


data$ramadan = ramadan[1:(data$N_est + data$N_obs)]
data$ramadan[316] <- 1

# Scale GDP for models
data$y = data$y+1

  
