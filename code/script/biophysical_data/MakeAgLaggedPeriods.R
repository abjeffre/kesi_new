

library(tidyverse)

###############################################################################
############### LAGGED RAINFALL ###############################################

Precip<-utils::read.csv("./data/MicheweniPrecipitation.csv")[,-1]
Precip$period<-NA
#2012, 2016, 2020 were leap years

Precip<-Precip%>%
  dplyr::mutate(year = lubridate::year(day))

for(i in 2010:2022){  #we'll do 2023 outside
  if(i %in%c(2012,2016,2020)){ #if it's a leap year
    Precip[Precip$year==i,]$period<-c(rep(1:4,each=14),rep(5,15),rep(6:26,each=14),26) #add an extra day in for the extra day in feb. Leap day is in period 5
  }
  
  if(!i %in%c(2012,2016,2020)){
    Precip[Precip$year==i,]$period<-c(rep(1:26,each=14),26)
  }
}

Precip[Precip$year==2023,]$period<-c(rep(1:21,each=14),rep(22,10)) #fills 2023 up to oct 31




#Get cumulative rainfall for past 4 months for each period, each year (90 days)
#Make a unique classifier for each period year combo
Precip$PeriodID<-paste0(Precip$year,"_",Precip$period)
Precip$PeriodID<-factor(Precip$PeriodID,levels=unique(Precip$PeriodID))


Precip<-Precip%>%group_by(PeriodID)%>%summarise(cummRain=sum(pr),period=median(period),year=median(year))
Precip$LagCummRain2<-NA

for(i in which(Precip$PeriodID=="2011_1"):nrow(Precip)){
  Precip[i,]$LagCummRain2<-sum(Precip[(i-4):(i-1),]$cummRain) #total rain over the last two months (8 periods)
}

Precip$LagCummRain3<-NA
for(i in which(Precip$PeriodID=="2011_1"):nrow(Precip)){
  Precip[i,]$LagCummRain3<-sum(Precip[(i-6):(i-1),]$cummRain) #total rain over the last three months (8 periods)
}

Precip$LagCummRain4<-NA
for(i in which(Precip$PeriodID=="2011_1"):nrow(Precip)){
  Precip[i,]$LagCummRain4<-sum(Precip[(i-8):(i-1),]$cummRain) #total rain over the last four months (8 periods)
}

Precip$LagCummRain5<-NA
for(i in which(Precip$PeriodID=="2011_1"):nrow(Precip)){
  Precip[i,]$LagCummRain5<-sum(Precip[(i-10):(i-1),]$cummRain) #total rain over the last five months (10 periods)
}

Precip$LagCummRain6<-NA
for(i in which(Precip$PeriodID=="2011_1"):nrow(Precip)){
  Precip[i,]$LagCummRain6<-sum(Precip[(i-12):(i-1),]$cummRain) #total rain over the last six months (12 periods)
}


Precip<-filter(Precip,year>2010)

Z_Score<-function(x){
  (x-mean(x,na.rm=T))/(sd(x,na.rm=T))}


Precip<-Precip%>%group_by(period)%>%mutate(zRain=Z_Score(LagCummRain3))

#ggplot(Precip,aes(x=PeriodID,y=cummRain))+geom_point(aes(color=zRain),alpha=0.6,size=3)+scale_color_viridis_c()+
 # scale_x_discrete(labels=Precip$period)

# 
# ###############################################################################
# ############### LAGGED HEAT ###################################################
# 
# MaxTemp<-utils::read.csv("./data/MicheweniMaxTemp.csv")[,-1]
# MaxTemp$period<-NA
# #2012, 2016, 2020 were leap years
# 
# MaxTemp<-MaxTemp%>%
#   dplyr::mutate(year = lubridate::year(day))
# 
# for(i in 2010:2022){  #we'll do 2023 outside
#   if(i %in%c(2012,2016,2020)){ #if it's a leap year
#     MaxTemp[MaxTemp$year==i,]$period<-c(rep(1:4,each=14),rep(5,15),rep(6:26,each=14),26) #add an extra day in for the extra day in feb. Leap day is in period 5
#   }
#   
#   if(!i %in%c(2012,2016,2020)){
#     MaxTemp[MaxTemp$year==i,]$period<-c(rep(1:26,each=14),26)
#   }
# }
# 
# MaxTemp[MaxTemp$year==2023,]$period<-c(rep(1:22,each=14),rep(23,11)) #fills 2023 up to may 31
# 
# #Get cumulative rainfall for past 4 months for each period, each year (90 days)
# #Make a unique classifier for each period year combo
# MaxTemp$PeriodID<-paste0(MaxTemp$year,"_",MaxTemp$period)
# MaxTemp$PeriodID<-factor(MaxTemp$PeriodID,levels=unique(MaxTemp$PeriodID))
# 
# MaxTemp<-MaxTemp%>%group_by(PeriodID)%>%summarise(AvgMaxTemp=mean(MaxTemp),period=median(period),year=median(year))
# MaxTemp$LagAvgHeat<-NA
# 
# for(i in which(MaxTemp$PeriodID=="2011_1"):nrow(MaxTemp)){
#   MaxTemp[i,]$LagAvgHeat<-mean(MaxTemp[(i-8):(i-1),]$AvgMaxTemp) #total rain over the last four months (8 periods)
# }
# MaxTemp<-filter(MaxTemp,year>2010)
# 
# Z_Score<-function(x){
#   (x-mean(x,na.rm=T))/(sd(x,na.rm=T))}
# 
# 
# MaxTemp<-MaxTemp%>%group_by(period)%>%mutate(zHeat=Z_Score(LagAvgHeat))
# 
# #trim the last line
# MaxTemp <- MaxTemp[-length(MaxTemp),]                                             
#                                              
# #########################################################################
# ################### GET HEAT AND RAINFALL RESPONSES #####################
# 
# dryness <-standardize(MaxTemp$LagAvgHeat) * -standardize(Precip$LagCummRain)
# 
# plot(data$y[,5] ~dryness[data$obs_ind])