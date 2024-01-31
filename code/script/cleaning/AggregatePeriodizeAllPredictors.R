#source("./MAKE.R")  #If not already run
source("./code/script/biophysical_data/MakeAgLaggedPeriods.R") #if not already run

#we first need to get the wind and waves data summarized into the same periods as the precip data

#This relies on them having unique observations for every day from 2011-01-01 to 2023-05-31

#the WaveWatch data has some missing data 36 observations to be exact
#Let's fill those values into an existing data frame in order to make those values NA in the data
#This also clips any observations after 2023-05-31

Waves<-data.frame(day=as_date(lubridate::as_date("2011-01-01"):lubridate::as_date("2023-10-31"))) 
Waves<-merge(Waves, read.csv("./data/Waves/MicheweniWaves.csv")[-1], by="day",all.x=TRUE )


Wind<-read.csv("./data/MicheweniWind.csv")[-c(1,2,4,5)] #Load daily wind 2011 - 2023
#the wind data has observations for every day so we just need to clip the observations after 2023-05-31
Wind<-Wind%>%filter(day<=lubridate::as_date("2023-10-31"))

#####Interpolate the missing waves data

dd<-data.frame(wave=Waves$medWaveHeight,wind=Wind$wind) #fix here. Differring date range
fit<-lm(wave~wind,data=na.omit(dd))
Waves[which(is.na(Waves$medWaveHeight)),]$medWaveHeight<-predict(fit,dd[which(is.na(dd$wave)),])

##Now assign the periods
Waves<-Waves%>%
  dplyr::mutate(year = lubridate::year(day))

Waves$period<-NA

for(i in 2011:2022){  #we'll do 2023 outside
  if(i %in%c(2012,2016,2020)){ #if it's a leap year
    Waves[Waves$year==i,]$period<-c(rep(1:4,each=14),rep(5,15),rep(6:26,each=14),26) #add an extra day in for the extra day in feb. Leap day is in period 5
  }
  
  if(!i %in%c(2012,2016,2020)){
    Waves[Waves$year==i,]$period<-c(rep(1:26,each=14),26) #one extra day for period 26
  }
}

Waves[Waves$year==2023,]$period<-c(rep(1:21,each=14),rep(22,10)) #fills 2023 up to oct 31

Waves$PeriodID<-paste0(Waves$year,"_",Waves$period)
Waves$PeriodID<-factor(Waves$PeriodID,levels=unique(Waves$PeriodID))

write.csv(Waves[-2],"./data/periodKey.csv")

#Now do wind
##Now assign the periods
Wind<-Wind%>%
  dplyr::mutate(year = lubridate::year(day))

Wind$period<-NA

for(i in 2011:2022){  #we'll do 2023 outside
  if(i %in%c(2012,2016,2020)){ #if it's a leap year
    Wind[Wind$year==i,]$period<-c(rep(1:4,each=14),rep(5,15),rep(6:26,each=14),26) #add an extra day in for the extra day in feb. Leap day is in period 5
  }
  
  if(!i %in%c(2012,2016,2020)){
    Wind[Wind$year==i,]$period<-c(rep(1:26,each=14),26) #one extra day for period 26
  }
}

Wind[Wind$year==2023,]$period<-c(rep(1:21,each=14),rep(22,10)) #fills 2023 up to oct 31

Wind$PeriodID<-paste0(Wind$year,"_",Wind$period)
Wind$PeriodID<-factor(Wind$PeriodID,levels=unique(Wind$PeriodID))

#Now do heat
MaxTemp<-read.csv("./data/MicheweniMaxTemp.csv")
MaxTemp<-MaxTemp%>%
  dplyr::mutate(year = lubridate::year(day))

MaxTemp$period<-NA

for(i in 2010:2022){  #we'll do 2023 outside
  if(i %in%c(2012,2016,2020)){ #if it's a leap year
    MaxTemp[MaxTemp$year==i,]$period<-c(rep(1:4,each=14),rep(5,15),rep(6:26,each=14),26) #add an extra day in for the extra day in feb. Leap day is in period 5
  }
  
  if(!i %in%c(2012,2016,2020)){
    MaxTemp[MaxTemp$year==i,]$period<-c(rep(1:26,each=14),26)
  }
}

MaxTemp$MaxTemp<-MaxTemp$MaxTemp-273.15 #Change to celcius
MaxTemp[MaxTemp$year==2023,]$period<-c(rep(1:22,each=14),rep(23,11)) #fills 2023 up to may 31
MaxTemp$PeriodID<-paste0(MaxTemp$year,"_",MaxTemp$period)
MaxTemp$PeriodID<-factor(MaxTemp$PeriodID,levels=unique(MaxTemp$PeriodID))

#######################
######## JEFF ADD


MaxTemp2<-MaxTemp%>%group_by(PeriodID)%>%summarise(AvgMaxTemp=mean(MaxTemp),period=median(period),year=median(year))


# Two Month Lag
MaxTemp2$LagAvgHeat2<-NA
for(i in which(MaxTemp2$PeriodID=="2011_1"):nrow(MaxTemp2)){
  MaxTemp2[i,]$LagAvgHeat2<-mean(MaxTemp2[(i-4):(i-2),]$AvgMaxTemp) #Average Max Temp over the last 2 months (4 periods)
}


# Threee Month Lag
MaxTemp2$LagAvgHeat3<-NA
for(i in which(MaxTemp2$PeriodID=="2011_1"):nrow(MaxTemp2)){
  MaxTemp2[i,]$LagAvgHeat3<-mean(MaxTemp2[(i-6):(i-4),]$AvgMaxTemp) #Average Max Temp over the last 3 months (6 periods)
}

# Four Month Lag
MaxTemp2$LagAvgHeat4<-NA
for(i in which(MaxTemp2$PeriodID=="2011_1"):nrow(MaxTemp2)){
  MaxTemp2[i,]$LagAvgHeat4<-mean(MaxTemp2[(i-8):(i-6),]$AvgMaxTemp) #Average Max Temp the last four months (8 periods)
}

# Five Month Lag
MaxTemp2$LagAvgHeat5<-NA
for(i in which(MaxTemp2$PeriodID=="2011_1"):nrow(MaxTemp2)){
  MaxTemp2[i,]$LagAvgHeat5<-mean(MaxTemp2[(i-10):(i-8),]$AvgMaxTemp) #Average Max Temp the last five months (10 periods)
}

# Six Month Lag
MaxTemp2$LagAvgHeat6<-NA
for(i in which(MaxTemp2$PeriodID=="2011_1"):nrow(MaxTemp2)){
  MaxTemp2[i,]$LagAvgHeat6<-mean(MaxTemp2[(i-12):(i-10),]$AvgMaxTemp) #Average Max Temp the last six months (10 periods)
}



Z_Score<-function(x){
  (x-mean(x,na.rm=T))/(sd(x,na.rm=T))}
MaxTemp2<-MaxTemp2%>%group_by(period)%>%mutate(zHeat=Z_Score(LagAvgHeat4))
#trim the last line
#MaxTemp2<-filter(MaxTemp2,day>=lubridate::as_date("2011-01-01"))

# Properly trim Max TEMP
MaxTemp2 <- MaxTemp2[-(1:26),]
MaxTemp2 <- MaxTemp2[-((nrow(Precip)+1):nrow(MaxTemp2)),]       
# dryness <-standardize(MaxTemp2$LagAvgHeat) * -standardize(Precip$LagCummRain4)

## END

MaxTemp<-filter(MaxTemp,day>=lubridate::as_date("2011-01-01"))
MaxTemp<-filter(MaxTemp,day<=lubridate::as_date("2023-10-31"))


#########################################################################
################### GET HEAT AND RAINFALL RESPONSES #####################

#### END



#Now do the period-wise summary
Waves<-Waves%>%group_by(PeriodID)%>%summarise(MedianWaveHeight=median(medWaveHeight),period=median(period),year=median(year))
Wind<-Wind%>%group_by(PeriodID)%>%summarise(MedianWindSpeed=median(wind),period=median(period),year=median(year))
MaxTemp<-MaxTemp%>%group_by(PeriodID)%>%summarise(MedianMaxTemp=median(MaxTemp),period=median(period),year=median(year))
MaxTemp$LagAvgHeat2 <- MaxTemp2$LagAvgHeat2
MaxTemp$LagAvgHeat3 <- MaxTemp2$LagAvgHeat3
MaxTemp$LagAvgHeat4 <- MaxTemp2$LagAvgHeat4
MaxTemp$LagAvgHeat5 <- MaxTemp2$LagAvgHeat5
MaxTemp$LagAvgHeat6 <- MaxTemp2$LagAvgHeat6

BioPhysPeriod<-cbind(Precip,Waves[2],Wind[2],MaxTemp[2],MaxTemp[5], MaxTemp[6], MaxTemp[7], MaxTemp[8], MaxTemp[9])


Key<-read.csv("./data/periodKey.csv")[-1]
Key<-Key%>%mutate(PeriodID=factor(PeriodID,levels=unique(PeriodID)))%>%group_by(PeriodID)%>%summarise(StartDay=min(day))

BioPhysPeriod$PeriodStart<-Key$StartDay

#Let's add these same periods to the household data

##From Source Data
dp2<-dp%>%mutate(day=lubridate::as_date(date))%>%filter(shehia %in% use_shehia) %>%select(day,sector,income)%>%group_by(day,sector)%>%
  summarise(income=sum(income))%>%merge(.,read.csv("./data/periodKey.csv")[c(2,5)],by="day",all.x=TRUE)%>%
  group_by(PeriodID,sector)%>%mutate(income=ifelse(is.na(income),0,income))%>%summarise(income=sum(income,na.rm=T))%>%
  tidyr::pivot_wider(names_from = "sector", values_from = "income",values_fill = 0)
 

# Lets calculate the total number of observations during each period to get the average household income
n_obs<-dp%>%mutate(day=lubridate::as_date(date))%>%filter(shehia %in% use_shehia)%>%select(hhid,day)%>%group_by(day)%>% merge(.,read.csv("./data/periodKey.csv")[c(2,5)],by="day",all.x=TRUE)%>%
  group_by(PeriodID,hhid)%>%group_by(PeriodID) %>% summarise(n_obs=length(unique(hhid)))

# Now adust average income by the total number of observations in that period and put it in euro
for(i in 1:nrow(dp2)){
  dp2[i,2:ncol(dp2)] <- dp2[i,2:ncol(dp2)]/n_obs$n_obs[i]/2500
}

dp2$n = n_obs$n_obs
###Old code using pre-periodized income vars

#HouseDF<-df_bkmm%>%
 # tidyr::pivot_wider(names_from = "sector", values_from = "gsp")  #get the data into wide form where each period is an observation

#HouseDF<-HouseDF[-1]
#HouseDF$PeriodID<-NA

#for(i in 1:nrow(HouseDF)){
#row<- which(BioPhysPeriod$PeriodStart>=HouseDF[i,]$dmin & BioPhysPeriod$PeriodStart<=HouseDF[i,]$dmax) #which row in bio phys data has a 
#start time of the period that is within the sample period for the household data
#P_ID<-BioPhysPeriod[row,]$PeriodID #What's the period ID from that row
#HouseDF[i,]$PeriodID<-P_ID #Make that period ID the HouseDF period ID
#}

#PeriodizedPreds<-merge(BioPhysPeriod,HouseDF[3:17], by="PeriodID",all.x=TRUE)

PeriodizedPreds<-merge(BioPhysPeriod,dp2, by="PeriodID",all.x=TRUE)
write.csv(PeriodizedPreds,"./data/PeriodizedPredictors.csv")

