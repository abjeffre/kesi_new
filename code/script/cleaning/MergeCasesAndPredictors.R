library(tidyverse)


dk<-read.csv("data/kesi_data4.csv")
dk$TAREHE[9] <- "03/12/2011"
dk$TAREHE[37] <- "12/31/2011"
dk$TAREHE[433] <- "02/17/2017"
dk$TAREHE[433] <- "3/16/2018"
dk$TAREHE[469] <- "08/24/2018"
dk$TAREHE[477] <- "10/05/2018"
dk$TAREHE[80] <- "10/29/2018"
dk$TAREHE[678] <- "03/29/2022"


dk$date<- as.Date(dk$TAREHE, c("%m/%d/%Y"))

dkn<-dk%>%select(date)%>%na.omit()
names(dkn)[1]<-"day"
Key<-read.csv("./data/periodKey.csv")[-1]

dkn$day<-as.character(dkn$day)
dkn<-merge(dkn,Key[c(1,4)],by="day",all.x=TRUE)%>%na.omit()
dk2<-dkn%>%group_by(PeriodID)%>%dplyr::count(name="cases")

PeriodizedPreds<-read.csv("./data/PeriodizedPredictors.csv")


allDat<-merge(PeriodizedPreds,dk2,by="PeriodID",all.x = TRUE)
allDat[is.na(allDat$cases),]$cases<-0 #make all na's into 0's
allDat[allDat$PeriodStart>range(dkn$day)[2],]$cases<-NA  #We don't have data after this point

write.csv(allDat,"./data/FullPeriodizedData.csv")

# MAKE SECOND DATA LIST
dkn <-dk[!grepl("mchanga", dk$MAZAO.YA.MISITU.1), ]
dkn<-dkn%>%select(date)%>%na.omit()
names(dkn)[1]<-"day"
Key<-read.csv("./data/periodKey.csv")[-1]

dkn$day<-as.character(dkn$day)
dkn<-merge(dkn,Key[c(1,4)],by="day",all.x=TRUE)%>%na.omit()
dk2<-dkn%>%group_by(PeriodID)%>%dplyr::count(name="cases")

PeriodizedPreds<-read.csv("./data/PeriodizedPredictors.csv")


allDat<-merge(PeriodizedPreds,dk2,by="PeriodID",all.x = TRUE)
allDat[is.na(allDat$cases),]$cases<-0 #make all na's into 0's
allDat[allDat$PeriodStart>range(dkn$day)[2],]$cases<-NA  #We don't have data after this point

write.csv(allDat,"./data/FullPeriodizedData_only_trees.csv")
