Waves<-read.csv("./data/Waves/Waves_2011.csv")
for(i in 2012:2023){ #Must be 2012:2023
  file<-read.csv(paste0("./data/Waves/Waves_",i,".csv"))
  Waves<-rbind(Waves,file)
                 
}


names(Waves)[]<-c("time","depth","lat","long","height")
##Waves[is.na(Waves$height),]$height<-0   # I don't think we should include this. I think the nans are actual missing values, not 0s
Waves<-Waves%>%filter(long==39.5 &lat==-5)%>%
  mutate(day=lubridate::as_date(substr(time,1,10)))%>%group_by(day)%>%
  summarise(medWaveHeight=median(height))%>%ungroup()

write.csv(Waves,"./data/MicheweniWaves.csv")

ggplot(Waves,aes(x = day, y = medWaveHeight)) +
  geom_line(alpha = 0.99,color="purple") +
  xlab("day") +
  ylab("median significant wave height (m)") +
  theme_minimal()
