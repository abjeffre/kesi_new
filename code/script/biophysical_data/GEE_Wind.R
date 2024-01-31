

Pemba<-st_read("~/Pemba_Project/PembaShapeFile.shp")
Mich<-Pemba%>%filter(NAME_2=="Micheweni")

Years<-2011:2023


#First get the EASTERLY wind
Eastwind<-data.frame("NAME_2"=NA,"day"=lubridate::as_date("1999-01-01"),"windE"=NA)
for(i in Years){
  LandDat <- ee$ImageCollection("ECMWF/ERA5_LAND/DAILY_AGGR") %>%
    ee$ImageCollection$filterDate(paste0(i,"-01-01"), paste0(i+1,"-01-01") )%>%
    ee$ImageCollection$map(function(x) x$select("u_component_of_wind_10m")) %>% # Select only easterly wind bands
    ee$ImageCollection$toBands() #%>% # from imagecollection to image
  #ee$Image$rename(sprintf("PP_%02d",364)) # rename the bands of an image
  
  
  ee_Pemba_easterly_wind <- ee_extract(x = LandDat, y = Mich["NAME_2"], sf = FALSE)
  
  
  
  yearly_easterly_wind<-ee_Pemba_easterly_wind %>%
    group_by(NAME_2)%>%summarise_all(median,na.rm=TRUE)%>%
    pivot_longer(-NAME_2, names_to = "day", values_to = "windE") %>%
    mutate(day=lubridate::as_date(substr(day , 2, 9)))
  
  Eastwind<-rbind(Eastwind,yearly_easterly_wind)}
Eastwind<-Eastwind[-1,]


#Now get the Northerly wind!
Northwind<-data.frame("NAME_2"=NA,"day"=lubridate::as_date("1999-01-01"),"windN"=NA)
for(i in Years){
  LandDat <- ee$ImageCollection("ECMWF/ERA5_LAND/DAILY_AGGR") %>%
    ee$ImageCollection$filterDate(paste0(i,"-01-01"), paste0(i+1,"-01-01") )%>%
    ee$ImageCollection$map(function(x) x$select("v_component_of_wind_10m")) %>% # Select only northerly wind bands
    ee$ImageCollection$toBands() #%>% # from imagecollection to image
  #ee$Image$rename(sprintf("PP_%02d",364)) # rename the bands of an image
  
  
  ee_Pemba_northerly_wind <- ee_extract(x = LandDat, y = Mich["NAME_2"], sf = FALSE)
  
  
  
  yearly_northerly_wind<-ee_Pemba_northerly_wind %>%
    group_by(NAME_2)%>%summarise_all(median,na.rm=TRUE)%>%
    pivot_longer(-NAME_2, names_to = "day", values_to = "windN") %>%
    mutate(day=lubridate::as_date(substr(day , 2, 9)))
  
  Northwind<-rbind(Northwind,yearly_northerly_wind)}
Northwind<-Northwind[-1,]


Wind<-cbind(Eastwind,Northwind[3])
Wind$wind<-sqrt(Wind$windN^2+Wind$windE^2)



write.csv(Wind,"./data/MicheweniWind.csv")


ggplot(Wind,aes(x = day, y = wind)) +
  geom_line(alpha = 0.99,color="grey") +
  xlab("day") +
  ylab("Combined wind speed (m/s 10m above Earth surface)") +
  theme_minimal()
