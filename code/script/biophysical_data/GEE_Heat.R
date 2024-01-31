
Pemba<-st_read("~/Pemba_Project/PembaShapeFile.shp")
Mich<-Pemba%>%filter(NAME_2=="Micheweni")

Years<-2010:2023
MaxTemp<-data.frame("NAME_2"=NA,"day"=lubridate::as_date("1999-01-01"),"MaxTemp"=NA) #this makes the df container. We delete this row l8r
for(i in Years){
  ERA5 <- ee$ImageCollection("ECMWF/ERA5_LAND/DAILY_AGGR") %>%
    ee$ImageCollection$filterDate(paste0(i,"-01-01"), paste0(i+1,"-01-01") )%>% #get the data through the first of the month in the next year (or else we lose new years)
    ee$ImageCollection$map(function(x) x$select("temperature_2m_max")) %>% # Select only precipitation bands
    ee$ImageCollection$toBands() #%>% # from imagecollection to image
  #ee$Image$rename(sprintf("PP_%02d",364)) # rename the bands of an image
  
  
  ee_Pemba_temp <- ee_extract(x = ERA5, y = Mich["NAME_2"], sf = FALSE)
  
  
  
  yearly_temp<-ee_Pemba_temp %>%
    group_by(NAME_2)%>%summarise_all(median,na.rm=TRUE)%>%
    pivot_longer(-NAME_2, names_to = "day", values_to = "MaxTemp") %>%
    mutate(day=lubridate::as_date(substr(day , 2, 9)))
  
  MaxTemp<-rbind(MaxTemp,yearly_temp)}
MaxTemp<-MaxTemp[-1,]

write.csv(MaxTemp,"./data/MicheweniMaxTemp.csv")


ggplot(MaxTemp,aes(x = lubridate::as_date(day), y = MaxTemp)) +
  geom_line(alpha = 0.99,color="red") +
  xlab("day") +
  ylab("Maximum daily temp at 2m (K)") +
  theme_minimal()
