library(tidyverse)
library(rgee)
library(sf)

ee_check()  #this makes sure the machine that's running this has python that is linked to a GEE API

ee_Initialize(user = 'matthewclark989@u.boisestate.edu')

Pemba<-st_read("~/Pemba_Project/PembaShapeFile.shp")
Mich<-Pemba%>%filter(NAME_2=="Micheweni")

Years<-2010:2023
Precip<-data.frame("NAME_2"=NA,"day"=lubridate::as_date("1999-01-01"),"pr"=NA) #this makes the df container. We delete this row l8r
for(i in Years){
chirps <- ee$ImageCollection("UCSB-CHG/CHIRPS/DAILY") %>%
  ee$ImageCollection$filterDate(paste0(i,"-01-01"), paste0(i+1,"-01-01") )%>% #get the data through the first of the month in the next year (or else we lose new years)
  ee$ImageCollection$map(function(x) x$select("precipitation")) %>% # Select only precipitation bands
  ee$ImageCollection$toBands() #%>% # from imagecollection to image
  #ee$Image$rename(sprintf("PP_%02d",364)) # rename the bands of an image


ee_Pemba_rain <- ee_extract(x = chirps, y = Mich["NAME_2"], sf = FALSE)



yearly_precip<-ee_Pemba_rain %>%
  group_by(NAME_2)%>%summarise_all(median,na.rm=TRUE)%>%
  pivot_longer(-NAME_2, names_to = "day", values_to = "pr") %>%
  mutate(day=lubridate::as_date(substr(day , 2, 9)))

Precip<-rbind(Precip,yearly_precip)}
Precip<-Precip[-1,]

write.csv(Precip,"./data/MicheweniPrecipitation.csv")


  ggplot(Precip,aes(x = day, y = pr)) +
  geom_line(alpha = 0.99,color="blue") +
  xlab("day") +
  ylab("Precipitation (mm)") +
  theme_minimal()
