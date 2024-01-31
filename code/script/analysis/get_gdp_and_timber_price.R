######################################################
################### Prep GDP #########################

df_g<-read_xls("data/API_NY.GDP.PCAP.CD_DS2_en_excel_v2_5994922.xls")
gdp_2010_2022<-df_g[df_g$`Country Name` == "Tanzania",which(names(df_g)=='2011'):which(names(df_g)=='2022')] 

scale_gdp_temp = gdp_2010_2022/gdp_2010_2022$"2022"
scale_gdp = rep(NA, data$N_obs+data$N_est)
for(i in 1:length(data$year)){
  if(data$year[i] < 13){
    scale_gdp[i] = scale_gdp_temp[data$year[i]]    
  }else{
    scale_gdp[i] = 1
  }
}

data$scale_gdp<-unlist(scale_gdp)

################################################################
############## PREP Consumer Price Index #######################

df_cpi<-read_csv("data/API_FP.CPI.TOTL_DS2_en_csv_v2_5994751.csv")
cpi_2010_2022<-df_cpi[df_cpi$`Country Name` == "Tanzania",which(names(df_cpi)=='2011'):which(names(df_cpi)=='2022')] 

scale_cpi_temp = cpi_2010_2022/cpi_2010_2022$"2022"
scale_cpi = rep(NA, data$N_obs+data$N_est)
for(i in 1:length(data$year)){
  if(data$year[i] < 13){
    scale_cpi[i] = scale_cpi_temp[data$year[i]]    
  }else{
    scale_cpi[i] = 1
  }
}

data$scale_cpi<-unlist(scale_cpi)


###############################################################
####################### PREP TIMBER ############################'

df_tim<-read_csv("data/lumber-prices-historical-chart-data.csv")
timber_price = rep(NA, nrow(dpp))

for(i in 1:(nrow(dpp)-1)){
  start<-dpp$PeriodStart[i]
  end <-dpp$PeriodStart[i+1]
  temp<-as_date(as_date(start):as_date(end))
  dates <- temp[1:(length(temp)-1)]
  ind <- which(df_tim$date %in% dates)
  timber_price[i] = mean(unlist(c(df_tim[ind, "value"])))
}

s<-as_date(dpp$PeriodStart[nrow(dpp)])
e<-as_date(dpp$PeriodStart[nrow(dpp)])+14
timber_price[length(timber_price)]<- mean(unlist(c(df_tim[which(df_tim$date %in% as_date(s:e)), "value"]))) 

data$timber_prices = log(timber_price/10)
