###################################
######### Build Date ##############

if (MAKE != TRUE) source("code/script/cleaning/scrapekobo.r")



#######################################################
############# CHANGE DATES ON ABALLAH ALI BAKAR #####

a=c(583, 584)

d$main$today[a] <-  c("2022-04-20 UTC", "2022-05-13 UTC")

######################################################
############ BUILD FUNCTIONS #########################

toJulian<- function(TS3)
{   
mm<- TS3[2]
xx<- 0
if( mm<=2) {xx<- 1}
mm<- (12*xx)+mm
yy<- TS3[1]-xx
nc<- floor(0.01*yy)
jd<- floor(365.25*yy)+floor(30.6001*(1+mm))+TS3[3]+1720995+(2-(nc-floor(0.25*nc)))
return(jd)
#EG toJulian c(1959,5,24) -> 2436713
#EG toJulian c(1992,12,16) -> 2448973
}



fill = rep(NA , nrow(db$main))
#break apart time
df<- data.frame(year= fill,
   month = fill,
   day = fill,
   jdate = fill,
   start_time = fill,
   end_time = fill)

temp <- as.character.Date(db$main$start)
temp<-str_replace(temp, "-", " ")  
temp<-str_replace(temp, "-", " ")
start_list<- str_split(temp, " ")

temp <- as.character.Date(db$main$end)
temp<-str_replace(temp, "-", " ")  
temp<-str_replace(temp, "-", " ")
end_list<- str_split(temp, " ")


for(i in 1:length(start_list)){
  df[i,"year"] <- start_list[[i]][1]
  df[i,"month"] <- start_list[[i]][2]
  df[i,"day"] <- start_list[[i]][3]
  df[i, "jdate"] <- toJulian(as.numeric(c(start_list[[i]][1],  start_list[[i]][2], start_list[[i]][3])))
  df[i,"start_time"] <- start_list[[i]][4]
  df[i,"end_time"] <- end_list[[i]][4]
}
#bind to main
db$main<- cbind(db$main, df)

# Get current time as Julian
temp <- as.character.Date(Sys.Date())
sysDateJulian<-toJulian(as.numeric(str_split(temp, "-")[[1]]))



####################################################################
################# DO ON d ##########################################




fill = rep(NA , nrow(d$main))
#break apart time
df<- data.frame(year= fill,
                month = fill,
                day = fill,
                jdate = fill,
                start_time = fill,
                end_time = fill)

temp <- as.character.Date(d$main$start)
temp<-str_replace(temp, "-", " ")  
temp<-str_replace(temp, "-", " ")
start_list<- str_split(temp, " ")

temp <- as.character.Date(d$main$end)
temp<-str_replace(temp, "-", " ")  
temp<-str_replace(temp, "-", " ")
end_list<- str_split(temp, " ")


for(i in 1:length(start_list)){
  df[i,"year"] <- start_list[[i]][1]
  df[i,"month"] <- start_list[[i]][2]
  df[i,"day"] <- start_list[[i]][3]
  df[i, "jdate"] <- toJulian(as.numeric(c(start_list[[i]][1],  start_list[[i]][2], start_list[[i]][3])))
  df[i,"start_time"] <- start_list[[i]][4]
  df[i,"end_time"] <- end_list[[i]][4]
}
#bind to main
d$main<- cbind(d$main, df)

# Get current time as Julian
temp <- as.character.Date(Sys.Date())
sysDateJulian<-toJulian(as.numeric(str_split(temp, "-")[[1]]))
d$main$julian <- as.numeric(julian(as.POSIXct.default(d$main$"_submission_time"), origin = "2022-02-06 CET"))
