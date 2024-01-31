if(MAKE!= TRUE) source("code/script/analysis/job_categorization.R")
#library(rethinking)

##############################################
############ FUNCTIONS #######################

# note that Init get redefined a couple times below to help in the data building 
init <- function(x){
  names <- c("type", "subtype", "income", "date", "shehia", "hhid",  "interviewer", "note", "period")
  nr = length(names)
  df <- as.data.frame(matrix(NA, ncol = nr, nrow = x))
  names(df) <- names
  return(df)
}


cnt <- 1

########################################################
################## MAIN INCOME LOOP ####################

dp <- init(0)
# Self
n<-nrow(d$self)
v <- init(n)
v$type = d$self$main_cat
v$income = d$self$profits
v$subtype = ifelse(!is.na(d$self$sub), d$self$sub, d$self$fundi)
v$subtype = ifelse(!is.na(v$subtype), v$subtype, d$self$uchuuzaji)
v$interviewer <- d$self$by
v$shehia <- d$self$shehia
v$date <- d$self$"_submission__submission_time"
v$note <- d$self$notes
v$period <- d$self$period
v$hhid <- d$self$hhID
dp<-rbind(dp, v)


# Wage
n<-nrow(d$wage)
v <- init(n)
v$type = d$wage$main_cat
v$income = d$wage$income
v$subtype = d$wage$government_type
v$interviewer <- d$wage$by
v$shehia <- d$wage$shehia
v$date <- d$wage$"_submission__submission_time"
v$note <- d$wage$notes
v$period <- d$wage$period
v$hhid <- d$wage$hhID
dp<-rbind(dp, v)


# Seaweed
ind = which(!is.na(d$main$mwani_amount)) 
v <- init(length(ind))
v$income=d$main$mwani_amount[ind]*ifelse(is.na(d$main$mwani_price[ind]), 1100, d$main$mwani_price[ind])
v$type = "mwani"
v$date = d$main$`_submission_time`[ind]
v$interviewer = d$main$`_submitted_by`[ind]
v$shehia = d$main$shehia[ind]
v$note = d$main$mwani_note[ind]
v$period = d$main$period[ind]
v$hhid = d$main$hh_ID[ind]
dp<-rbind(dp,v)


##################################
########### AGRICULTURE ##########
Agroforestry = c("mabelungi", "mabungo", "maembe sakuwa", "papai", "maembe sakua", "mabalungi",
                 "machungwa", "balungi", "parachichi", 'mapera', "maembe", "nazi", "fenesi", "mashelishei")



############ AG MAIN ###########
# Sold
ind = which((d$ag_crop$sold > 0 & !is.na(d$ag_crop$sold)) & !d$ag_crop$type %in% c("karafu", "mwani", Agroforestry))
v <- init(length(ind))
v$income=d$ag_crop$price[ind]*as.numeric(d$ag_crop$sold[ind])
v$type = "ag_main"
v$date = d$ag_crop$`_submission__submission_time`[ind]
v$interviewer = d$ag_crop$`_submission_submitted_by`[ind]
v$shehia = d$ag_crop$shehia[ind]
v$note = d$ag_crop$crop_note[ind]
v$period = d$ag_crop$period[ind]
v$hhid = d$ag_crop$hhID[ind]
dp<-rbind(dp,v)

# Subsitence 
correct_prices <- c()
ind2 <- c()
ind = which(!is.na(d$ag_crop$harvest) & (d$ag_crop$sold == 0 & !d$ag_crop$type %in% c("karafu", "mwani", Agroforestry)))

for(i in ind){
  di <- init(1)
  if((d$ag_crop$harvest[i] >0 & !is.na(d$ag_crop$sold[i])) & !is.na(d$ag_crop$harvest[i])){
    di$hhid = d$ag_crop$hhID[i]
    di$type = "ag_main"
    di$shehia = d$ag_crop$shehia[i]
    if(d$ag_crop$unit[i] != "themai" &  !is.na(d$ag_crop$type[i])){
      u=paste(d$ag_crop$type[i], d$ag_crop$unit[i], sep ="_")
      a <- gsub("piece", "kipande", u)
      a <- tolower(a)
      a <- gsub("banana", "ndizi", a)
      a <- gsub("_", " ", a)
      b <- gsub(" ", "_", a)
      temp <- tryCatch({
        dpl[, b]
      }, error=function(e){
        b
      }
      )
      if(is.character(temp)){
        correct_prices <- c(correct_prices, temp)
        price <- NA
        next()
      }else{
        price<-temp
      }
    }else{
      price = 1
    }
    di$income = as.vector(price*(as.numeric(d$ag_crop$harvest[i])-as.numeric(d$ag_crop$sold)[i]))[[1]]
    if(di$income < 0){
      ind2 <- c(ind2, i)
    }
    di$period = d$ag_crop$period[i]
    di$date = d$ag_crop$`_submission_submission_time`[i]
    di$interviewer = d$ag_crop$by[i]
    dp <- rbind(dp, di)
  }
}

############### AGRO FORESTRY #################
# Sold
ind = which((d$ag_crop$sold > 0 & !is.na(d$ag_crop$sold)) & d$ag_crop$type %in% c(Agroforestry))
v <- init(length(ind))
v$income=d$ag_crop$price[ind]*as.numeric(d$ag_crop$sold[ind])
v$type = "agroforestry"
v$date = d$ag_crop$`_submission__submission_time`[ind]
v$interviewer = d$ag_crop$`_submission_submitted_by`[ind]
v$shehia = d$ag_crop$shehia[ind]
v$note = d$ag_crop$crop_note[ind]
v$period = d$ag_crop$period[ind]
v$hhid = d$ag_crop$hhID[ind]
dp<-rbind(dp,v)


# Subsistence
correct_prices <- c()
ind2 <- c()
ind = which(!is.na(d$ag_crop$harvest) & (d$ag_crop$sold == 0 & d$ag_crop$type %in% c(Agroforestry)))

for(i in ind){
  di <- init(1)
  if((d$ag_crop$harvest[i] >0 & !is.na(d$ag_crop$sold[i])) & !is.na(d$ag_crop$harvest[i])){
    di$hhid = d$ag_crop$hhID[i]
    di$type = "agroforestry"
    di$shehia = d$ag_crop$shehia[i]
    if(d$ag_crop$unit[i] != "themai" &  !is.na(d$ag_crop$type[i])){
      u=paste(d$ag_crop$type[i], d$ag_crop$unit[i], sep ="_")
      a <- gsub("piece", "kipande", u)
      a <- tolower(a)
      a <- gsub("banana", "ndizi", a)
      a <- gsub("_", " ", a)
      b <- gsub(" ", "_", a)
      temp <- tryCatch({
        dpl[, b]
      }, error=function(e){
        b
      }
      )
      if(is.character(temp)){
        correct_prices <- c(correct_prices, temp)
        price <- NA
        next()
      }else{
        price<-temp
      }
    }else{
      price = 1
    }
    di$income = as.vector(price*(as.numeric(d$ag_crop$harvest[i])-as.numeric(d$ag_crop$sold)[i]))[[1]]
    if(di$income < 0){
      ind2 <- c(ind2, i)
    }
    di$period = d$ag_crop$period[i]
    di$date = d$ag_crop$`_submission_submission_time`[i]
    di$interviewer = d$ag_crop$by[i]
    dp <- rbind(dp, di)
  }
}

############## Karafuu ###################

# Sold
ind = which((d$ag_crop$sold > 0 & !is.na(d$ag_crop$sold)) & d$ag_crop$type %in% c("karafu"))
v <- init(length(ind))
v$income=d$ag_crop$price[ind]*as.numeric(d$ag_crop$sold[ind])
v$type = "karafu"
v$date = d$ag_crop$`_submission__submission_time`[ind]
v$interviewer = d$ag_crop$`_submission_submitted_by`[ind]
v$shehia = d$ag_crop$shehia[ind]
v$note = d$ag_crop$crop_note[ind]
v$period = d$ag_crop$period[ind]
v$hhid = d$ag_crop$hhID[ind]
dp<-rbind(dp,v)


# Subsistence
correct_prices <- c()
ind2 <- c()
ind = which(!is.na(d$ag_crop$harvest) & (d$ag_crop$sold == 0 & d$ag_crop$type %in% c("karafu")))

for(i in ind){
  di <- init(1)
  if((d$ag_crop$harvest[i] >0 & !is.na(d$ag_crop$sold[i])) & !is.na(d$ag_crop$harvest[i])){
    di$hhid = d$ag_crop$hhID[i]
    di$type = "karafu"
    di$shehia = d$ag_crop$shehia[i]
    if(d$ag_crop$unit[i] != "themai" &  !is.na(d$ag_crop$type[i])){
      u=paste(d$ag_crop$type[i], d$ag_crop$unit[i], sep ="_")
      a <- gsub("piece", "kipande", u)
      a <- tolower(a)
      a <- gsub("banana", "ndizi", a)
      a <- gsub("_", " ", a)
      b <- gsub(" ", "_", a)
      temp <- tryCatch({
        dpl[, b]
      }, error=function(e){
        b
      }
      )
      if(is.character(temp)){
        correct_prices <- c(correct_prices, temp)
        price <- NA
        next()
      }else{
        price<-temp
      }
    }else{
      price = 1
    }
    di$income = as.vector(price*(as.numeric(d$ag_crop$harvest[i])-as.numeric(d$ag_crop$sold)[i]))[[1]]
    if(di$income < 0){
      ind2 <- c(ind2, i)
    }
    di$period = d$ag_crop$period[i]
    di$date = d$ag_crop$`_submission_submission_time`[i]
    di$interviewer = d$ag_crop$by[i]
    dp <- rbind(dp, di)
  }
}


########################## MWANI ################################

#Sold
ind = which((d$ag_crop$sold > 0 & !is.na(d$ag_crop$sold)) & d$ag_crop$type %in% c("mwani"))
v <- init(length(ind))
v$income=d$ag_crop$price[ind]*as.numeric(d$ag_crop$sold[ind])
v$type = "mwani"
v$date = d$ag_crop$`_submission__submission_time`[ind]
v$interviewer = d$ag_crop$`_submission_submitted_by`[ind]
v$shehia = d$ag_crop$shehia[ind]
v$note = d$ag_crop$crop_note[ind]
v$period = d$ag_crop$period[ind]
v$hhid = d$ag_crop$hhID[ind]
dp<-rbind(dp,v)

# 
# # Subsistence
# correct_prices <- c()
# ind2 <- c()
# ind = which(!is.na(d$ag_crop$harvest) & (d$ag_crop$sold == 0 & d$ag_crop$type %in% c("mwani")))
# 
# for(i in ind){
#   di <- init(1)
#   if((d$ag_crop$harvest[i] >0 & !is.na(d$ag_crop$sold[i])) & !is.na(d$ag_crop$harvest[i])){
#     di$hhid = d$ag_crop$hhID[i]
#     di$type = "mwani"
#     di$shehia = d$ag_crop$shehia[i]
#     if(d$ag_crop$unit[i] != "themai" &  !is.na(d$ag_crop$type[i])){
#       u=paste(d$ag_crop$type[i], d$ag_crop$unit[i], sep ="_")
#       a <- gsub("piece", "kipande", u)
#       a <- tolower(a)
#       a <- gsub("banana", "ndizi", a)
#       a <- gsub("_", " ", a)
#       b <- gsub(" ", "_", a)
#       temp <- tryCatch({
#         dpl[, b]
#       }, error=function(e){
#         b
#       }
#       )
#       if(is.character(temp)){
#         correct_prices <- c(correct_prices, temp)
#         price <- NA
#         next()
#       }else{
#         price<-temp
#       }
#     }else{
#       price = 1
#     }
#     di$income = as.vector(price*(as.numeric(d$ag_crop$harvest[i])-as.numeric(d$ag_crop$sold)[i]))[[1]]
#     if(di$income < 0){
#       ind2 <- c(ind2, i)
#     }
#     di$period = d$ag_crop$period[i]
#     di$date = d$ag_crop$`_submission_submission_time`[i]
#     di$interviewer = d$ag_crop$by[i]
#     dp <- rbind(dp, di)
#   }
# }



#######################################################
#################### Forest ###########################

# Timber 
timber = c("kuni", "makuti", "mkaa", "miti kupiga tanu", "miti kujenga", "pegi")
# Forest Sold

ind = which((d$forest_crop$sold > 0 & !is.na(d$forest_crop$sold)) & !d$forest_crop$main_cat %in% timber)
v <- init(length(ind))
v$income=d$forest_crop$price[ind]*as.numeric(d$forest_crop$sold[ind])
v$type = "ntfp"
v$subtype = d$forest_crop$type[ind]
v$date = d$forest_crop$`_submission__submission_time`[ind]
v$interviewer = d$forest_crop$`_submission__submitted_by`[ind]
v$shehia = d$forest_crop$shehia[ind]
v$note = d$forest_crop$notes[ind]
v$period = d$forest_crop$period[ind]
v$hhid = d$forest_crop$hhID[ind]
dp<-rbind(dp,v)



# Subsistence
correct_prices <- c()
ind2 <- c()
ind = which(!is.na(d$forest_crop$harvest) & (d$forest_crop$sold == 0 & !d$forest_crop$main_cat %in% timber))

for(i in ind){
  di <- init(1)
  if((d$forest_crop$harvest[i] >0 & !is.na(d$forest_crop$sold[i])) & !is.na(d$forest_crop$harvest[i])){
    di$hhid = d$forest_crop$hhID[i]
    di$type = "ntfp"
    di$subtype = d$forest_crop$type[i]
    di$shehia = d$forest_crop$shehia[i]
    if(d$forest_crop$unit[i] != "themai" &  !is.na(d$forest_crop$type[i])){
      u=paste(d$forest_crop$type[i], d$forest_crop$unit[i], sep ="_")
      a <- gsub("piece", "kipande", u)
      a <- tolower(a)
      a <- gsub("banana", "ndizi", a)
      a <- gsub("firewood", "kuni", a)
      a <- gsub("_", " ", a)
      b <- gsub(" ", "_", a)
      # Try and find errors
      temp <- tryCatch({
        dpl[, b]
      }, error=function(e){
        b
      }
      )
      if(is.character(temp)){
        correct_prices <- c(correct_prices, temp)
        price <- NA
        next()
      }else{
        price<-temp
      }
    }else{
      price = 1
    }
    di$income = as.vector(price*(as.numeric(d$forest_crop$harvest[i])-as.numeric(d$forest_crop$sold)[i]))[[1]]
    if(di$income < 0){
      ind2 <- c(ind2, i)
    }
    di$period = d$forest_crop$period[i]
    di$date = d$forest_crop$`_submission__submission_time`[i]
    di$interviewer = d$forest_crop$`_submission__submitted_by`[i]
    dp <- rbind(dp, di)
  }
}

##########################################
################## Animals################

#animal sold

for(i in 1:nrow(d$animal)){
  di <- init(1)
  if((d$animal$sold[i] > 0 & d$animal$price[i] > 0) & !is.na(d$animal$sold[i])){
    di$hhid = d$animal$hhID[i]
    di$type = "livestock"
    di$income = d$animal$price[i]
    di$period = d$animal$period[i]
    di$interviewer = d$animal$by[i]
    di$shehia = d$animal$shehia[i]
    di$date = d$animal$`_submission_submission_time`[i]
    dp <- rbind(dp, di)
  }
}

#animal slaughter
for(i in 1:nrow(d$animal)){
  di <- init(1)
  if(d$animal$slaughtered[i] > 0 & !is.na(d$animal$slaughtered[i])){
    di$hhid = d$animal$hhID[i]
    di$type = "livestock"
    di$income = d$animal$slaughtered[i] * median(d$animal$price[d$animal$type == d$animal$type[i]]/ d$animal$sold[d$animal$type == d$animal$type[i]], na.rm = T)
    di$period = d$animal$period[i]
    di$shehia = d$animal$shehia[i]
    di$interviewer = d$animal$by[i]
    di$date = d$animal$`_submission_submission_time`[i]
    dp <- rbind(dp, di)
  }
}


#animals Milk
for(i in 1:nrow(d$main)){
  di <- init(1)
  if(d$main$animals_milk[i] =="yes" & !is.na(d$main$animals_milk[i])){
    di$hhid = d$main$hh_ID[i]
    di$type = "livestock"
    di$income = d$main$animals_milk_collected[i] * dpl$milk_liter
    di$period = d$main$period[i]
    di$shehia = d$main$shehia[i]
    di$date = d$main$`_submission_time`[i]
    di$interviewer = d$main$"_submitted_by"[i]
    v <- rbind(v, di)
  }
}


# safari
n<-nrow(d$safari)
v <- init(n)
v$type = "safari"
v$income = d$safari$income
v$subtype = d$safari$type
v$interviewer <- d$safari$`_submission__submitted_by`
v$shehia <- d$safari$shehia
v$date <- d$safari$"_submission__submission_time"
v$note <- d$safari$note
v$period <- d$safari$period
v$hhid <- d$safari$hhID
dp<-rbind(dp, v)

# Sold 
n<-nrow(d$sold)
v <- init(n)
v$type = "sale"
v$income = d$sold$price
v$subtype = d$sold$type
v$interviewer <- d$sold$`_submission__submitted_by`
v$shehia <- d$sold$shehia
v$date <- d$sold$"_submission__submission_time"
v$note <- d$sold$notes
v$period <- d$sold$period
v$hhid <- d$sold$hhID
dp<-rbind(dp, v)

# Sold
n<-nrow(d$rent)
v <- init(n)
v$type = "rental"
v$income = d$rent$price
v$subtype = d$rent$job_rent
v$interviewer <- d$rent$`_submission__submitted_by`
v$shehia <- d$rent$shehia
v$date <- d$rent$"_submission__submission_time"
v$note <- d$rent$notes
v$period <- d$rent$period
v$hhid <- d$rent$hhID
dp<-rbind(dp, v)


##################################################################################
######################## Do proper recateogrization of Sectors ###################


sort(table(dp$type))
sec <- dp$type # Make sector vector

# Agriculture
sec[grepl("ag_main", dp$type)] <- "agriculture"
sec[grepl("kibarua ya kilimo", dp$type)] <- "agriculture"
sec[grepl("biasahara ya kilimo", dp$type)] <- "agriculture"
sec[grepl("kibarua ya nazi", dp$type)] <- "agriculture"
sec[grepl("kuchuma nazi", dp$type)] <- "agriculture"
sec[grepl("kuuza mazao", dp$type)] <- "agriculture"
sec[grepl("kusagisha nafaka", dp$type)] <- "agriculture"

#

# Seaweed
sec[grepl("mwani", dp$type)] <- "seaweed"

# Cloves
sec[grepl("karafu", dp$type)] <- "cloves"

# Fishing 
sec[grepl("samaki", dp$type)] <- "fishing"
sec[grepl("uvuvi", dp$type)] <- "fishing" # Note this removes all female harvesting
sec[grepl("dalali", dp$type)] <- "fishing"
sec[grepl("kuvua", dp$type)] <- "fishing"
sec[grepl("udalali", dp$type)] <- "fishing"

# Government
sec[grepl("serekali", dp$type)] <- "government"
sec[grepl("mwalimu", dp$type)] <- "government"
sec[grepl("polisi", dp$type)] <- "government"
sec[grepl("pension", dp$type)] <- "government"



# manufacturing - household 
sec[((grepl("fundi", dp$type) & !grepl("ujenzi", dp$type)) & !grepl("ujenzi", dp$subtype)) & !grepl("matofali", dp$subtype)] <- "manufacturing" 
sec[grepl("kutengeneza njukuti", dp$type)] <- "manufacturing"
sec[grepl("makuti", dp$type)] <- "manufacturing"
sec[grepl("kutengeza kamba", dp$type)] <- "manufacturing"
sec[grepl("kutengenza vitu vidogo vidogo", dp$type)] <- "manufacturing"
sec[grepl("kutengeneza sabuni", dp$type)] <- "manufacturing"
sec[grepl("kukata mbao", dp$type)] <- "manufacturing"


# Retail and Bulk trade
sec[grepl("duka", dp$type)] <- "retail"
sec[grepl("uchuuzaji", dp$type)] <- "retail"
sec[grepl("usafiri ya bithaa", dp$type)] <- "retail"
sec[grepl("mafuta na nguo", dp$type)] <- "retail"
sec[grepl("kuuza nguo", dp$type)] <- "retail"
sec[grepl("wakala", dp$type)] <- "retail"
sec[grepl("kubeba", dp$type)] <- "retail"
sec[grepl("mali kachara", dp$type)] <- "retail"

# housing industry
sec[grepl("fundi", dp$type)  & grepl("ujenzi", dp$subtype)] <- "housing" 
sec[grepl("fundi", dp$type)  & grepl("matofali", dp$subtype)] <- "housing" 
sec[grepl("ujenzi", dp$type)] <- "housing" 
sec[grepl("matofali", dp$type)] <- "housing" 

# Transport  
sec[grepl("usafiri", dp$type)] <- "transport" 
sec[grepl("usafari", dp$type)] <- "transport" 

# Food services 
sec[grepl("biashara ya chakula", dp$type)] <- "food services" 
sec[grepl("bekary", dp$type)] <- "food services" 
sec[grepl("biashara ya chakula", dp$type)] <- "food services" 
sec[grepl("butcher", dp$type)] <- "food services" 
sec[grepl("mbuzi", dp$type)] <- "livestock" 

#
sec[grepl("kufua", dp$type)] <- "household services" 
sec[grepl("ndani", dp$type)] <- "household services" 
sec[grepl("pamba", dp$type)] <- "household services" 
sec[grepl("pamba", dp$type)] <- "household services" 

# Health
sec[grepl("afya", dp$type)] <- "health services" 
sec[grepl("medicine", dp$type)] <- "health services" 
sec[grepl("baba asili", dp$type)] <- "health services" 
sec[grepl("mkunga", dp$type)] <- "health services" 


# other serives
sec[grepl("kuchimba kisma", dp$type)] <- "other services" 
sec[grepl("kujitolea", dp$type)] <- "other services" 
sec[grepl("uchoraji", dp$type)] <- "other services" 
sec[grepl("madrassa", dp$type)] <- "other services" 
sec[grepl("hoteli", dp$type)] <- "other services" 
sec[grepl("mshahara ya kampuni", dp$type)] <- "other services" 
sec[grepl("nywele", dp$type)] <- "other services" 
sec[grepl("tasaf", dp$type)] <- "other services" 
sec[grepl("mkataba", dp$type)] <- "other services" 

#chikchi
sec[grepl("chikichi", dp$type)] <- "ntfp" 
# Security
sec[grepl("ulinzi", dp$type)] <- "security" 
# Other primary
sec[grepl("mchanga", dp$type)] <- "mining" 
sec[grepl("kokoto", dp$type)] <- "mining" 

# Remove work trips because what is this??
sec[grepl("kuni", dp$type)] <- NA 




#################################################################
############### Sectors Main ####################################

# Take sectors and refine it more

sectors <- sec

# Combine Territary sectors
sectors[grepl("services", sectors)] <- "services"
sectors[grepl("security", sectors)] <- "services"

#sales and rentals
sectors[grepl("sale", sectors)] <- "private sales and rentals"
sectors[grepl("rent", sectors)] <- "private sales and rentals"

# work trips 
sectors[grepl("safari", sectors)] <- NA 

# mining
sectors[grepl("mining", sectors)] <- NA 
sort(table(sectors))


dp$sec <- sec
dp$sector <- sectors

##################################################################
################### Construct period estimates for each shehia ###

period = c()
gsp = c()
sector = c()
shehia = c()
dmin = as.Date(c())
dmax = as.Date(c())

for(t in 1:max(dp$period)){
  print(t)
  for(i in unique(dp$shehia)){
    for(k in unique(dp$sector)){
      ind = which((dp$period ==t & dp$sector == k) & dp$shehia == i)
      period = c(period, t)
      sector = c(sector, k)
      shehia = c(shehia, i)
      gsp = c(gsp, sum(dp$income[ind],na.rm =T))
      dmin = c(dmin, min(dp$date[ind]))
      dmax = c(dmax, max(dp$date[ind]))
    }
  }
}

gsp[is.infinite(dmin)] <- NA
dmin[is.infinite(dmin)] <- NA
dmax[is.infinite(dmax)] <- NA


df = data.frame(period = period,
                shehia = shehia,
                sector = sector,
                gsp = gsp,
                dmin = dmin,
                dmax = dmax)

df <- df[complete.cases(df),]


#################################################################
############ CONSTRUCT AGGREGATE FOR MSUKA AND KIFUNDI ##########

period = c()
gsp = c()
sector = c()
dmin = as.Date(c())
dmax = as.Date(c())
n = c()
for(t in 2:(max(dp$period)-1)){
  print(t)
  for(k in unique(dp$sector)){
    ind = which((dp$period ==t & dp$sector == k) & dp$shehia %in% c("msuka_magharibi", "kifundi", "konde", "kipange"))
    period = c(period, t)
    n = c(n, length(unique(d$main$hh_ID[dp$shehia %in% c("msuka_magharibi", "kifundi", "konde", "kipange") & dp$period ==t])))
    sector = c(sector, k)
    gsp = c(gsp, sum(dp$income[ind],na.rm =T))
    dmin = c(dmin, min(dp$date[dp$period==t]))
    dmax = c(dmax, max(dp$date[dp$period==t]))
  }
}

#gsp[is.infinite(dmin)] <- NA
#dmin[is.infinite(dmin)] <- NA
#dmax[is.infinite(dmax)] <- NA


df_bkmm = data.frame(period = period,
                     sector = sector,
                     gsp = gsp/n/2500,
                     dmin = dmin,
                     dmax = dmax)

df_bkmm <- df_bkmm[!is.na(df_bkmm$sector),]




