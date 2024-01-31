##################################################################################
###################### GET DATA LONG #############################################

`%ni%` <- Negate(`%in%`)
# 
# getData=function(load_imputed){
#   #################################################
#   ######### IDENTIFY COMPUTER AND SETWD ###########
#   
#   set_project_wd <- function(){
#     user=Sys.info()[[6]]
#     if(user=="jeffrey_andrews") setwd("C:/Users/jeffrey_andrews/OneDrive/Documents/DOL")
#     else if(user=="Jeff") setwd("C:/Users/Jeff/OneDrive/Documents/DOL")
#     else if(user == 'jeffr')  setwd("C:/Users/jeffr/OneDrive/Documents/DOL/")
#   }
#   set_project_wd()
#   user = Sys.info()[[6]]
#   source("code/functions/measurement.R")
#   source("code/functions/data_manipulation.R")
#   source("code/script/cleaning/cleaning.R")
#   
#   
#   
#   ###############################################################################
#   ############# LOAD IMPUTED DATA ###############################################
#   
#   if(load_imputed == TRUE){
#     print("got_here")
#     imputation_list<-readRDS("data/imputation/imputed.RDS")
#     for(j in 1:length(imputation_list)){
#       dat_to_impute <- imputation_list[[j]]
#       if(j == 1)
#         d$self$profits_imputed <- rep("no", nrow(d$self))
#         for(i in 1:nrow(dat_to_impute)){
#           d$self$profits[d$self$index == dat_to_impute$ind[i]] <- dat_to_impute$p[i]
#           d$self$profits_imputed[d$self$index == dat_to_impute$ind[i]] <- "yes"
#         }
#       if(j == 2)
#         d$wage$income_imputed <- rep("no", nrow(d$wage))
#         for(i in 1:nrow(dat_to_impute)){
#           d$wage$income[d$wage$index == dat_to_impute$ind[i]] <- dat_to_impute$p[i]
#           d$wage$income_imputed[d$wage$index == dat_to_impute$ind[i]] <- "yes"
#         }
#     }
#   }
#   
  
  # IDS
  uhhid<- unique(d$main$hh_ID)
  ##################################
  ####### Fishing ##################
  d$self$profits <- as.numeric(d$self$profits)
  fishing <- as.numeric(d$self$profits[d$self$main == "uvuvi"])
  fishing[which(fishing < 100)] = fishing[which(fishing < 100)] *1000 
  fishingtype <- d$self$sub[d$self$main == "uvuvi"]
  fishers <- as.numeric(d$self$crew[d$self$main == "uvuvi"])
  fish_hours <- as.numeric(d$self$hours[d$self$main == "uvuvi"])
  fish_note <- d$self$notes[d$self$main == "uvuvi"]
  
  fish_sum <- data.frame(inc = fishing,
                         type = fishingtype,
                         fishers = fishers,
                         hours = fish_hours,
                         wage = fishing/fish_hours,
                         notes = fish_note
  )
  
  
  d$self$profits[which(d$self$profits < 100)] = d$self$profits[which(d$self$profits < 100)] *1000
  ind<-which(d$self$main == "uvuvi" & d$self$profits > 100000)
  
  
  d$self$'_submission__submitted_by'[ind]
  d$self$hhh[ind]
  d$self$profits[ind]
  
  ###################################
  ######## FUNDI ####################
  
  fundi <- as.numeric(d$self$profits[d$self$main == "fundi"])
  fundi[which(fundi < 100)] = fundi[which(fundi < 100)] *1000 
  funditype <- d$self$sub[d$self$main == "fundi"]
  fundi_hours <- as.numeric(d$self$hours[d$self$main == "fundi"])
  fundi_note <- d$self$notes[d$self$main == "fundi"]
  
  
  
  fundi_sum <- data.frame(inc = fundi,
                          type = funditype,
                          note = fundi_note,
                          hours = fundi_hours,
                          wage = fundi/fundi_hours)
  
  
  d$self$profits[which(d$self$profits < 100)] = d$self$profits[which(d$self$profits < 100)] *1000
  ind<-which(d$self$main == "fundi" & d$self$profits > 100000)
  
  
  d$self$'_submission__submitted_by'[ind]
  d$self$hhh[ind]
  d$self$profits[ind]
  
  ####################################################
  ############# GET FULL LIST OF SALES ###############
  
  
  v <- data.frame(hhID= NA, 
                  type= NA,
                  subtype = NA,
                  value=NA,
                  main = NA,
                  period=NA,
                  shehia = NA,
                  location = NA,
                  hours = NA,
                  by = NA,
                  uuid = NA,
                  julian = NA, 
                  date = NA)
  v2<- v
  
  h <- data.frame(hhID= NA, 
                  main= NA,
                  type = NA,
                  uuid = NA,
                  hours=NA,
                  name = NA,
                  by = NA, 
                  shehia = NA,
                  julian = NA)
  h2 <- h
  
  h$hhID[1] = d$ag$hhID[1]
  h$hours[1] = d$ag$weed_hours[1] + d$ag$weed_hours[1] + d$ag$ag_harvest_hours[1]
  h$name[1] = d$ag$names[1]
  h$period[1] = d$ag$period[1]
  h$by[1] = d$ag$"_submission__submitted_by"[1]
  h$shehia[1]= d$wage$shehia[1]
  h$main = "ag"
  h$julian = d$ag$julian[1]
  
  
  
  
  v$hhID[1] = d$wage$hhID[1]
  v$type[1] = d$wage$main[1]
  v$value[1] = d$wage$income[1]
  v$period[1] = d$wage$period[1]
  v$hours[1] = d$wage$hours[1]
  v$by[1] = d$wage$by[1]
  v$shehia[1] = d$wage$shehia[1]
  v$location[1] = d$wage$location[1]
  v$main = "wage"
  v$julian = d$wage$julian[1]
  v$subtype = d$wage$main[1]
  v$date = d$wage$`_submission_submission_time`[1]
  #wages
  
  for(i in 2:nrow(d$wage)){
    di <- v2
    di$hhID = d$wage$hhID[i]
    di$type = d$wage$main[i]
    di$main = "wage"
    di$value = d$wage$income[i]
    di$hours = d$wage$hours[i]
    di$period = d$wage$period[i]
    di$shehia = d$wage$shehia[i]
    di$by = d$wage$by[i]
    di$julian = d$wage$julian[i]
    di$date = d$wage$`_submission_submission_time`[i]
    v <- rbind(v, di)
  }
  
  
  #wage hours
  
  for(i in 1:nrow(d$wage)){
    di <- h2
    di$hhID = d$wage$hhID[i]
    di$hours = d$wage$hours[i]
    di$name = d$wage$names[i]
    di$period = d$wage$period[i]
    di$shehia = d$wage$shehia[i]
    di$by = d$wage$by[i]
    di$main = "wage"
    di$julian = d$wage$julian[i]
    h <- rbind(h, di)
  }
  
  
  #self
  
  for(i in 1:nrow(d$self)){
    di <- v2
    di$hhID = d$self$hhID[i]
    di$type = d$self$main[i]
    di$subtype = d$self$sub[i]
    di$main = "self"
    di$hours = d$self$hours[i]
    di$value = d$self$profits[i]
    di$period = d$self$period[i]
    di$shehia = d$self$shehia[i]
    di$by = d$self$by[i]
    di$julian = d$self$julian[i]
    di$date = d$self$`_submission_submission_time`[i]
    v <- rbind(v, di)
  }
  
  
  #self hours
  
  for(i in 1:nrow(d$self)){
    di <- h2
    di$hhID = d$self$hhID[i]
    di$hours = d$self$hours[i]
    di$name = d$self$names[i]
    di$period = d$self$period[i]
    di$shehia = d$self$shehia[i]
    di$by = d$self$by[i]
    di$main = ifelse(is.na(d$self$fishing[i]), "self", "fishing") 
    di$julian = d$self$julian[i]
    h <- rbind(h, di)
  }
  
  
  #ag_sold
  
  for(i in 1:nrow(d$ag_crop)){
    di <- v2
    if(d$ag_crop$sold[i] > 0 & !is.na(d$ag_crop$sold[i])){
      di$hhID = d$ag_crop$hhID[i]
      di$type = d$ag_crop$type[i]
      di$main = "ag_sold"
      di$value = d$ag_crop$price[i]*as.numeric(d$ag_crop$sold)[i]
      di$period = d$ag_crop$period[i]
      di$shehia = d$ag_crop$shehia[i]
      di$by = d$ag_crop$by[i]
      di$julian = d$ag_crop$julian[i]
      di$date = d$ag_crop$`_submission_submission_time`[i]
      v <- rbind(v, di)
    }
  }
  
  
  #ag_subsistence 
  # This creates a bin to catch all the bad prices
  correct_prices <- c()
  ind <- c()
  for(i in 1:nrow(d$ag_crop)){
    di <- v2
    if((d$ag_crop$harvest[i] >0 & !is.na(d$ag_crop$sold[i])) & !is.na(d$ag_crop$harvest[i])){
      di$hhID = d$ag_crop$hhID[i]
      di$type = d$ag_crop$type[i]
      di$main = "ag_sub"
      di$shehia = d$ag_crop$shehia[i]
      di$julian = d$ag_crop$julian[i]
      
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
      di$value = price*(as.numeric(d$ag_crop$harvest[i])-as.numeric(d$ag_crop$sold)[i])
      if(di$value < 0){
        ind <- c(ind, i)
      }
      di$period = d$ag_crop$period[i]
      di$date = d$ag_crop$`_submission_submission_time`[i]
      di$by = d$ag_crop$by[i]
      v <- rbind(v, di)
    }
  }
  
  
  
  
  
#   #ag_subsistence 
#   # This creates a bin to catch all the bad prices
#   correct_prices <- c()
#   ind <- c()
#   for(i in 1:nrow(d$ag_crop)){
#     di <- v2
#     if((d$ag_crop$harvest[i] >0 & !is.na(d$ag_crop$sold[i])) & !is.na(d$ag_crop$harvest[i])){
#       di$hhID = d$ag_crop$hhID[i]
#       di$type = d$ag_crop$type[i]
#       di$main = "ag_sub"
#       di$shehia = d$ag_crop$shehia[i]
#       di$julian = d$ag_crop$julian[i]
#       di$date = d$ag_crop$`_submission_submission_time`[i]
#       
#       if(d$ag_crop$unit[i] != "themai" &  !is.na(d$ag_crop$type[i])){
#         u=paste(d$ag_crop$type[i], d$ag_crop$unit[i], sep ="_")
#         a <- gsub("piece", "kipande", u)
#         a <- tolower(a)
#         a <- gsub("banana", "ndizi", a)
#         a <- gsub("_", " ", a)
#         b <- gsub(" ", "_", a)
#         
#         temp <- tryCatch({
#           dpl[, b]
#         }, error=function(e){
#           b
#         }
#         )
#         if(is.character(temp)){
#           correct_prices <- c(correct_prices, temp)
#           price <- NA
#           next()
#         }else{
#           price<-temp
#         }
#       }else{
#         price = 1
#       }
#       di$value = price*(as.numeric(d$ag_crop$harvest[i])-as.numeric(d$ag_crop$sold)[i])
#       if(di$value < 0){
#         ind <- c(ind, i)
#       }
#       di$period = d$ag_crop$period[i]
#       di$by = d$ag_crop$by[i]
#       v <- rbind(v, di)
#     }
# }
#   
  if(length(ind > 0)) stop("Negative Ag harvest - Check! - Look in ind")
    
  
  #ag hours
  for(i in 1:nrow(d$ag)){
    di <- h2
    di$hhID = d$ag$hhID[i]
    di$hours = d$ag$ag_plant_hours[i] + d$ag$[i] + d$ag$ag_harvest_hours[i]
    di$name = d$ag$names[i]
    di$period = d$ag$period[i]
    di$shehia = d$ag$shehia[i]
    di$julian = d$ag$julian[i]
    di$by = d$ag$"_submission__submitted_by"[i]
    di$main = "ag"
    h <- rbind(h, di)
  }
  
  # Clove Hours
    for(i in 1:nrow(d$ag)){
    di <- h2
    di$hhID = d$ag$hhID[i]
    di$hours =  d$ag$ag_prepare_cloves_hours[i] + d$ag$ag_harvest_cloves_hours[i]
    di$name = d$ag$[i]
    di$period = d$ag$period[i]
    di$shehia = d$ag$shehia[i]
    di$julian = d$ag$julian[i]
    di$by = d$ag$"_submission__submitted_by"[i]
    di$main = "cloves"
    h <- rbind(h, di)
  }
  
# Mwani Hours

  for(i in 1:nrow(db2$mwani_emp_source)){
    di <- h2
    di$hhID = d$main$hhh[which(d$main$"_uuid" == db2$mwani_emp_source$"_submission__uuid"[i])]
    di$hours =  db2$mwani_emp_source$ag_harvest_mwani_hours[i]+db2$mwani_emp_source$ag_prepare_mwani_hours[i]
    di$name =   db2$mwani_emp_source$mwani_names[i]
    di$period = d$main$period[which(d$main$"_uuid" == db2$mwani_emp_source$"_submission__uuid"[i])]
    di$shehia = d$main$shehia[which(d$main$"_uuid" == db2$mwani_emp_source$"_submission__uuid"[i])]
    di$julian = d$main$julian[which(d$main$"_uuid" == db2$mwani_emp_source$"_submission__uuid"[i])]
    di$by = d$main$"_submitted_by"[which(d$main$"_uuid" == db2$mwani_emp_source$"_submission__uuid"[i])]
    di$main = "mwani"
    h <- rbind(h, di)
  }
  

  #animal sold
  
  for(i in 1:nrow(d$animal)){
    di <- v2
    if((d$animal$sold[i] > 0 & d$animal$price[i] > 0) & !is.na(d$animal$sold[i])){
      di$hhID = d$animal$hhID[i]
      di$type = d$animal$type[i]
      di$main = "animal_sold"
      di$value = d$animal$price[i]
      di$period = d$animal$period[i]
      di$by = d$animal$by[i]
      di$julian = d$animal$julian[i]
      di$shehia = d$animal$shehia[i]
      di$date = d$animal$`_submission_submission_time`[i]
      v <- rbind(v, di)
    }
  }
  
  #animal slaughter
  for(i in 1:nrow(d$animal)){
    di <- v2
    if(d$animal$slaughtered[i] > 0 & !is.na(d$animal$slaughtered[i])){
      di$hhID = d$animal$hhID[i]
      di$main = "slaughtered"
      di$type = d$animal$type[i]
      di$value = d$animal$slaughtered[i] * median(d$animal$price[d$animal$type == d$animal$type[i]]/ d$animal$sold[d$animal$type == d$animal$type[i]], na.rm = T)
      di$period = d$animal$period[i]
      di$shehia = d$animal$shehia[i]
      di$julian = d$animal$julian[i]
      di$by = d$animal$by[i]
      di$date = d$animal$`_submission_submission_time`[i]
      v <- rbind(v, di)
    }
  }
  
  
  #animals Milk
  for(i in 1:nrow(d$main)){
    di <- v2
    if(d$main$animals_milk[i] =="yes" & !is.na(d$main$animals_milk[i])){
      di$hhID = d$main$hh_ID[i]
      di$main = "milk"
      di$type = NA
      di$value = d$main$animals_milk_collected[i] * dpl$milk_liter
      di$period = d$main$period[i]
      di$shehia = d$main$shehia[i]
      di$julian = d$main$julian[i]
      di$date = d$main$`_submission_time`[i]
      di$by = d$main$"_submitted_by"[i]
      v <- rbind(v, di)
    }
  }
  
  
  
  #animals hours
  for(i in 1:nrow(d$animal_emp)){
    di <- h2
    di$hhID = d$animal_emp$hhID[i]
    di$hours = d$animal_emp$animals_hours[i]
    di$name = d$animal_emp$animals_hours[i]
    di$period = d$animal_emp$period[i]
    di$shehia = d$animal_emp$shehia[i]
    di$julian = d$animal_emp$julian[i]
    di$by = d$animal_emp$"_submission__submitted_by"[i]
    di$main = "animals"
    h <- rbind(h, di)
  }
  
  
  
  
  # Forest Sold
  
  for(i in 1:nrow(d$forest_crop)){
    di <- v2
    if(d$forest_crop$sold[i] > 0 & !is.na(d$forest_crop$sold[i])){
      di$hhID = d$forest_crop$hhID[i]
      di$type = d$forest_crop$type[i]
      di$main = "forest_sold"
      di$shehia = d$forest_crop$shehia[i]
      di$value = as.numeric(d$forest_crop$price[i])*as.numeric(d$forest_crop$sold)[i]
      di$period = d$forest_crop$period[i]
      di$julian = d$forest_crop$julian[i]
      di$by = d$forest_crop$by[i]
      di$by = d$forest_crop$"_submission__uuid"[i]
      di$date = d$forest_crop$`_submission_submission_time`[i]
      v <- rbind(v, di)
    }
  }
  
  
  # Forest_subsistence 
  
  ind <- c()
  zeros <-c()
  for(i in 1:nrow(d$forest_crop)){
    di <- v2
    if((d$forest_crop$harvest[i] >0 & !is.na(d$forest_crop$sold[i])) & !is.na(d$forest_crop$harvest[i])){
      di$hhID = d$forest_crop$hhID[i]
      di$type = d$forest_crop$type[i]
      di$shehia = d$forest_crop$shehia[i]
      di$main = "forest_sub"
      di$julian = d$forest_crop$julian[i]
      di$uuid = d$forest_crop$"_submission__uuid"[i]
      di$date = d$forest_crop$`_submission_submission_time`[i]
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
      di$value = price*(as.numeric(d$forest_crop$harvest[i])-as.numeric(d$forest_crop$sold)[i])
      if(di$value == 0){
        # print(paste("ERROR: GOT ZERO VALUE ON LINE", i, "of Forest goods"))
        zeros <- c(zeros, i)
      }
      if(di$value < 0){
        ind <- c(ind, i)
      }
      di$period = d$forest_crop$period[i]
      di$by = d$forest_crop$by[i]
      v <- rbind(v, di)
    }
  }
  
  
# ADD IN MEDICINE 

  
  
  if(length(ind > 0)) stop("Negative Forest harvest - Check! - Look in ind")
  
  unique(correct_prices)
  
  
  #forests hours
  for(i in 1:nrow(d$forest)){
    di <- h2
    di$hhID = d$forest$hhID[i]
    di$hours = d$forest$forest_hours[i]
    di$name = d$forest$forest_names[i]
    di$period = d$forest$period[i]
    di$shehia = d$forest$shehia[i]
    di$julian = d$forest$julian[i]
    di$type = d$forest$main[i]
    di$uuid = d$forest$"_submission__uuid"[i]
    di$by = d$forest$"_submission__submitted_by"[i]
    di$main = "forests"
    h <- rbind(h, di)
  }
  
  
  
  
  # Rental Income
  
  
  for(i in 1:nrow(d$rent)){
    di <- v2
    di$hhID = d$rent$hhID[i]
    di$type = d$rent$rent_other[i]
    di$main = "rent"
    di$value = d$rent$rent_price[i]
    di$period = d$rent$period[i]
    di$shehia = d$rent$shehia[i]
    di$by = d$rent$"_submission__submitted_by"[i]
    di$julian = d$rent$julian[i]
    di$date = d$rent$`_submission__submission_time`[i]
    v <- rbind(v, di)
  }
  
  # Sale Income
  
  
  for(i in 1:nrow(d$sold)){
    di <- v2
    di$hhID = d$sold$hhID[i]
    di$type = d$sold$sold_other[i]
    di$main = "sold"
    di$value = d$sold$sold_price[i]
    di$period = d$sold$period[i]
    di$shehia = d$sold$shehia[i]
    di$by = d$sold$"_submission__submitted_by"[i]
    di$julian = d$sold$julian[i]
    di$date = d$sold$`_submission__submission_time`[i]
    v <- rbind(v, di)
  }
  
  
  # Store-Credit
  
  for(i in 1:nrow(d$store_credit)){
    di <- v2
    di$hhID = d$store_credit$hhID[i]
    di$type = d$store_credit$store_credit_items[i]
    di$main = "store_credit"
    di$value = d$store_credit$store_credit_value[i]
    di$period = d$store_credit$period[i]
    di$shehia = d$store_credit$shehia[i]
    di$by = d$store_credit$"_submission__submitted_by"[i]
    di$julian = d$store_credit$julian[i]
    di$date = d$store_credit$'_submission__submission_time'[i]
    v <- rbind(v, di)
  }
  
  
  # Gifts In
  
  for(i in 1:nrow(d$gift_rec)){
    types <-str_split(d$gift_rec$gift_rec_type[i], " ")[[1]]
    for(j in 1:length(types)){
      di <- v2
      di$hhID = d$gift_rec$hhID[i]
      di$type = ifelse(types[j] == "nyingine", d$gift_rec$gift_rec_type_other[i], types[j])
      di$main = "gift_rec"
      di$value = d$gift_rec$gift_rec_value[i]/length(types)
      di$period = d$gift_rec$period[i]
      di$location = ifelse(d$gift_rec$gift_rec_location[i] == "nyingine", d$gift_rec$gift_rec_location_other[i], d$gift_rec$gift_rec_location[i])  
      di$subtype = ifelse(d$gift_rec$gift_rec_source[i] == "nyingine", d$gift_rec$gift_rec_source_other[i], d$gift_rec$gift_rec_source[i])
      di$shehia = d$gift_rec$shehia[i]
      di$by = d$gift_rec$"_submission__submitted_by"[i]
      di$julian = d$gift_rec$julian[i]
      di$date = d$gift_rec$`_submission__submission_time`[i]
      v <- rbind(v, di)
    }
  }
  
  # Gifts Out 
  
  for(i in 1:nrow(d$gift_give)){
    types <-str_split(d$gift_give$gift_give_type[i], " ")[[1]]
    for(j in 1:length(types)){
      di <- v2
      di$hhID = d$gift_give$hhID[i]
      di$type = ifelse(types[j] == "nyingine", d$gift_give$gift_give_type_other[i], types[j])
      di$main = "gift_give"
      di$value = d$gift_give$gift_give_value[i]/length(types)
      di$location = ifelse(d$gift_give$gift_give_location[i] == "nyingine", d$gift_give$gift_give_location_other[i], d$gift_give$gift_give_location[i])  
      di$period = d$gift_give$period[i]
      di$subtype = ifelse(d$gift_give$gift_give_source[i] == "nyingine", d$gift_give$gift_give_source_other[i], d$gift_give$gift_give_source[i])  
      di$shehia = d$gift_give$shehia[i]
      di$by = d$gift_give$"_submission__submitted_by"[i]
      di$julian = d$gift_give$julian[i]
      di$date = d$gift_give$`_submission__submission_time`[i]
      v <- rbind(v, di)
    }
  }
  
  
  
  
  
  # Credit fixes
  
  d$debt$creditor_amout_returned<-  ifelse(is.na(d$debt$creditor_amout_returned), 0, d$debt$creditor_amout_returned) 
  d$debt$creditor_amount_lent<-  ifelse(is.na(d$debt$creditor_amount_lent), 0, d$debt$creditor_amount_lent) 
  d$debt$debtor_amount_borrowed<-  ifelse(is.na(d$debt$debtor_amount_borrowed), 0, d$debt$debtor_amount_borrowed) 
  d$debt$debtor_amount_returned<-  ifelse(is.na(d$debt$debtor_amount_returned), 0, d$debt$debtor_amount_returned) 
  
  # Credit Amount Returned
  
  
  for(i in 1:nrow(d$debt)){
    di <- v2
    if(as.numeric(d$debt$creditor_amout_returned[i]) > 0){
      di$hhID = d$debt$hhID[i]
      di$type = NA
      di$main = "credit_returned"
      di$value = d$debt$creditor_amout_returned[i]
      di$period = d$debt$period[i]
      di$shehia = d$debt$shehia[i]
      di$by = d$debt$"_submitted_by"[i]
      di$julian = d$debt$julian[i]
      di$date = d$debt$`_submission_time`[i]
      
      v <- rbind(v, di)
    }#end if
  }
  
  
  # Credit Amount Returned
  
  
  for(i in 1:nrow(d$debt)){
    di <- v2
    if(as.numeric(d$debt$creditor_amount_lent[i]) > 0){
      di$hhID = d$debt$hhID[i]
      di$type = NA
      di$main = "credit_lent"
      di$value = d$debt$creditor_amount_lent[i]
      di$period = d$debt$period[i]
      di$shehia = d$debt$shehia[i]
      di$by = d$debt$"_submitted_by"[i]
      di$julian = d$debt$julian[i]
      di$date = d$debt$`_submission_time`[i]
      v <- rbind(v, di)
    }#end if
  }
  
  
  # Credit Amount Returned
  
  
  for(i in 1:nrow(d$debt)){
    di <- v2
    if(as.numeric(d$debt$debtor_amount_returned[i]) > 0){
      di$hhID = d$debt$hhID[i]
      di$type = NA
      di$main = "debt_returned"
      di$value = d$debt$debtor_amount_returned[i]
      di$period = d$debt$period[i]
      di$shehia = d$debt$shehia[i]
      di$by = d$debt$"_submitted_by"[i]
      di$julian = d$debt$julian[i]
      di$date = d$debt$`_submission_time`[i]
      v <- rbind(v, di)
    }#end if
  }
  
  
  
  for(i in 1:nrow(d$debt)){
    di <- v2
    if(as.numeric(d$debt$debtor_amount_borrowed[i]) > 0){
      di$hhID = d$debt$hhID[i]
      di$type = NA
      di$main = "debt_borrowed"
      di$value = d$debt$debtor_amount_borrowed[i]
      di$period = d$debt$period[i]
      di$shehia = d$debt$shehia[i]
      di$by = d$debt$"_submitted_by"[i]
      di$julian = d$debt$julian[i]
      di$date = d$debt$`_submission_time`[i]
      v <- rbind(v, di)
    }#end if
  }
  
  # VSLA savings 
  
  
  # Savings saved
  d$vsla$vsla_contribution<-  ifelse(is.na(d$vsla$vsla_contribution), 0, d$vsla$vsla_contribution) 
  d$vsla$vsla_withdraw<-  ifelse(is.na(d$vsla$vsla_withdraw), 0, d$vsla$vsla_withdraw) 
  
  
  
  for(i in 1:nrow(d$vsla)){
    di <- v2
    if(as.numeric(d$vsla$vsla_contribution [i]) > 0){
      di$hhID = d$vsla$hhID[i]
      di$type = NA
      di$main = "vsla_save"
      di$value = d$vsla$vsla_contribution[i]
      di$period = d$vsla$period[i]
      di$shehia = d$vsla$shehia[i]
      di$by = d$vsla$"_submitted_by"[i]
      di$julian = d$vsla$julian[i]
      di$date = d$vsla$`_submission_time`[i]
      
      v <- rbind(v, di)
    }#end if
  }
  
  
  # VSLA withdraw
  
  
  for(i in 1:nrow(d$vsla)){
    di <- v2
    if(as.numeric(d$vsla$vsla_withdraw [i]) > 0){
      di$hhID = d$vsla$hhID[i]
      di$type = NA
      di$main = "vsla_withdraw"
      di$value = d$vsla$vsla_withdraw[i]
      di$period = d$vsla$period[i]
      di$shehia = d$vsla$shehia[i]
      di$by = d$vsla$"_submitted_by"[i]
      di$julian = d$vsla$julian[i]
      di$date = d$vsla$`_submission_time`[i]
      
      v <- rbind(v, di)
    }#end if
  }
  
  
  # Savings saved
  d$savings$withdraw_money<-  ifelse(is.na(d$savings$withdraw_money), 0, d$savings$withdraw_money) 
  d$savings$store_money<-  ifelse(is.na(d$savings$store_money), 0, d$savings$store_money) 
  
  
  for(i in 1:nrow(d$savings)){
    di <- v2
    if(as.numeric(d$savings$store_money[i]) > 0){
      di$hhID = d$savings$hhID[i]
      di$type = NA
      di$main = "saving_save"
      di$value = d$savings$store_money[i]
      di$period = d$savings$period[i]
      di$shehia = d$savings$shehia[i]
      di$by = d$savings$"_submitted_by"[i]
      di$julian = d$savings$julian[i]
      di$date = d$savings$`_submission_time`[i]
  
      v <- rbind(v, di)
    }#end if
  }
  
  
  # Savings saved
  
  for(i in 1:nrow(d$savings)){
    di <- v2
    if(as.numeric(d$savings$withdraw_money[i]) > 0){
      di$hhID = d$savings$hhID[i]
      di$type = NA
      di$main = "saving_withdraw"
      di$value = d$savings$store_money[i]
      di$period = d$savings$period[i]
      di$shehia = d$savings$shehia[i]
      di$by = d$savings$"_submitted_by"[i]
      di$julian = d$savings$julian[i]
      di$date = d$savings$`_submission_time`[i]
      
      v <- rbind(v, di)
    }#end if
  }
  
  
  v$value <- as.numeric(v$value)
  
  
  ############################################################################
  ################ DROP OUT CLOVES SOLD ######################################
  
  
  a<-which((v$main == "ag_sold" & v$type == "karafu") & v$period==1)
  v$value[a] <- 0
  
  
  ############################################################################
  ################# Calculate household Income ###############################
  #Seperate out fishing
  v$main[v$type == "uvuvi"] <- "uvuvi"
  v$main[v$type == "fundi"] <- "fundi"
  
  
  dl <- list(flow = v,
             hours=h,
             revenue = c("inc", "wage", "self", "animal_sold", "slaughtered","ag_sold",
                         "ag_sub", "forest_sold", "forest_sub", "fundi", "uvuvi", "sold", "rent", "milk"),
             cash_in = c("wage", "self", "fundi", "uvuvi", "ag_sold", "animal_sold", "forest_sold", "rent", "sold"),
             sub_in = c("ag_sub", "slaughtered", "forest_sub"),
             cred_in = c("store_credit", "debt_borrowed", "credit_returned"),
             sav_in = c("vsla_withdraw", "saving_withdraw"),
             gift_in = c("gift_rec"),
             gift_out = c("gift_give"),
             sav_out = c("saving_save", "vsla_save"),
             cred_out = c("credit_lent", "debt_returned"),
             price_errors = unique(correct_prices),
             prices = dpl
  )