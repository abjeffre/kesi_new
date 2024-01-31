#############################################################
######### CLEANING ##########################################
#setwd("C:/Users/jeffr/OneDrive/Documents/Risk/")
if(MAKE != TRUE) source("code/script/buildIds.r")

#######################################################
######### HOUSEHOLD LOCATION FIXES  ###################

ind<-which(d$main$hhm_hhh == "Mbwana Ali Suwai")
d$main$shehia[ind] <- "kipange"

ind<-which(d$main$hhm_hhh == "Kahlid Tuil Abdallah")
d$main$shehia[ind] <- "kifundi"
#

###################################################### 
################## CLEANING DETAILS ##################

#####################################################
################ CLEANING AG ########################

d$hhm_details$hhID <- d$main$hh_ID[d$hhm_details$'_parent_index']
d$hhm_details$shehia <- d$main$shehia[d$hhm_details$'_parent_index']
d$hhm_details$julian <- d$main$julian[d$hhm_details$'_parent_index']
d$hhm_details$hhh <- d$main$hhm_hhh[d$hhm_details$'_parent_index']


##########################################################
################## CLEANING CHECKING #####################


d$hhm_check$hhID <- d$main$hh_ID[d$hhm_check$'_parent_index']
d$hhm_check$shehia <- d$main$shehia[d$hhm_check$'_parent_index']
d$hhm_check$julian <- d$main$julian[d$hhm_check$'_parent_index']
d$hhm_check$hhh <- d$main$hhm_hhh[d$hhm_check$'_parent_index']

#######################################################
################ CLEANING WAGES ########################


names(d)[which(names(d) == "wage_emp_sources")] <- "wage_emp_source"
d$wage_emp_source$wage_main<-ifelse(!is.na(d$wage_emp_source$wage_government_type_other), # Make diffrent types of gov employment a main.
                                    d$wage_emp_source$wage_government_type_other,d$wage_emp_source$wage_main)
d$wage_emp_source$hhID <- d$main$hh_ID[d$wage_emp_source$"_parent_index"]
d$wage_emp_source$shehia <- d$main$shehia[d$wage_emp_source$"_parent_index"]
d$wage_emp_source$julian <- d$main$julian[d$wage_emp_source$"_parent_index"]
d$wage_emp_source$hhh <- d$main$hhm_hhh[d$wage_emp_source$"_parent_index"]


#######################################################
################ CLEANING SELF ########################

temp <- select(d$self_emp_source, contains("/"))
cat <- c()

subcat <- rep(NA, nrow(d$self_emp_source))
subcat<-ifelse(!is.na(d$self_emp_source$self_duka),d$self_emp_source$self_duka, subcat) 
subcat<-ifelse(!is.na(d$self_emp_source$self_fundi),d$self_emp_source$self_duka, subcat) 
subcat<-ifelse(!is.na(d$self_emp_source$self_uchuuzaji),d$self_emp_source$self_uchuuzaji, subcat) 
subcat<-ifelse(!is.na(d$self_emp_source$self_fishing),d$self_emp_source$self_fishing, subcat) 
subcat<-ifelse(!is.na(d$self_emp_source$self_usafiri),d$self_emp_source$self_usafiri, subcat) 
subcat<-ifelse(!is.na(d$self_emp_source$self_food_business),d$self_emp_source$self_food_business, subcat) 
subcat<-ifelse(!is.na(d$self_emp_source$self_trading),d$self_emp_source$self_trading, subcat) 

df <- select(d$self_emp_source, !contains("/"))
df$self_sub <- subcat
df$hhID <- d$main$hh_ID[d$self_emp_source$"_parent_index"]
df$shehia <- d$main$shehia[d$self_emp_source$"_parent_index"]
df$hhh <- d$main$hhm_hhh[d$self_emp_source$"_parent_index"]
df$julian <- d$main$julian[d$self_emp_source$"_parent_index"]


d$self_emp_source <- df



#####################################################
################ CLEANING AG ########################

d$ag_crop$hhID <- d$main$hh_ID[d$ag_crop$'_parent_index']
d$ag_crop$shehia <- d$main$shehia[d$ag_crop$'_parent_index']
d$ag_crop$julian <- d$main$julian[d$ag_crop$'_parent_index']
d$ag_crop$hhh <- d$main$hhm_hhh[d$ag_crop$'_parent_index']

#emp source

d$ag_emp_source$hhID <- d$main$hh_ID[d$ag_emp_source$'_parent_index']
d$ag_emp_source$shehia <- d$main$shehia[d$ag_emp_source$'_parent_index']
d$ag_emp_source$julian <- d$main$julian[d$ag_emp_source$'_parent_index']
d$ag_emp_source$hhh <- d$main$hhm_hhh[d$ag_emp_source$'_parent_index']


##########################################################
################ CLEANING FORESTS ########################
d$forest_emp$hhID <- d$main$hh_ID[d$forest_emp$'_parent_index']
d$forest_emp$shehia <- d$main$shehia[d$forest_emp$'_parent_index']
d$forest_emp$julian <- d$main$julian[d$forest_emp$'_parent_index']
d$forest_emp$hhh <- d$main$hhm_hhh[d$forest_emp$'_parent_index']


# Forest sources
names(d)[which(names(d) == "forest_person_emp")] <- "forest_emp_source"

d$forest_emp_source$hhID <- d$main$hh_ID[d$forest_emp$"_parent_index"[d$forest_emp_source$"_parent_index"]]
d$forest_emp_source$julian <- d$main$julian[d$forest_emp$"_parent_index"[d$forest_emp_source$"_parent_index"]]
d$forest_emp_source$shehia <- d$main$shehia[d$forest_emp$"_parent_index"[d$forest_emp_source$"_parent_index"]]
d$forest_emp_source$hhh <- d$main$hhm_hhh[d$forest_emp$"_parent_index"[d$forest_emp_source$"_parent_index"]]
d$forest_emp_source$main <- d$forest_emp$job_forest[d$forest_emp_source$"_parent_index"] 



#############################################################
################ Cleaning Animals ###########################

d$animals_emp$hhID <- d$main$hh_ID[d$animals_emp$'_parent_index']
d$animals_emp$shehia <- d$main$shehia[d$animals_emp$'_parent_index']
d$animals_emp$julian <- d$main$julian[d$animals_emp$'_parent_index']
d$animals_emp$hhh <- d$main$hhm_hhh[d$animals_emp$'_parent_index']


# Animal sources
names(d)[which(names(d) == "animals_person_emp")] <- "animals_emp_source"
d$animals_emp_source$hhID <- d$main$hh_ID[d$animals_emp$"_parent_index"[d$animals_emp_source$"_parent_index"]]
d$animals_emp_source$shehia <- d$main$shehia[d$animals_emp$"_parent_index"[d$animals_emp_source$"_parent_index"]]
d$animals_emp_source$julian <- d$main$julian[d$animals_emp$"_parent_index"[d$animals_emp_source$"_parent_index"]]
d$animals_emp_source$main <- d$animals_emp$job_animals[d$animals_emp_source$"_parent_index"] 
d$animals_emp_source$hhh <- d$main$hhm_hhh[d$animals_emp$"_parent_index"[d$animals_emp_source$"_parent_index"]]

#######################################################################
#################### Cleaning Store Credit ############################

d$store_credit$hhID <- d$main$hh_ID[d$store_credit$'_parent_index']
d$store_credit$shehia <- d$main$shehia[d$store_credit$'_parent_index']
d$store_credit$julian <- d$main$julian[d$store_credit$'_parent_index']
d$store_credit$hhh <- d$main$hhm_hhh[d$store_credit$'_parent_index']

#######################################################################
#################### Cleaning gifts ###################################

d$gift_give$hhID <- d$main$hh_ID[d$gift_give$'_parent_index']
d$gift_give$julian <- d$main$julian[d$gift_give$'_parent_index']
d$gift_give$shehia <- d$main$shehia[d$gift_give$'_parent_index']
d$gift_give$hhh <- d$main$hhm_hhh[d$gift_give$'_parent_index']

d$gift_rec$hhID <- d$main$hh_ID[d$gift_rec$'_parent_index']
d$gift_rec$shehia <- d$main$shehia[d$gift_rec$'_parent_index']
d$gift_rec$julian <- d$main$julian[d$gift_rec$'_parent_index']
d$gift_rec$hhh <- d$main$hhm_hhh[d$gift_rec$'_parent_index']

#######################################################################
#################### Cleaning Debts ###################################

df <- data.frame(creditor = d$main$creditor,
                 creditor_amout_returned = as.numeric(d$main$creditor_amout_returned),
                 creditor_amount_lent= as.numeric(d$main$creditor_amount_lent),
                 debtor = d$main$debtor,
                 debtor_amount_returned=as.numeric(d$main$debtor_amount_returned),
                 debtor_amount_borrowed=as.numeric(d$main$debtor_amout_lent),
                 hhID = d$main$hh_ID,
                 shehia = d$main$shehia,
                 julian = d$main$julian,
                 hhh = d$main$hhm_hhh
                 )
df <- cbind(df, select(d$main, starts_with("_")))

d$debt <- df

#######################################################################
###################### CLEANING VSLA ###############################

df <- data.frame(vsla_membership = d$main$vsla_membership,
                 vsla_contribution= as.numeric(d$main$vsla_contribution),
                 vsla_withdraw= as.numeric(d$main$vsla_withdraw),
                 hhID = d$main$hh_ID,
                 julian = d$main$julian,
                 shehia= d$main$shehia,
                 hhh = d$main$hhm_hhh
)
df <- cbind(df, select(d$main, starts_with("_")))
d$vsla <- df



####################################################################
#################### CLEANING SAVINGS #################################

df <- data.frame(bank_account = d$main$bank_account,
                 sim_account = d$main$sim_account,
                 store_money= as.numeric(d$main$store_money),
                 withdraw_money= as.numeric(d$main$withdraw_money),
                 hhID = d$main$hh_ID,
                 julian = d$main$julian,
                 shehia = d$main$shehia,
                 hhh = d$main$hhm_hhh
)
df <- cbind(df, select(d$main, starts_with("_")))
d$savings <- df



#################################################################
############ THEFT ##############################################

df <- data.frame(theft =  d$main$theft,
                  item = d$main$theft_item,
                  other = d$main$theft_other,
                  value = d$main$theft_value,
                  note = d$main$theft_note,
                  hhID = d$main$hh_ID,
                  julian = d$main$julian,
                  shehia = d$main$shehia,
                 hhh = d$main$hhm_hhh)
df <- cbind(df, select(d$main, starts_with("_")))
d$theft <- df


############################################################
#################### Consumption ###########################

d$con <- select(d$main, starts_with("con"))
d$con <- cbind(d$con, select(d$main, starts_with("_")))
d$con$hhID <- d$main$hh_ID
d$con$julian <- d$main$julian
d$con$shehia <- d$main$shehia
d$con$hhh <- d$main$hhm_hhh



#############################################################
################ CLeaning Sales #############################

d$sold <- db2$sold_emp
d$sold$hhID <- d$main$hh_ID[d$sold$'_parent_index']
d$sold$shehia <- d$main$shehia[d$sold$'_parent_index']
d$sold$julian <- d$main$julian[d$sold$'_parent_index']
d$sold$hhh <- d$main$hhm_hhh[d$sold$'_parent_index']


#############################################################
################ CLeaning Rent #############################

d$rent <- db2$rent_emp
d$rent$hhID <- d$main$hh_ID[d$rent$'_parent_index']
d$rent$shehia <- d$main$shehia[d$rent$'_parent_index']
d$rent$julian <- d$main$julian[d$rent$'_parent_index']
d$rent$hhh <- d$main$hhm_hhh[d$rent$'_parent_index']


##############################################################
################## CLEANING MWANI #############################

d$mwani$hhh <- NA
d$mwani$julian <- NA
d$mwani$period <- NA
d$mwani$shehia <- NA
d$mwani$hhID <- NA
for(i in 1:nrow(d$mwani)){
  id =d$mwani$`_submission__uuid`[i]
  ind=which(d$main$`_uuid` == id)
  d$mwani$`_parent_index`[i] <- d$main$`_index`[ind]
  d$mwani$hhID[i] <- d$main$hh_ID[ind]
  d$mwani$shehia[i] <- d$main$shehia[ind]
  d$mwani$julian[i] <- d$main$julian[ind]
  d$mwani$hhh[i] <- d$main$hhm_hhh[ind]
}

###############################################################
################# CLEANING SAFARI #############################


d$safari$hhh <- NA
d$safari$julian <- NA
d$safari$period <- NA
d$safari$shehia <- NA
d$safari$hhID <- NA
for(i in 1:nrow(d$safari)){
  id =d$safari$`_submission__uuid`[i]
  ind=which(d$main$`_uuid` == id)
  d$safari$`_parent_index`[i] <- d$main$`_index`[ind]
  d$safari$hhID[i] <- d$main$hh_ID[ind]
  d$safari$shehia[i] <- d$main$shehia[ind]
  d$safari$julian[i] <- d$main$julian[ind]
  d$safari$hhh[i] <- d$main$hhm_hhh[ind]
}



###########################################################
############# PERIOD ######################################

for (i in names(d)){
  if("julian" %in% names(d[[i]])){
    d[[i]]$period <- as.numeric(ceiling(d[[i]]$julian/14))
  }
}



############################################################
############## WEALTH ######################################

temp <- names(d$main)
col<-c(which(temp=="radio_num"):which(temp =="axes_val"))
col <-c(col, which(temp=="donkeys_num"), which(temp=="donkeys_val"), 
        which(temp=="michungwa_num"), which(temp=="michungwa_val"),
        which(temp=="mipapai_num"), which(temp=="mipapai_val"),
        which(temp=="mibalungi_num"), which(temp=="mibalungi_val"),
        which(temp=="donkeys_num"), which(temp=="mipasheni_val"),
        which(temp=="mipasheni_num"), which(temp=="donkeys_num"))

temp<-which(grepl("_val", names(d$main)))
value_cols<-temp[-which(names(d$main)[temp] %in% c("_validation_status", "theft_value"))]
# temp<-which(grepl("_num", names(d$main)))
# names_cols<-temp[-which(names(d$main)[temp] %in% c("gift_rec_num", "gift_give_num", "work_trip_num", "ag_help_num", "ag_labor_num", "animals_help_num", "animals_labor_num"))]

# build DF
sum=sum(d$main[1, value_cols], na.rm =T)
temp=data.frame(total_weatlh = sum,
                hhid = d$main$hh_ID[1],
                shehia = d$main$shehia[1],
                period = d$main$period[1])
d$wealth<-cbind(temp, d$main[1, col])

for(i in 2:nrow(d$main)){
  sum=sum(d$main[i, value_cols], na.rm =T)
  if(sum > i){
    temp=data.frame(total_weatlh = sum,
                    hhid = d$main$hh_ID[i],
                    shehia = d$main$shehia[i],
                    period = d$main$period[i])
    temp2<- cbind(temp, d$main[i, col])
    d$wealth<-rbind(d$wealth, temp2 )
  }
}
# Wealth is going to be transformed into a long-form dataset


# wealth_var <-names(d$main)[cols]
# $wealth <- d$main[1:nfirst, wealth_var] 
# d$wealth <- cbind(d$wealth, select(d$main[1:nfirst, ], starts_with("_")))
# d$wealth$hhID <- d$main$hh_ID[1:nfirst]
# d$wealth$julian <- d$main$julian[1:nfirst]
# d$wealth$shehia <- d$main$shehia[1:nfirst]
# d$wealth$hhh <- d$main$hhm_hhh[1:nfirst]


##############################################################
############## CLEANING CHECK ################################

d$hhm_check$julian <- NA
d$hhm_check$period <- NA
d$hhm_check$hhh <- NA
d$hhm_check$hhID <- NA


for(i in 1:nrow(d$hhm_check)){
  d$hhm_check$julian[i] <- d$main$julian[which(d$main$"_uuid" == d$hhm_check$"_submission__uuid"[i])]
  d$hhm_check$period[i] <- d$main$period[which(d$main$"_uuid" == d$hhm_check$"_submission__uuid"[i])]
  d$hhm_check$hhID[i] <- d$main$hh_ID[which(d$main$"_uuid" == d$hhm_check$"_submission__uuid"[i])]
  d$hhm_check$hhh[i] <- d$main$hhm_hhh[which(d$main$"_uuid" == d$hhm_check$"_submission__uuid"[i])]
}


##############################################################
############## CLEANING Left ################################

d$left$julian <- NA
d$left$period <- NA
d$left$hhh <- NA
d$left$hhID <- NA


for(i in 1:nrow(d$left)){
  d$left$julian[i] <- d$main$julian[which(d$main$"_uuid" == d$left$"_submission__uuid"[i])]
  d$left$period[i] <- d$main$period[which(d$main$"_uuid" == d$left$"_submission__uuid"[i])]
  d$left$hhID[i] <- d$main$hh_ID[which(d$main$"_uuid" == d$left$"_submission__uuid"[i])]
  d$left$hhh[i] <- d$main$hhm_hhh[which(d$main$"_uuid" == d$left$"_submission__uuid"[i])]
}




############################################################
############# MAKE EDUCATIONAL FACTOR ######################

temp <- d$hhm_details$hhm_education
temp <- as.factor(temp)

factor(temp, ordered = TRUE, levels = c("none", "chekechea", "s1", "s2", "s3", "s4", "s5", "s6", "s7", 
                                          "f1", "f2", "f3", "f4", "f5", "f6", "certificate", "chuo ki kuu", "nyingine"))

d$hhm_details$hhm_education_factor <- temp
############################################################
############## SHORTENING NAMES ############################


a<-which(grepl("_emp_", names(d)))
nms<-names(d)[a]
nmsl<-lapply(nms, function(x) strsplit(x, "[_]")[[1]][1])
nmsl<-lapply(nmsl, unlist)
names(d)[a] <- nmsl
#names(d)[24] <- "wage_other"


#wages
temp <-select(d$wage, starts_with('_submission'))
a<-which(names(d$wage)=="number_source_wage")
names(d$wage)[a] <- "number"
a<-which(names(d$wage)=="_parent_table_name")
names(d$wage)[a] <- "table"
nms <- names(d$wage)
nmsl<-lapply(nms, function(x) strsplit(x, "[_]")[[1]][])
for(i in 1:length(nmsl)){
  nmsl[[i]]=ifelse(nmsl[[i]][1] == "wage", paste(unlist(nmsl[[i]][-1]), collapse = "_"), paste(unlist(nmsl[[i]]), collapse = "_"))   
  nmsl[[i]]=sub("__", "_", nmsl[[i]])
}
nmsl<-unlist(nmsl)
ind<-which(is.na(nmsl))
nmsl[ind] <- nms[ind]
names(d$wage) <- nmsl
d$wage<-cbind(d$wage, temp)
a<-which(names(d$wage)=="_submission__submitted_by")
names(d$wage)[a] <- "by"



#self
temp <-select(d$self, starts_with('_submission'))
a<-which(names(d$self)=="number_source_self")
names(d$self)[a] <- "number"
a<-which(names(d$self)=="self_fishing_crew")
names(d$self)[a] <- "crew"
a<-which(names(d$self)=="_parent_table_name")
names(d$self)[a] <- "table"
nms <- names(d$self)
nmsl<-lapply(nms, function(x) strsplit(x, "[_]")[[1]][])
for(i in 1:length(nmsl)){
  nmsl[[i]]=ifelse(nmsl[[i]][1] == "self", paste(unlist(nmsl[[i]][-1]), collapse = "_"), paste(unlist(nmsl[[i]]), collapse = "_"))   
  nmsl[[i]]=sub("__", "_", nmsl[[i]])
}
nmsl<-unlist(nmsl)
ind<-which(is.na(nmsl))
nmsl[ind] <- nms[ind]
names(d$self) <- nmsl
d$self<-cbind(d$self, temp)
a<-which(names(d$self)=="_submission__submitted_by")
names(d$self)[a] <- "by"

#ag_crop
temp <-select(d$ag_crop, starts_with('_submission'))
a<-which(names(d$ag_crop)=="number_source_ag")
names(d$ag_crop)[a] <- "number"
a<-which(names(d$ag_crop)=="ag_unit_other")
names(d$ag_crop)[a] <- "unitother"
a<-which(names(d$ag_crop)=="ag_num_other")
names(d$ag_crop)[a] <- "numother"
a<-which(names(d$ag_crop)=="_parent_table_name")
names(d$ag_crop)[a] <- "table"
nms <- names(d$ag_crop)
nmsl<-lapply(nms, function(x) strsplit(x, "[_]")[[1]][])
for(i in 1:length(nmsl)){
  nmsl[[i]]=ifelse(nmsl[[i]][1] == "ag", paste(unlist(nmsl[[i]][-1]), collapse = "_"), paste(unlist(nmsl[[i]]), collapse = "_"))   
  nmsl[[i]]=sub("__", "_", nmsl[[i]])
}
nmsl<-unlist(nmsl)
ind<-which(is.na(nmsl))
nmsl[ind] <- nms[ind]
names(d$ag_crop) <- nmsl
d$ag_crop<-cbind(d$ag_crop, temp)
a<-which(names(d$ag_crop)=="_submission__submitted_by")
names(d$ag_crop)[a] <- "by"
d$ag_crop$unit <- ifelse(d$ag_crop$unit == "nyingine",d$ag_crop$unitother, d$ag_crop$unit)
d$ag_crop$type <- ifelse(d$ag_crop$type == "nyingine",d$ag_crop$other, d$ag_crop$type)
d$ag_crop$unit[grepl("kicha", tolower(d$ag_crop$unit))] <- "kicha"



#forest
a<-which(names(d)=="forest_emp")
names(d)[a] <- "forest_crop"
temp <-select(d$forest_crop, starts_with('_submission'))
a<-which(names(d$forest_crop)=="job_forest_other")
names(d$forest_crop)[a] <- "typeother"
a<-which(names(d$forest_crop)=="job_forest")
names(d$forest_crop)[a] <- "forest_type"
a<-which(names(d$forest_crop)=="num_forest")
names(d$forest_crop)[a] <- "forest_num"
a<-which(names(d$forest_crop)=="forest_unit_other")
names(d$forest_crop)[a] <- "unitother"
a<-which(names(d$forest_crop)=="forest_num_other")
names(d$forest_crop)[a] <- "numother"
a<-which(names(d$forest_crop)=="_parent_table_name")
names(d$forest_crop)[a] <- "table"
a<-which(names(d$forest_crop)=="_submission__submitted_by")
names(d$forest_crop)[a] <- "by"
nms <- names(d$forest_crop)
nmsl<-lapply(nms, function(x) strsplit(x, "[_]")[[1]][])
for(i in 1:length(nmsl)){
  nmsl[[i]]=ifelse(nmsl[[i]][1] == "forest", paste(unlist(nmsl[[i]][-1]), collapse = "_"), paste(unlist(nmsl[[i]]), collapse = "_"))   
  nmsl[[i]]=sub("__", "_", nmsl[[i]])
}
nmsl<-unlist(nmsl)
ind<-which(is.na(nmsl))
nmsl[ind] <- nms[ind]
names(d$forest_crop) <- nmsl
d$forest_crop<-cbind(d$forest_crop, temp)

#Animals
a<-which(names(d)=="animals_emp")
names(d)[a] <- "animal"

a<-which(names(d)=="animals")
names(d)[a] <- "animal_emp"

temp <-select(d$animal, starts_with('_submission'))
a<-which(names(d$animal)=="job_animals_other")
names(d$animal)[a] <- "typeother"
a<-which(names(d$animal)=="job_animals")
names(d$animal)[a] <- "animals_type"
a<-which(names(d$animal)=="num_animals")
names(d$animal)[a] <- "animals_num"
a<-which(names(d$animal)=="animals_meat_sold")
names(d$animal)[a] <- "meatsold"
a<-which(names(d$animal)=="animals_meat_unit")
names(d$animal)[a] <- "meatunit"
a<-which(names(d$animal)=="animals_meat_unit_other")
names(d$animal)[a] <- "meatunitother"
a<-which(names(d$animal)=="animals_meat_price")
names(d$animal)[a] <- "meatprice"
a<-which(names(d$animal)=="animals_num_other")
names(d$animal)[a] <- "numother"
a<-which(names(d$animal)=="_parent_table_name")
names(d$animal)[a] <- "table"
a<-which(names(d$animal)=="_submission__submitted_by")
names(d$animal)[a] <- "by"
nms <- names(d$animal)
nmsl<-lapply(nms, function(x) strsplit(x, "[_]")[[1]][])
for(i in 1:length(nmsl)){
  nmsl[[i]]=ifelse(nmsl[[i]][1] == "animals", paste(unlist(nmsl[[i]][-1]), collapse = "_"), paste(unlist(nmsl[[i]]), collapse = "_"))   
  nmsl[[i]]=sub("__", "_", nmsl[[i]])
}
nmsl<-unlist(nmsl)
ind<-which(is.na(nmsl))
nmsl[ind] <- nms[ind]
names(d$animal) <- nmsl
d$animal<-cbind(d$animal, temp)

###########################################################
############# TO NUMERIC ##################################

d$wage$wage_hours<-as.numeric(d$wage$hours)
d$wage$wage_income<- as.numeric(d$wage$income)

d$self$crew <- as.numeric(d$self$crew)
d$self$hours <-as.numeric(d$self$hours)
d$self$costs <-as.numeric(d$self$costs)
d$self$profits <-as.numeric(d$self$profits)

d$ag_crop$harvest <- as.numeric(d$ag_crop$harvest)
d$ag_crop$sold <- as.numeric(d$ag_crop$sold)
d$ag_crop$price <- as.numeric(d$ag_crop$price)  

d$animal$sold<- as.numeric(d$animal$sold)
d$animal$price<- as.numeric(d$animal$price)
d$animal$slaughtered<- as.numeric(d$animal$slaughtered)
d$animal$meatsold<- as.numeric(d$animal$meatsold)
d$animal$meatprice<- as.numeric(d$animal$meatprice)


d$forest_crop$harvest <- as.numeric(d$forest_crop$harvest)
d$forest_crop$sold <- as.numeric(d$forest_crop$sold)
d$forest_crop$price <- as.numeric(d$forest_crop$price)
d$forest_crop$unit <- ifelse(d$forest_crop$unit == "nyingine", d$forest_crop$unitother, d$forest_crop$unit)

d$ag$ag_plant_hours <- as.numeric(d$ag$ag_plant_hours)
d$ag$ag_weed_hours <- as.numeric(d$ag$ag_weed_hours)
d$ag$ag_harvest_hours <- as.numeric(d$ag$ag_harvest_hours)


d$con[, which(names(d$con) == "con_food"):which(names(d$con) == "con_other")] <- apply(d$con[, which(names(d$con) == "con_food"):which(names(d$con) == "con_other")],2, as.numeric)


############################################################
############## CLEAN MAIN INDEX ############################

d$main$"_index"[67:nrow(d$main)]<- 67:nrow(d$main)


###########################################################
################ CLEAN MISPELLINGS ########################
# Jeff
# 6=2022_06_11


library(dplyr)
names(d$wage)
# d$wage <- d$wage[-which(names(d$wage)=="submission")]
# d$self <- d$self[-which(names(d$self)=="submission")]
# d$forest_crop <- d$forest_crop[-which(names(d$forest_crop)=="submission")]


############################################################
#################### DROP THE MILK ERRORS ##################

d$animal<-d$animal[,-which(names(d$animal)=="milk")]

###########################################################
############# VALUE CLEANS ################################

#Personal Correspondance with Bakari
#ag 
d$ag_crop[1:60, "sold"][d$ag_crop$type[1:60] == "muhogo"] <- 0
d$ag_crop[1:60, "sold"][d$ag_crop$type[1:60] == "ndizi" & d$ag_crop$sold[1:60] == 30000] <- 0
d$ag_crop[1:60, "sold"][d$ag_crop$type[1:60] == "nazi" & d$ag_crop$sold[1:60] == 28000] <- 0
d$ag_crop[1:60, "sold"][d$ag_crop$type[1:60] == "nazi" & d$ag_crop$sold[1:60] == 125000] <- 0
d$ag_crop[1:60, "sold"][d$ag_crop$type[1:60] == "mashelisheli" & d$ag_crop$sold[1:60] == 8000] <- 0
d$ag_crop[1:60, "sold"][d$ag_crop$type[1:60] == "viazi" & d$ag_crop$sold[1:60] == 70000] <- 0
d$ag_crop[1:60, "unit"][d$ag_crop$type[1:60] == "maembe" & d$ag_crop$unit[1:60] == "gari_ya_gnombe"] <- "kipande"
#forest

d$forest_crop[1:60, "sold"][d$forest_crop$type[1:60] == "makuti"] <- 0
d$forest_crop[1:60, "sold"][d$forest_crop$type[1:60] == "mbao"] <- 0
d$forest_crop[1:60, "sold"][d$forest_crop$type[1:60] == "asali"] <- 0
d$forest_crop[1:60, "sold"][d$forest_crop$type[1:60] == "pegi"] <- 0
d$forest_crop[1:60, "sold"][d$forest_crop$type[1:60] == "kuni"] <- 0

d$forest_crop$type[d$forest_crop$unit == "firewood"] <- "kuni"

d$forest_crop$price[452] <- 750
d$forest_crop$sold[452] <- 8
d$forest_crop$sold[473] <- 150
d$forest_crop$sold[488] <- 120
d$forest_crop$price[488] <- 7000
d$ag_crop[1510,"harvest"] <- 1
d$ag_crop[1510,"sold"] <- 1
d$ag_crop[1510,"price"] <- 60000
d$self$profits[663] <- NA
d$forest_crop$harvest[917] <- 7
d$ag_crop[d$ag_crop$"_index" == 3879, "harvest"] <- 20
d$ag_crop[d$ag_crop$"_index" == 3860, "price"] <- 12680

d$self$profits[which(d$self$profits ==99)] <- NA

d$wage$income[d$wage$"_index"==310] <- NA
d$ag_crop$harvest[d$ag_crop$"_index"==4495 ] <- 11 # CHECK WITH ABUUU
d$ag_crop$harvest[d$ag_crop$"_index"==4484 ] <- 11 # CHECK WITH ABUUU
d$ag_crop$sold[d$ag_crop$"_index"==4524 ] <- .62 # CHECK WITH ABUUU
d$ag_crop$harvest[d$ag_crop$"_index"==4659 ] <- 3 # CHECK WITH BAKARI
d$ag_crop$harvest[d$ag_crop$"_index"==5093]  <- 9 # Check with ABUU
d$forest_crop$harvest[d$forest_crop$"_index"==657]  <- 5 # Check with ABUU
d$forest_crop$sold[d$forest_crop$"_index"==936]  <- 3 # Check with ABUU
d$forest_crop$sold[d$forest_crop$"_index"==1035 ]  <- 3 # Check with BAKARI
d$forest_crop$sold[d$forest_crop$"_index"==1322 ]  <- 40 # Check with BAKARI

d$forest_crop$harvest[d$forest_crop$"_index"== 164] = d$forest_crop$sold[d$forest_crop$"_index"== 164] 
d$forest_crop$harvest[d$forest_crop$"_index"== 201] = d$forest_crop$sold[d$forest_crop$"_index"== 201] 
d$forest_crop$harvest[d$forest_crop$"_index"== 282] = d$forest_crop$sold[d$forest_crop$"_index"== 282] 
d$forest_crop$harvest[d$forest_crop$"_index"== 308] = d$forest_crop$sold[d$forest_crop$"_index"== 308] 
d$forest_crop$harvest[d$forest_crop$"_index"== 387] = d$forest_crop$sold[d$forest_crop$"_index"== 387] 
d$forest_crop$harvest[d$forest_crop$"_index"== 459] = d$forest_crop$sold[d$forest_crop$"_index"== 459] 
d$forest_crop$harvest[d$forest_crop$"_index"== 2677] = 6


d$main$mwani_price[d$main$`_index` == 1541] = 2200
d$main$mwani_price[d$main$`_index` == 1659] = 1000
d$main$mwani_price[d$main$`_index` == 1660] = 2200
d$main$mwani_sold_amount[d$main$`_index` == 1690] = 15
d$main$mwani_price[d$main$`_index` == 1805] = 1000
d$main$mwani_price[d$main$`_index` == 1937] = 1000
d$main$mwani_price[d$main$`_index` == 1937] = 1000
d$main$mwani_price[d$main$`_index` == 2074] = 1000
d$self$hours[d$self$"_index" == 67] = 12*14
d$self$hours[d$self$"_index" == 5] = 10*12
d$self$costs[d$self$"_index" == 67] = 279600 
d$self$costs[d$self$"_index" == 68] = 279600 
d$self$costs[d$self$"_index" == 960] = 45000 
d$self$profits[d$self$"_index" == 2522] = 140000
d$self$profits[d$self$"_index" == 2496] = 30000
d$self$profits[d$self$"_index" == 826] = 90000 # THESE MIGHT BE BAD CHANGES
d$self$profits[d$self$"_index" == 827] = 60000 # THESE MIGHT BE BAD CHANGES
d$self$profits[d$self$"_index" == 1616] = 70000 # THESE MIGHT BE BAD CHANGES
d$ag_crop$unit[d$ag_crop$"_index" == 9511] <- "kipande"
d$ag_crop$price[d$ag_crop$"_index" == 10013] <- 10000
d$ag_crop$price[d$ag_crop$"_index" == 17881] <- 10000

####################################################
################## WAGE CLEANS #####################

# FOR EVERYONE WHO DOES WAGE LABOR REMEMBER TO CHECK
#
d$wage$income[d$wage$"_index"==3] <- 150000
d$wage$income[d$wage$"_index"==3] <- 150000
d$wage$income[d$wage$"_index"==146] <- 150000
d$wage$income[d$wage$"_index"==2379] <- 450000 
# These are all people who hid their salaries at the begninig and later disclosed!
d$wage$income[d$wage$names == "Siti Ali Salum" & is.na(d$wage$income)] == 200000
d$wage$income[d$wage$names == "Ismail Ali Juma" & is.na(d$wage$income)] == 500000
d$wage$income[d$wage$names == "Bimkubwa Salim Ali" & is.na(d$wage$income)] == 150000
d$wage$income[d$wage$names == "Atka Juma Ali" & is.na(d$wage$income)] == 500000
d$wage$income[d$wage$names == "Mbwana Khalifa Mbwana" & is.na(d$wage$income)] == 750000
d$wage$income[d$wage$names == "Suleiman Hamad Suleiman" & is.na(d$wage$income)] == 750000
d$wage$income[d$wage$names == "Aisha Said Mkadam" & d$wage$income >0 & d$wage$income < 200000] == 750000



d$wage$income[d$wage$"_index"==59] <- 150000
d$wage$income[d$wage$"_index"==287] <- 300000
d$wage$income[d$wage$"_index"==311] <- 300000
d$wage$income[d$wage$"_index"==46] <- 100000
d$wage$income[d$wage$"_index"==428] <- 400000
d$wage$income[d$wage$"_index"==145] <- 15000
d$wage$income[d$wage$"_index"==217] <- 40000
d$wage$income[d$wage$"_index"==3407] <- 48000
d$wage$income[d$wage$"_index"==3436] <- 48000


# DOUBLE CHECK

###################################################
############## SELF CLEANS ########################

d$self$profits[d$self$"_index"==988] <- 4500
d$self$profits[d$self$"_index"==431] <- 2500
d$self$profits[d$self$"_index"==79] <- 21000
d$self$profits[d$self$"_index"==80] <- 21000
d$self$profits[d$self$"_index"==157] <- 35000
d$self$profits[d$self$"_index"==794] <- 35000
d$self$profits[d$self$"_index"==677] <- 4200
d$self$profits[d$self$"_index"==997] <- 4200
d$self$profits[d$self$"_index"==2995] <- 45000
d$self$profits[d$self$"_index"==395] <- 240000 
d$self$hours[d$self$"_index"==2980] <- 42.5
d$self$profits[d$self$"_index"==55] <- 5000
d$self$profits[d$self$"_index"==560] <- 15000
d$self$profits[d$self$"_index"==4601] <- 100000
d$self$profits[d$self$"_index"==4687] <- NA
d$self$hours[d$self$"_index"==4687] <- 90



d$self$profits[d$self$"_index"==5704] <- 84000
d$self$main[d$self$"_index"==5704] <- "biashara ya kilimo"




####################################################
################## WAGES ###########################

d$wage$income[d$wage$"_index"==1823] <- 50000

####################################################
################### AG CLEANING ####################

# After reviewing the data it appears that Abuu was grossly inflating the amount time spent harvesting 

d$ag$ag_plant_hours[d$ag$`_submission__submitted_by` == "sijuitaponi" & d$ag$period %in%  1:26] <- d$ag$ag_plant_hours[d$ag$`_submission__submitted_by` == "sijuitaponi" & d$ag$period %in%  1:26] /3 


#####################################################
############### AG CROP CLEAN #######################


unique(d$ag_crop$type)
d$ag_crop$type <- tolower(d$ag_crop$type)
#d$ag_crop$type[grepl("hogo", tolower(d$ag_crop$type))] <- "mboga_ya_muhogo" 
d$ag_crop$type[d$ag_crop$type=="malimau"] <-"dimu/limau"
d$ag_crop$type[d$ag_crop$type=="njigu"] <-"njugu"
d$ag_crop$type[d$ag_crop$type=="banana"] <-"ndizi"
d$ag_crop$type[d$ag_crop$type=="breadfruit"] <-"mashelisheli"
d$ag_crop$type[grepl("boga", tolower(d$ag_crop$type))] <- "mboga_mboga" 
d$ag_crop$type[grepl("moga", tolower(d$ag_crop$type))] <- "mboga_mboga" 
d$ag_crop$type[grepl("mchicha", tolower(d$ag_crop$type))] <- "mboga_mboga" 
d$ag_crop$type[grepl("njugu", tolower(d$ag_crop$type))] <- "njugu" 
d$ag$ag_guard_hours[1211] <- 90
d$ag_crop$unit[grepl("viazi", tolower(d$ag_crop$type)) & grepl("mikungu", tolower(d$ag_crop$unit))] <- "polo50" 
d$ag_crop$unit[grepl("bamia", tolower(d$ag_crop$type)) & grepl("mikungu", tolower(d$ag_crop$unit))] <- "fungu" 

##################################################
#################### AGE #########################

d$hhm_details$hhm_age[which(d$hhm_details$hhm_age == 220)] <- 22
d$hhm_details$hhm_age[which(d$hhm_details$hhm_age == 667)] <- 66

d$hhm_details$hhm_age[d$hhm_details$`_index` == 2389] = 19


############### PRICE FIXES #########################
names(dpl[,which(dpl ==0)])
names(dpl) <- tolower(names(dpl))
dpl$nazi_gari_ya_gnombe <- 200* dpl$nazi_kipande
dpl$maembe_gari_ya_gnombe <- 200* dpl$maembe_kipande
dpl$kuni_gari_ya_gnombe <- 10*dpl$kuni_gari_ya_gnombe
dpl$kuni_mzigo_kichwa <- dpl$kuni_korja/2
dpl$mpunga_gunia <- 200000
dpl$mashelisheli_mzigo_kichwa <- dpl$breadfruit_kipande*3
dpl$muhogo_kipande <- 500
dpl$muhogo_kg <- 1000
dpl$ndizi_kipande <- 300
dpl$ndizi_mzigo_kichwa <- dpl$ndizi_mikungu
dpl$viazi_kg <- 1800
dpl$njugu_Nyasa_polo50 <- dpl$njugu_pishi*(50/1.5)
dpl$njugu_polo25 <- dpl$njugu_pishi*(25/1.5)
dpl$njugu_polo50 <- dpl$njugu_pishi*(50/1.5)
dpl$njugu_polo50 <- dpl$njugu_pishi*(50/1.5)
dpl$njugu_kipande <- dpl$njugu_pishi/10
dpl$muhogo_mzigo_kichwa <- 5*dpl$muhogo_kg
dpl$pegi_mzigo_kichwa <- 30*25#number*price
dpl$malisho_polo50 <- 2000
dpl$kuni_gari_ya_gnombe <- dpl$kuni_korja*3
dpl$kuni_gari_ya_gnombe <- 5*400
dpl$mboga_ya_matembele_kicha <- 500
dpl$malimau_kipande <- 200
dpl$`dimu/limau_kipande` <- 200
dpl$mchicha_vicha <- 500
dpl$mchicha_kicha <- 500
dpl$`dimu/limau_kg` <- 1000
dpl$fenesi_kg <- 2000
dpl$mdalasini_kipande <- 2000
dpl$mwani_kg <- 500
dpl$mchicha_kipande <- 500
dpl$mboga_ya_muhogo_vicha <- 500
dpl$mafuta_liter <- 5000
dpl$pegi_kipande  <- 30
dpl$matunda_kg  <- 1500
dpl$kuwinda_kipande  <- 2000
dpl$chikichi_pishi <- 2000
dpl$chikichi_polo50 <- dpl$chikichi_pishi*(50/1.5)
dpl$ukili_kicha <- 500
dpl$matunda_kipande <- 500
dpl$makuti_kipande <- 400
dpl$kuni_kipande <- dpl$kuni_korja/20
dpl$mafuta_chupa_2l <- dpl$mafuta_liter*2
dpl$matunda_polo25 <- dpl$matunda_kg*25
dpl$moga_ya_muhogo_kicha <- 500
dpl$mboga_yamuhogo_kicha <- 500
dpl$alipasua_boriti_korja <- 20000
dpl$alikata_miti_ya_kujengea_korja <- 20000
dpl$mafuta_mzigo_kichwa <- 5000
dpl$chikichi_polo25 <- dpl$chikichi_polo50/2
dpl$utembo_kicha <- 1000
dpl$nazi_kicha <- 1000
dpl$malisho_polo25 <- dpl$malisho_polo50/2
dpl$mboga_ya_mhogo_kicha <- 500
dpl$njigu_pishi <- dpl$njugu_pishi
dpl$mbogabya_muhogo_kicha <- 1000
dpl$`dimu/limau_polo25` <- 25000
dpl$mboga_ya_mhuogo_kicha <- 1000
dpl$`dimu/limau_ndoo_ndogo` <- 10000
dpl$mboga_ya_mriba_kipande <- 1000
dpl$mboga_ya_mhuogo_polo25 <- 25000 # CHECK
dpl$matawi_ya_mjiti_kipande <- 1500 # CHECK 
dpl$muhogo_kicha <- dpl$muhogo_kipande # CHECK 
dpl$mchikichi_mzigo_kichwa <- 5000
dpl$ndizi_polo25 <- 20000
dpl$asali_chupa_2l <- dpl$asali_liter*2
dpl$mbao_korja <- 50000
dpl$`dimu/limau_kicha` <- 500
dpl$mchikichi_kicha <- 1000 #CHECK
dpl$chikichi_kicha <- 1000 #CHECK
dpl$mboga_ya_mhuogo_kipande <- 1000 # CHECK
dpl$mabilingani_kipande <- 1000 # CHECK
dpl$'kubanja chikichi_pishi' <- 1000
dpl$kubanja_chikichi_pishi <- 1000
dpl$kuni_dau <- dpl$kuni_gari_ya_gnombe # CHECK
dpl$kukata_magogo_kwa_ajili_ya_kutengeneza_mkaa_kipande <- 100000 #CHECK
dpl$miche_ya_mikarafuu_kipande <- 1000
dpl$machungwa_kipande <- 175
dpl$mboga_ya_mhuogo_mikungu <- 5000
dpl$kuni_kicha <- 1000
dpl$mboga_mboga_mikungu <- 1000
dpl$mboga_ya_mhuogo_mzigo_kichwa <- 1000
dpl$kukata_fito_kipande <- 1000 # part of 
dpl$mkaa_polo50<-10000
dpl$mashelisheli_kicha <- dpl$mashelisheli_kipande
dpl$mashelisheli_kg <- dpl$mashelisheli_kipande #CHECK
dpl$mabelungi_kipande <- 1000 
dpl$mpunga_polo25 <- 40000 #CHECK
dpl$'dimu/limau_bakuli' <- dpl$`dimu/limau_kicha` #CHECK
dpl$mashelisheli_polo50 <- 50000 #CHECK
dpl$kukata_miyale_kipande <- 1200 #
dpl$mbegu_za_viazi_polo50 <- 30000 #CHECK
dpl$nazi_polo25 <- 25000 #CHECK
dpl$balungi_kipande <- 1000 #CHECK
dpl$balungi_kicha <- dpl$balungi_kipande
dpl$kuni_polo25 <- dpl$kuni_korja #CHECK
dpl$kukata_miti_ya_kujengea_kibanda_kipande <- dpl$alikata_miti_ya_kujengea_korja #CHECK
dpl$ukili_mzigo_kichwa <- dpl$ukili_kicha*2 #CHECK
dpl$dawa_na <- 1
dpl$"alipasua boriti_korja" <- 20000
dpl$"alikata miti ya kujengea_korja" <- 20000
dpl$"alikata miti ya kujengea_korja" <- 20000
dpl$"alikata miti ya kujengea_korja" <- 20000
dpl$"kukata magogo kwa ajili ya kutengeneza mkaa_kipande" <- 1000 #CHECK
dpl$"miche ya mikarafuu_kipande" <- 1000 #CHECK
dpl$"matawi ya mjiti_kipande" <- dpl$matawi_ya_mjiti_kipande
dpl$themai <- 1
dpl$mboga_mboga_kicha <-  dpl$mbogabya_muhogo_kicha
dpl$mboga_mboga_mzigo_kichwa <- dpl$mboga_mboga_kicha*3
dpl$mboga_mboga_vicha <-  dpl$mbogabya_muhogo_kicha
dpl$mboga_mboga_kipande <-  dpl$mboga_ya_mriba_kipande
dpl$"dimu/limau_ndoo ndogo" <- dpl$`dimu/limau_ndoo_ndogo`
dpl$"mboga_mboga_polo25" <- dpl$mboga_ya_mhuogo_polo25
dpl$`dimu/limau_ndoo`<- dpl$`dimu/limau_ndoo_ndogo`*2
dpl$viazi_vitamu_polo25 <- 30000
dpl$mboga_mboga_mzigo_kichwa <- 20000
dpl$mkaa_kipande <- dpl$mkaa_polo50 #CHECK
dpl$milk_liter = 1000
dpl$mpunga_polo50 = 78000
dpl$mahindi_polo25 <- 50000
dpl$matunda_bakuli <- 1000
dpl$kukata_miti_ya_kujengea_korja <- 20000
dpl$mdalasini_kg = 6000 #CHECk
dpl$viazi_polo25 = 110000#CHECk
dpl$mbaanzi_kg = 2600 #CHECk
dpl$mabilingani_ndoo <- 5000 #CHECk
dpl$matunda_kocha <- 1000 #CHECk
dpl$bamia_ndoo <- 20000
dpl$ndizi_kicha <- dpl$ndizi_mikungu
dpl$'dimu/limau_mikungu' <- dpl$`dimu/limau_kipande`
dpl$ukwaju_kipande <- 100
dpl$viazi_vitamu_polo50 <- 45000
dpl$viazi_vitamu_kipande <- 500
dpl$mpunga_pishi <- 3500
dpl$mabilingni_ndoo <- 12500
dpl$mashelisheli_ndoo <- dpl$mashelisheli_kipande*20
dpl$malisho_kipande <- dpl$malisho_polo25
dpl$tungule_ndoo <- 15000
dpl$mahindi_polo50 <- dpl$mahindi_polo25*2
dpl$tango_kipande <- 5000
dpl$parachichi_ndoo <- 11000
dpl$mboga_mboga_bakuli <- 1000
dpl$"mwani_dinge_moja.ni_sawa_na_50kg_polo(5)" <- 25000*6
dpl$mboga_mboga_polo50 <- dpl$mboga_mboga_polo25*2
dpl$nazi_mikungu <- dpl$nazi_kipande
dpl$mabungo_kipande <- 100
dpl$mapera_ndoo <- 20000 # CHECK  
dpl$matufaa_ndoo <- 30000 # CHECK 
dpl$mahindi_kipande <- 100 # Check
dpl$mwani_dinge <- 25000*6 # Check 
dpl$matunda_gari_ya_gnombe <- 100000 # CHECK
dpl$kuni_ndoo <- 20000 #CHECK 
dpl$"dimu/limau_ndoo_ya_lita_25" <- 7000
#2022-10-11
dpl$mapera_kipande <- 75 # Check
dpl$passion_ndoo <- 8000 # Check 
dpl$majimbi_polo25 <- 50000 # Check
dpl$viazi_vitamu_mzigo_kichwa <- 10000 # 
dpl$mwani_hori_moja <- dpl$mwani_dinge
dpl$mdalasini_polo50 <- 60000
dpl$mwani_dau <- 20000
dpl$karafu_kikombe <-  1000
dpl$maembe_sakua_ndoo <- 5000
dpl$mboga_mboga_liter <- 1000
dpl$uwanga_kg <- 1000
dpl$makuti_gari_ya_gnombe <- 10000
dpl$matunda_ndoo <- 5000
dpl$makuti_ndoo <- 5000
dpl$kuka_miti_mzigo_kichwa <- 10000
dpl$makuti_mzigo_kichwa <- 1200
dpl$kuni_mzigo_kichwa <- 2000
dpl$miwa_korja = 13000
dpl$"dimu/limau_kisado" <- 1000
dpl$"dimu/limau_kisado" <- 1000
dpl$"tungule_ndoo_ndogo_1" <- dpl$tungule_ndoo
dpl$mapapai_kipande  <- 500
dpl$maembe_sakuwa_ndoo  <- 8000
dpl$maembe_sakuwa_ndoo  <- 8000
dpl$mabilingani_fungu <- 2000
dpl$kuni_gari_moja_ya_carry <- 400000
dpl$bilingani_kipande <- 1000
dpl$bilingani_fungu <- 3000
dpl$viazi_vitamu_bakuli <- 1000
dpl$malisho_mzigo_kichwa <- 1000
dpl$ukili_kipande <- 1000
dpl$papai_kipande <- 750
dpl$mdalasini_polo25 <- 3*6000
dpl$bilingani_ndoo <- 10000
dpl$bilingani_kicha <- 1000
dpl$nynya_nyna_chungu_fungu <- 1000
dpl$nynya_nyna_chungu_kidoo_kidogo<- 1000
dpl$nynya_nyna_chungu_ndoo<- 11000
dpl$bilingani_mikungu <- dpl$bilingani_fungu
dpl$ndizi_mikungu <- 6500
dpl$kunde_polo50 <- 30000 # NOTE THIS PERSON HAS HARVESTED WET 7696 is wet and not dried.
dpl$nanasi_kipande <- 3500 # NOTE THIS PERSON HAS HARVESTED WET 7696 is wet and not dried.
dpl$karafu_polo25 <- 25*21000 # NOTE THIS PERSON HAS HARVESTED WET 7696 is wet and not dried.
dpl$miwa_polo50 <- 12000
dpl$maembe_ndoo <- 300*12
dpl$bamia_fungu <- 100
dpl$ndizi_fungu <- 6000
dpl$fenesi_kicha <- 3000
dpl$fenesi_ndoo <- 9000
dpl$fenesi_mikungu <- 3000
dpl$kunde_kg <- 3000
dpl$mahindi_fungu <- 1000
dpl$muhogo_nusu_shamba  <- 100000
dpl$maembe_bakuli <-1000
dpl$maembe_bakuli <-1000
dpl$majimbi_fungu <-2000
dpl$nazi_ndoo <-20000
dpl$muhogo_fungu <- 1000
dpl$mapera_kisado <- 1000
dpl$fenesi_dinge <- 1000
dpl$parachichi_kipande <- 500
dpl$njugu_pishi_wet <- 3500
dpl$maembe_polo50 <- 40000
dpl$miwa_kipande <- 1500
dpl$muhogo_kocha <- 1500
dpl$matunda_polo50 <- 30000
dpl$matunda_polo50 <- 3000
dpl$matunda_polo50 <- 3000
dpl$matunda_ya_msitu_ndoo <- 10000
dpl$"utembo_(kwa_makuti)_mzigo_kichwa" <- 20000
dpl$kuni_gair_ya_kari <- 40000
# 17/08/2023
dpl$viungo_kipande <- 100
dpl$viungo_bakuli <- 2000
dpl$mabalungi_kipande <- 600
dpl$balungi_gunia <- 600*20
dpl$njugu_kg <- 4000
dpl$muhogo_gunia <- 30000  
dpl$ticketi_maji_kipande <- 3000
dpl$maembe_polo25 <- 500*40
dpl$majimbi_polo50 <- 80000  
dpl$viungo_ndoo <-  
dpl$passion_kipande <- 350   
dpl$mboga_mboga_fungu <- 750   
dpl$viazi_vitamu_fungu <- 2000
dpl$njugu_fungu <- 400
dpl$machungwa_polo25 <- 30*200
dpl$mahindi_ndoo <- 500*15
dpl$mpunga_mzigo_kichwa <- 
dpl$tungule_kisado <- 3000
dpl$viazi_fungu <- 1000
dpl$karafu_pishi_wet <-    
dpl$mpunga_kicha <- 
dpl$viazi_vitamu_kicha <- 1000
dpl$matunda_ya_msitu_kisado <- 1000
dpl$'mkaa_(tanu)_polo50' <- 12000
dpl$"asali_ya_pori/msitu_chupa_2l" <- 30000 
dpl$"asali_ya_pori/msitu_liter" <- 15000 
dpl$makuti_kicha <- 500
dpl$utembo_kipande <- 100
dpl$fito_kipande <- 300 


###################
######## FIX ######

##########################################################################
###################### PERSONELLE REMOVALS ###############################

remover<-function(uid, d){
  for(i in 1:length(d)){
    print(i)
    col<-select(d[[i]], contains("uuid"))
    rm<-which(col == uid)
    if(length(rm) > 0){
      d[[i]]<-d[[i]][-rm,]
    }
  }
  return(d)
}


# Remove Abdalla Ali Abadalla duplicates
uid <- d$main$"_uuid"[which(((d$main$hhm_hhh ==  "Abdalla Ali Abdalla" & d$main$day=="19")& d$main$month =="04") & d$main$year=="2022")]
d <- remover(uid, d)
# Remove second
uid <- d$main$"_uuid"[which(((d$main$hhm_hhh ==  "Abdalla Ali Abdalla" & d$main$day=="12")& d$main$month =="05") & d$main$year=="2022")]
d <- remover(uid, d)

# DELETE DUPLICATE ENTRIES!
d$hhm_details<-d$hhm_details[-which(d$hhm_details$hhm_name == "Ali Hamadi Salim"  & d$hhm_details$hh_ID=="doacc5")[2],]# REMOVE Duplicate ALI HAMADI SALIM 
# d$hhm_details<-d$hhm_details[which(d$hhm_details$hhm_name == "Muhammed Mussa Hamadi")  & d$hhm_details$hh_ID=="z3im00")[2],] # Muhammed Mussa Hamadi
# 
# 
# #removeInstance <- function(x)
# 
# which(d$hhm_details$hhm_name == "Muhammed Mussa Hamadi")
# 
# a<-which(db2$self_emp_source$self_names == "Muhammed Mussa Hamadi")
# uuid<-db2$self_emp_source$"_submission__uuid"[a]
# ind<-which(db2$main$"_uuid" %in% uuid)
# db2$main[ind, ]$hhh[5]
# a[5]
# 
# 
# getSurvey("z3im00", 7)
# 
 a<-which(d$self$names == "Muhammed Mussa Hamadi")
 uuid<-d$self$"_submission__uuid"[a]
 ind<-which(d$main$"_uuid" %in% uuid)
 d$main[ind, ]$hhh
# 
# ind[5]
# 
# d$main[ind, ]$hhm_hhh
 
 
 ###############################################################################
 ######################### ADDITIONAL BUILDS ###################################
 
d$con$con_total<-rowSums(d$con[, which(names(d$con)=="con_food"):which(names(d$con) == "con_sikuukuu")], na.rm = T)

 ###############################################################################
 ####################### CLEAN NAMES ##########################################
 
 
 uhid <-unique(d$hhm_details$hh_ID)
 # So here i think i need to drop all NA
 d$hhm_details<-d$hhm_details[d$hhm_details$hhm_name != "NA", ]

 # TEST BIN

  
 dup <-c()
 first <-c()
 duplicate <-c()
 id <-c()
 hhid <-c()
 age <- c()
 gender <- c()
 for(i in uhid){
   allnames<-d$hhm_details$hhm_name[which(d$hhm_details$hh_ID==i)]
   allids<-d$hhm_details$hhm_ID[which(d$hhm_details$hh_ID==i)]
   allages<-d$hhm_details$hhm_age[which(d$hhm_details$hh_ID==i)]
   allgender<-d$hhm_details$hhm_gender[which(d$hhm_details$hh_ID==i)]
   
   
   for(j in 1:length(allnames)){
     duplicate=stringdist(allnames[j], allnames[-j], method = "jw") <.1
     if(any(duplicate)){
       dup <-c(dup, i)
       first <- c(first, allnames[j])
       id <- c(id, allids[j])
       match <- c(match, allnames[duplicate])
       hhid <- c(hhid, i)
       age <- c(age, allages[j])
       gender <- c(gender, allgender[j])
     }
   }
 }
 dup<-unique(dup)
 
 num_id <- c()
 for(i in id){
   num_id = c(num_id, sum(d$hhm_check$get_name_old == i, na.rm =T))
 }
 
 
 df = data.frame(
   id = id,
   first = first,
   age = age,
   gender = gender,
   num_id = num_id,
   hhid = hhid
 )
 
 keep_name <- c("Fatma Yussuf Saidi", "Khamis Juma Makarani", "Halima Abeid Seif", "Sabra Hamadi Omar", "Zulfa Issa Ali", 'Khalfani Tumu Ali',
                "Salama Mkwale Hassan", "Lailati Omar", "Said Omar Khamisi", "Time Hamad Salim", "Arafa Uledi Muhamadi", "Saumu Abdalla Suleiman",
                "Hassan Khamis Juma","Khalfan Hamad Khamis", "Saumu Fumu Miraji", "Raya Msellem Ali", "Khadija Ali Mohd", "Fatma Haji Said",
                "Mulhat Said Mohd", "Rukaiya Ahmed Juma", "Naifat Salim Nassor", " Fatuma Asaa Khamis")
 
 keep_id <-c("jfy5hy","vi4ujl", "ddp76g", "qvxyyq", "puff28", "e2yk15", "a6208v", "hqnqha","ft39jy", "fqaj3u", "c64446","hn76rv", "sr3m4h", "kn1qub", 
             "vqevxs", "o9i7pl", "f4rhho", "wrabbv", "u0hx7s", "znrs7l", "bg3y5c", "dzkht8")
 
 drop_name <- c("\nFatma Yussuf Said","Khamisi Juma Makarani", "Halima Abaid Seif", "Sabra Hamad Omar", "Zulfa Issa", "Khalfani Tumu Ali",
                "Salama Mkwale Hassan", "Lailati Omar", "Said Omar Khamis", "Time Hamadi Salum", "Arafa Uledi Muhammed", "Saumu Abdalla Suleiman",
                "Hassan Khamis Juma\n", "Khalfan Hamad Khamis", "Saumu Fumu Miraji", "Raya Msellem Ali", "Khadija Ali Mohd", "Fatma Haji Said",
                "Mulhat Said Mohd", "Rukaiya Ahmed Juma","Naifat Salim Nassor", "Fatma Asaa Khamis")
 
 drop_id <-c("ztzjup", "lqj1nq", "kqqgxo", "ccafwt", "p43zk9", "jrcf4o", "zqpc7e", "hkkweo", "wi8k7p", "qfq4zu", "qcngjp", "pmhzol", "xybcyc", "stphky",
             "relgp4", "ayqp2z", "wknoqx", "u9qyys", "jnkerj", "hwq101", "tivmy6", "rx96dl")
 
 #############################################################################
 ################## DO A MANUAL FULL CLEAN ###################################
 
 # the cleaner will search all dataframes where names are possible and replace all instances of drop_name with keep_name - done
 # the cleaner will change all checking names and id.- done
 # the cleaner will do the same with hmm_details - done
 # the cleaner will search for duplicates of the same names in hmm_details
 # when a second instance of the same name is found, it will delete that instance and make a new instance in hmm_checking for the period.
 
 cnt = 1
 for(i in drop_name){
   ind=which(d$safari$work_trip_names==i)
   if(length(ind > 0)) d$safari$work_trip_names[ind]==keep_name[cnt]
   
   ind=which(d$hhm_check$hhm_name_old1 == i)
   if(length(ind > 0)){
     d$hhm_check$hhm_name_old1[ind]==keep_name[cnt]
     d$hhm_check$get_name_old[ind]==keep_id[cnt]
   }
   ind=which(d$self$names ==i)
   if(length(ind > 0)) d$self$names[ind]==keep_name[cnt]
   
   ind=which(d$wages$names ==i)
   if(length(ind > 0)) d$wages$names[ind]==keep_name[cnt]
   
   ind=which(d$ag$ag_names==i)
   if(length(ind > 0)) d$ag$ag_names[ind]==keep_name[cnt]
   
   ind=which(d$mwani$mwani_names ==i)
   if(length(ind > 0)) d$mwani$mwani_names[ind]==keep_name[cnt]
   
   ind=which(d$animal_emp$animals_names==i)
   if(length(ind > 0)) d$animal_emp$animals_names[ind]==keep_name[cnt]
   
   ind=which(d$forest$forest_names==i)
   if(length(ind > 0)) d$forest$forest_names[ind]==keep_name[cnt]
   
   ind=which(d$hhm_details$hhm_name==i)
   if(length(ind>0)){
     d$hhm_details$hhm_name ==keep_name[cnt]
     d$hhm_details$hhm_ID ==keep_id[cnt]
   }
   cnt = cnt+1
 }
 
ind_dup<-which(duplicated(d$hhm_details$hhm_name))
dup_names<-d$hhm_details$hhm_name[ind_dup]


ind = which(d$hhm_details$hhm_name==dup_names[1])
df<-d$hhm_details[ind, ]

for(i in dup_names[2:length(dup_names)]){
  ind = which(d$hhm_details$hhm_name==i)
  if(anyDuplicated(d$hhm_details$hh_ID[ind]) >1){
    df<- rbind(df, d$hhm_details[ind, ])
  }
}
 
df<-df[3:nrow(df),]
make_new<-rep(c(FALSE, TRUE), nrow(df)/2)
df_delete<-df[make_new,]
make_new<-rep(c(TRUE, FALSE), nrow(df)/2)
df_keep<-df[make_new,]


empty_row <- rep(NA, length=ncol(d$hhm_check))
names(empty_row) <- names(d$hhm_check)


for(i in 1:nrow(df_delete)){
  add = nrow(d$hhm_check)+1
  d$hhm_check <- rbind(d$hhm_check, empty_row)
  d$hhm_check[add, "get_name_old"] <-df_keep$hhm_ID[i]
  d$hhm_check[add, "hhm_name_old"] <-df_keep$hhm_name[i]
  d$hhm_check[add, "hhm_name_old1"] <-df_keep$hhm_name[i]
  d$hhm_check[add, "hhm_health_old"]<-df_keep$hhm_health[i]
  d$hhm_check[add, "hhm_food_security_old"]<-df_keep$hhm_food_security[i]
  d$hhm_check[add, "_index"] <- d$hhm_check[add-1, "_index"]+1 
  d$hhm_check[add, "_parent_index"] <- df_keep$"_parent_index"[i] 
  d$hhm_check[add, "_submission__uuid"] <- df_keep$"_submission__uuid"[i] 
  d$hhm_check[add, "_parent_table_name"] <- df_keep$"_parent_table_name"[i] 
  d$hhm_check[add, "_submission__id"] <- df_keep$"_submission__id"[i] 
  d$hhm_check[add, "_submission__submission_time"] <- df_keep$"_submission__submission_time"[i] 
  d$hhm_check[add, "_submission__submitted_by"] <- df_keep$"_submission__submitted_by"[i] 
  d$hhm_check[add, "_submission___version__"] <- df_keep$"_submission___version__"[i]
  d$hhm_check[add, "period"] <- df_keep$"period"[i]
  d$hhm_check[add, "hhh"] <- df_keep$"hhh"[i]
  d$hhm_check[add, "hhID"] <- df_keep$"hhID"[i]
  d$hhm_check[add, "shehia"] <- df_keep$"shehia"[i]
}


# Now delete them from the main list #

for(i in 1:nrow(df_delete)){
  print(ind)
  ind = which(d$hhm_details$"_index" == df_delete$"_index"[i])
  d$hhm_details = d$hhm_details[-ind,]
}


 ########################################################
 ############ FOREST NAME CLEAN #########################
 
 # 
 # 
 # u=tolower(paste(d$forest_crop$type, d$forest_crop$unit, sep ="_"))
 # u<- gsub(" ", "_", u)
 # u <- gsub("piece", "kipande", u)
 # u[grepl("themai", u)] <- "themai"
 # u[u=="firewood_mzigo_kichwa"] <-  "kuni_mzigo_kichwa"
 # 
 # 
 # d$forest_crop$value <- NA
 # for(i in 1:nrow(d$forest_crop)){
 #   if(!is.na(d$forest_crop$type[i])){
 #     temp <- dpl[,u[i]]
 #     d$forest_crop$value[i] <- as.double(temp)*d$forest_crop$harvest[i]
 #   }
 # }
 # 
 # # Partially clean the data
 # inds<-which(d$forest_crop$type %in% c("Alipasua boriti", "Kukata Magogo kwa ajili ya kutengeneza mkaa", "Kukata fito", "mbao", "Alikata miti ya kujengea",
 #                                       "Matawi Ya mjiti", "Kukata Miti ya Kujengea kibanda", "Kukata miti Ya kujengea"))
 # d$forest_crop$type[inds] = "timber"
 # d$forest_crop$type[which(d$forest_crop$type =="Kubanja Chikichi")] <- "mchikichi"
 # d$forest_crop$type[which(d$forest_crop$type =="Chikichi")] <- "mchikichi"

####################################################
########## CLEANS JANUARY 25th 2024 ################
finder(d$ag_crop, "viazi vitamu", "polo50")

# Price additions
dpl$viazi_polo50 <- 50000

d$ag_crop$unit[replacer(d$ag_crop, finder2(d$ag_crop, "majimbi", "mikungu"))] <- "fungu"
d$ag_crop$unit[replacer(d$ag_crop, finder2(d$ag_crop, "muhogo", "mikungu"))] <- "fungu"
d$ag_crop$unit[replacer(d$ag_crop, finder2(d$ag_crop, "ndizi", "polo50"))] <- "mikungu"
dpl$nazi_polo50 <- 60000
dpl$"dimu/limau_polo" <- 100000
d$ag_crop$unit[replacer(d$ag_crop, finder2(d$ag_crop, "muhogo", "bakuli"))] <- "kg"
d$ag_crop$unit[replacer(d$ag_crop, finder2(d$ag_crop, "mpunga", "fungu"))] <- "kg" # DOUBLE CHECK!
#d$ag_crop$harvest[replacer(d$ag_crop,  finder2(d$ag_crop, "viazi", "piece"))] <- 10 # DOUBLE CHECK!
d$ag_crop$unit[replacer(d$ag_crop, finder2(d$ag_crop, "viazi", "kg"))] <- "kg" # DOUBLE CHECK!
dpl$mdalasini_bakuli <- 5000
dpl$mpunga_pishi_dry <- dpl$mpunga_polo50/50 *1.5
dpl$karafu_pishi_dry <- 21000
dpl$passion_kg <- 5000
dpl$bilingani_polo25 <- 20000
d$ag_crop$price[d$ag_crop$`_index` == 6155] <- 1000
d$ag_crop$unit[replacer(d$ag_crop, finder2(d$ag_crop, "mpunga", "mikungu"))] <- "polo50" 
dpl$tungule_fungu <- 700
dpl$uanga_polo25 <- 10000
dpl$mbirimbi_polo25 <- 10000
dpl$mbaazi_bakuli <- 1000
dpl$mbaazi_kisado <- 10000
dpl$pili_pili_manga_kg <- 10000
dpl$nynya_nyna_chungu_kisado <- 4000
dpl$viazi_vikuu_polo25 <- 25000
dpl$"dimu/limau_mzigo_kichwa" <- 3000
dpl$fenesi_polo25 <- 10000
dpl$matunda_ya_msitu_kipande <- 2000
dpl$kuni_polo50 <- 20000
dpl$matunda_ya_msitu_mzigo_kichwa <- 2000
dpl$miti_kupiga_tanu_korja <- 20000
dpl$mchikichi_kipande <- 1000
dpl$machakacha_mzigo_kichwa <- 1000 #CHECK
dpl$machakacha_mzigo_kichwa <- 1000 #CHECK
dpl$matunda_ya_msitu_vigaloni_vya_litre5 <- 20000
dpl$"mkaa_(tanu)_gari_ya_gnombe" <- 40000
dpl$"bamia_kipande" <- 50
dpl$mabungo_polo50 <- 35000
dpl$kuokota_karafuu_pishi_wet <- 3000
dpl$korosho_pishi_wet <- 1000
dpl$muhogo_ndoo <- 3000
dpl$"utembo_(kwa_makuti)_gari_ya_gnombe" <- 10000
dpl$matunda_ya_msitu_kocha <- 1000
#d$forest_crop[replacer(d$ag_crop, finder2(d$forest_crop, "mkaa (tanu)", "korja"), ] #TALK TO MACHANO
dpl$matunda_ya_msitu_polo50 <- 40000
d$forest_crop$unit[replacer(d$forest_crop, finder2(d$forest_crop, "pegi", "ndoo"))] <- "piece"
dpl$uwanga_polo25 <- 30000
dpl$uwanga_ndoo <- 10000
dpl$matunda_ya_msitu_kidumu_cha_litre_5 <- 20000
d$ag_crop$unit[replacer(d$forest_crop, finder2(d$ag_crop, "tungule", "mikungu"))] <- "fungu" 
dpl$karafu_polo50 <- 50/1.5*21000
dpl$ukwaju_polo25 <- 20000
dpl$kunde_kisado <- 15000
dpl$mbirimbi <- 200
dpl$miti_ya_kujengea_korja <- 30000
dpl$matunda_ya_msitu_bakuli <- 4000
dpl$matunda_ya_msitu_polo25 <- 30000
dpl$"mkaa_(tanu)_polo25" <- 6000
d$forest_crop$unit[replacer(d$forest_crop, finder2(d$forest_crop, "kuni", "mikungu"))] <- "mzigo ya kiwchwa"
dpl$ukindu_kicha <- 1000
dpl$ukindu_kicha <- 1000
dpl$miti_kupiga_tanu_gari_ya_gnombe <- 70000
dpl$uwanga_polo50 <- 300000
dpl$kukata_kuni_korja <- 20000

############################################################################################
##### JAN 24 2024  REMOVE Annoying DUPLICATE NAMES and Reorder Columns in each dataset #####


for(i in names(d)){
    names(d[[i]]) <- gsub("__", "_", names(d[[i]]))
    d[[i]] <- d[[i]][, !duplicated(colnames(d[[i]]), fromLast = TRUE)] 
    d[[i]] <- d[[i]][,c(names(select(d[[i]], !starts_with("_"))), names(select(d[[i]], starts_with("_"))))]
}


################################################################################
########## CLEANING THE BAD DATA SETS ##########################################

# Clean ag
old_ag_names<- names(d$ag)
new_ag_names <- gsub("ag_", "", old_ag_names)
names(d$ag) <-new_ag_names

# Clean animal emp
names(d$animal_emp)
old_animal_names<- names(d$animal_emp)
new_animal_names <- gsub("animals_", "", old_animal_names)
names(d$animal_emp) <-new_animal_names

# Clean forest
names(d$forest)
old_forest_names<- names(d$forest)
new_forest_names <- gsub("forest_", "", old_forest_names)
names(d$forest) <-new_forest_names

# Clear gifting Give 
names(d$gift_give)
old_gift_give_names<-names(d$gift_give)
new_gift_give_names <- gsub("gift_give_", "", old_gift_give_names)
names(d$gift_give) <-new_gift_give_names

# Clean gift recieve
names(d$gift_rec)
old_gift_rec_names<-names(d$gift_rec)
new_gift_rec_names <- gsub("gift_rec_", "", old_gift_rec_names)
names(d$gift_rec) <-new_gift_rec_names

# Clean Details
names(d$hhm_details)
old_hhm_details_names<- names(d$hhm_details)
new_hhm_details_names <- gsub("hhm_", "", old_hhm_details_names)
names(d$hhm_details) <-new_hhm_details_names
names(d$hhm_details)[which(names(d$hhm_details) == "ID")] <- "hhm_ID" 

# Clean Checking
names(d$hhm_check)
old_hhm_check_names<- names(d$hhm_check)
new_hhm_check_names <- gsub("hhm_", "", old_hhm_check_names)
names(d$hhm_check) <-new_hhm_check_names

# Clean
names(d$store_credit)
old_store_credit_names<- names(d$store_credit)
new_store_credit_names <- gsub("store_credit_", "", old_store_credit_names)
names(d$store_credit) <-new_store_credit_names

names(d$store_credit)[which(names(d$store_credit) == "name")] <- "names"

# Clean safari
names(d$safari)
old_work_trip_names<- names(d$safari)
new_work_trip_names <- gsub("work_trip_", "", old_work_trip_names)
names(d$safari) <-new_work_trip_names

# Clean Rent
names(d$rent_emp)
old_rent_names<- names(d$rent)
new_rent_names <- gsub("rent_", "", old_rent_names)
names(d$rent) <-new_rent_names

# Clean sold
names(d$sold)
old_sold_names<- names(d$sold)
new_sold_names <- gsub("sold_", "", old_sold_names)
names(d$sold) <-new_sold_names

# Clean mwani
names(d$mwani)
old_mwani_names<- names(d$mwani)
new_mwani_names <- gsub("mwani_", "", old_mwani_names)
new_mwani_names <- gsub("ag_", "", new_mwani_names)
names(d$mwani) <-new_mwani_names

# Clean
# Set all name to names

for(i in 1:length(d)){
  if(any(names(d[[i]])=="name")) print(names(d)[[i]])
}


##########################################################################
############# ADD IN PERSONELLE IDS ######################################

for(j in names(d)){
  if(any(names(d[[j]])=="names") & any(names(d[[j]]) != "hhm_ID" )){ # check to see if the data frame has names in it
    uni_ppl <- unique(data.frame(hhID = d[[j]]$hhID, name = d[[j]]$names, hhmID = NA)) # get a full list of unique people
    for(i in 1:nrow(uni_ppl)){
      uni_ppl$hhmID[i] <- d$hhm_details$hhm_ID[which(d$hhm_details$name == uni_ppl$name[i] & d$hhm_details$hhID == uni_ppl$hhID[i])][1] # find thier hhmID
    }
    d[[j]]$hhm_ID <- NA # initalize a vector for hhm_id
    for(i in 1:nrow(uni_ppl)){
      ind = which(d[[j]]$names == uni_ppl$name[i] & d[[j]]$hhID == uni_ppl$hhID[i]) # get all the instances where that person appears
      d[[j]]$hhm_ID[ind] = uni_ppl$hhmID[i]  # assign ID
    }
  }
}



###########################################
######### THESE SHOULD BE DROPPED ##########

remove <- c("gift_rec_rep", "gift_give_rep", "rent_emp", "emp", "self_emp", "store_credit_rep")
rm <- c()
for(i in remove){
 rm <-  c(rm, which(names(d)== i))
}
d <- d[-rm]


#ADD IN 
#gov_help_other_num
# gov_help_emp_sources

# CHECK IF PERPARE CLOVES IS EMPTY
# CHECK IF ag_harvest_cloves_hours IS EMPTY
# CHECK MWANI PREPARE HOURS