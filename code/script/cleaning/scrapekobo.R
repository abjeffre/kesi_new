library(readxl)
library(dplyr)
`%ni%` <- Negate(`%in%`)
# setwd("C:/Users/jeffr/OneDrive/Documents/Risk/")
# setwd("C:/Users/Jeffrey Andrews/OneDrive/Documents/Risk")

#############################################
########## LOAD PRICE LIST ##################

df <- file.info(list.files(path ="data/", full.names = T))

df <- df[which(grepl("price_list", rownames(df))), ]

newest<-rownames(df)[which.max(df$mtime)]

sheet_names<-excel_sheets(newest)
dpl <- list()

for(i in sheet_names){
  dpl[[i]]<-read_xlsx(newest, sheet = i, na = c("", "-99", "  -99"))
}
dpl <- dpl$price_list


##############################################
########## GET FIRST WEEK ####################


sheet_names<-excel_sheets("data/week_one.xlsx")
db <- list()

for(i in sheet_names){
  db[[i]]<-read_xlsx("data/week_one.xlsx", sheet = i, 
                     na = c("", "-99", "  -99"))
}


names(db)[1] <- "main"

#skip times 
skip<-c("_submission__submission_time", "start", "end", "_submission_time", "today", "migration_year", "hhm_yob")

for(i in 1:length(db)){
  for(j in colnames(db[[i]])){
    if (j %ni% skip){
      db[[i]][,j] <- sapply(db[[i]][,j], function(x) ifelse(x == "Version1 - Mapato Survey", "main", x))
    }
  }
  if(i >1) db[[i]]$'_parent_index' <- as.numeric(db[[i]]$'_parent_index') 
}

db$main<-select(db$main, !contains("..."))

nfirst <-nrow(db$main) 


#############################################
######## GRAB ADDITIONAL WEEKS ##############

# Always grab the most recent file

df <- file.info(list.files(path ="data/", full.names = T))

df <- df[which(grepl("version2", rownames(df))), ]

newest<-rownames(df)[which.max(df$mtime)]

sheet_names<-excel_sheets(newest)
db2 <- list()

for(i in sheet_names){
  db2[[i]]<-read_xlsx(newest, sheet = i, na = c("", "-99", "  -99"))
}

################################################################
################# Correct improper reads ########################

# I made a function  that tabes the var name the database, and she sheet and replaces it will a string. 

corrector<-function(varname, db, sheet, type = "text"){
  a=unlist(read_excel(newest, sheet, range = cell_cols(which(names(db[[sheet]]) == varname)),
                                   col_types = type, n_max = nrow(db[[sheet]]), trim_ws = FALSE))
  if(length(a)!= nrow(db[[sheet]])) a=c(a, rep(NA, nrow(db[[sheet]])-length(a)))
  db[[sheet]][,varname]=a
  return(db)
}

#########################################
########## EXAMPLE IS HERE ##############

db2<-corrector("year_one_project_type", db2, "version2", "text")  
db2<-corrector("mwani_price", db2, "version2", "numeric")  
db2<-corrector("mwani_sold_amount", db2, "version2", type = "numeric") 
db2<-corrector("self_usafiri", db2, "self_emp_source", type = "text")
db2<-corrector("ag_harvest_cloves_hours", db2, "ag_emp_source", type = "numeric")
db2<-corrector("ag_guard_hours", db2, "ag_emp_source", type = "numeric")
db2<-corrector("ag_prepare_cloves_hours", db2, "ag_emp_source", type = "numeric")
db2<-corrector("ag_dry_cloves_hours", db2, "ag_emp_source", type = "numeric")
db2<-corrector("ag_harvest_mwani_hours", db2, "ag_emp_source", type = "numeric")
db2<-corrector("ag_prepare_mwani_hours", db2, "ag_emp_source", type = "numeric")
db2 <- corrector("self_fishing_gear", db2, "self_emp_source", type = "text")
db2 <- corrector("self_store_employees_relationship", db2, "self_emp_source", type = "text")
db2 <- corrector("self_store_hours", db2, "self_emp_source", type = "numeric")
db2 <- corrector("self_store_employees", db2, "self_emp_source", type = "numeric")
db2 <- corrector("self_store_employees", db2, "self_emp_source", type = "numeric")
db2 <- corrector("self_profits_method", db2, "self_emp_source", type = "text")
db2 <- corrector("self_fishing_position", db2, "self_emp_source", type = "text")
db2 <- corrector("self_fishing_gear", db2, "self_emp_source", type = "text")
db2 <- corrector("self_trading", db2, "self_emp_source", type = "text")
db2 <- corrector("self_inkind", db2, "self_emp_source", type = "text")
db2 <- corrector("self_tupopamoja_hours", db2, "self_emp_source", type = "numeric")
db2 <- corrector("self_usafiri", db2, "self_emp_source", type = "text")
db2 <- corrector("self_store_hours", db2, "self_emp_source", type = "numeric")
db2 <- corrector("self_food_business", db2, "self_emp_source", type = "text")
db2 <- corrector("wage_payperiod", db2, "wage_emp_sources", type = "text")
db2 <- corrector("wage_income_method", db2, "wage_emp_sources", type = "text")
db2 <- corrector("wage_government_type", db2, "wage_emp_sources", type = "text")
db2 <- corrector("wage_government_type_other", db2, "wage_emp_sources", type = "text")
db2 <- corrector("ag_sold_method", db2, "ag_other_num", type = "text")
db2 <- corrector("ag_sold_method_other", db2, "ag_other_num", type = "text")
db2 <- corrector("ag_help_plant_mwani", db2, "version2", type = "text")
db2 <- corrector("ag_labor_plant_mwani", db2, "version2", type = "text")
db2 <- corrector("ag_labor_harvest_mwani", db2, "version2", type = "text")
db2 <- corrector("ag_help_harvest_mwani", db2, "version2", type = "text")
db2 <- corrector("ag_help_harvest_cloves", db2, "version2", type = "text")
db2 <- corrector("ag_help_prepare_cloves", db2, "version2", type = "text")
db2 <- corrector("ag_help_harvest_cloves_hours", db2, "version2", type = "text")
db2 <- corrector("ag_help_prepare_cloves_hours", db2, "version2", type = "text")
db2 <- corrector("debtor_lent_relationship", db2, "version2", type = "text")
db2 <- corrector("gift_pemba_south_year1", db2, "version2", type = "text")



db2<-corrector("self_usafiri", db2, "self_emp_source", type = "text")
db2$hhm_check$hhm_food_security_old<-as.numeric(unlist(read_excel(newest, "hhm_check", range = cell_cols(toupper(letters[which(names(db2$hhm_check) == "hhm_food_security_old")])),
           col_types = "numeric")))

# year one gift
a=as.character(unlist(read_excel(newest, "version2", range = cell_cols(toupper(letters[which(names(db2$version2) == "year_one_gift")])),
                               col_types = "text", n_max = nrow(db2$version2), trim_ws = FALSE)))
if(length(a)!= nrow(db2$version2)) a=c(a, rep(NA, nrow(db2$version2)-length(a)))
db2$version2$year_one_gift=a

# Year one tree type
a=as.character(unlist(read_excel(newest, "version2", range = cell_cols(toupper(letters[which(names(db2$version2) == "year_one_gift_tree_type")])),
                                 col_types = "text", n_max = nrow(db2$version2), trim_ws = FALSE)))
if(length(a)!= nrow(db2$version2)) a=c(a, rep(NA, nrow(db2$version2)-length(a)))
db2$version2$year_one_gift_tree_type=a


#################################################################
########### CORRECT MISREAD CHARACHTER VARIABLE #################


a=as.character(unlist(read_excel(newest, "version2", range = cell_cols(toupper(letters[which(names(db2$version2) == "year_one_gift_tree_type")])),
                                 col_types = "text", n_max = nrow(db2$version2), trim_ws = FALSE)))
if(length(a)!= nrow(db2$version2)) a=c(a, rep(NA, nrow(db2$version2)-length(a)))
db2$version2$year_one_gift_tree_type=a



# Build
names(db2)[1] <- "main"

temp <-function(x) ifelse(x == "version2", "main", x)

for(i in 1:(length(db2)-3)){
  for(j in colnames(db2[[i]])){
    if(j %ni% skip){
      db2[[i]][,j] <- sapply(db2[[i]][,j], temp)
    }
  }
  if(i >1) db2[[i]]$'_parent_index' <- as.numeric(db2[[i]]$'_parent_index')
}



db2[[23]]$'_parent_index' <- as.numeric(db2[[23]]$'_parent_index')
db2[[24]]$'_parent_index' <- as.numeric(db2[[24]]$'_parent_index')
db2[[25]]$'_parent_index' <- as.numeric(db2[[25]]$'_parent_index')

db2$main<-select(db2$main, !contains("..."))



################################################
########### PRELIM CLEANS ######################

#check
# db$self_emp_source$"_parent_index"
# db$self_emp$"_index"
# db$self_emp$"_parent_index"
# names(db2$self_emp)
# names(db2$self_other_num)


db$self_emp$job_self <- ifelse(!is.na(db$self_emp$self_other), db$self_emp$self_other, db$self_emp$job_self)
db$main$shehia[61] <- "kipange"
db$main$shehia[62] <- "kipange"


################################################
############ BUILD FULL SETS ###################

# Wage
# First 66
db$wage_emp_sources$wage_main = db$emp$job_wage[db$wage_emp_sources$"_parent_index"]
db$wage_emp_sources$wage_main = ifelse( db$wage_emp_sources$wage_main == "nyingine", db$emp$wage_type_other[db$wage_emp_sources$"_parent_index"], db$wage_emp_sources$wage_main)
#All Remaining
db2$wage_emp_sources$wage_main = db2$wage_other_num$job_wage[db2$wage_emp_sources$"_parent_index"]

#Self
#Fist 66
# Here we can ignore db$self_num_other
db$self_emp_source$self_main = db$self_emp$job_self[db$self_emp_source$"_parent_index"]
#All remaining
db2$self_emp_source$self_main = db2$self_other_num$job_self[db2$self_emp_source$"_parent_index"]

#Ag
# Fist 66
# First note that the db$ag_crop$_index is screwed up because of the ag_other_num (see indexes 157 - 158).  Thus I am re-assigning new unique Indexes
db$ag_crop$"_index" <- 1:nrow(db$ag_crop)
# All remaining
# All data is now stored in ag_other_num  - thus we replace ag_crop with ag_other_num
db2$ag_crop <- db2$ag_other_num

#Animals
# First 66
# We can ignore animals_num_other as it has simply duplicated db$animals_emp
# All Remaining
# Incase we find a duplicate make sure we use other_num
if(nrow(db2$animals_other_num) != nrow(db2$animals_emp)) stop("db2$Animal_other_num and db2$animal_emp are not the same size review code and ensure all indexing is correct") 
db2$animals_emp <- db2$animals_other_num

# Forest
# First 66
# We can ignore forest_other_num as it has simply duplicated db$forest_emp
# All Remaining
if(nrow(db2$forest_other_num) != nrow(db2$forest_emp)) stop("db2$Forest_other_num and db2$animal_emp are not the same size review code and ensure all indexing is correct") 
db2$forest_emp <- db2$forest_other_num

# Rent
# First 66
# Nothing to do here
# All Remaining 
if(nrow(db2$rent_emp) != nrow(db2$rent_other_num)) stop("db2$rent_other_num and db2$rent_emp are not the same size review code and ensure all indexing is correct") 
db2$rent_emp$job_rent <- ifelse(db2$rent_emp$job_rent == "nyingine", db2$rent_other_num$rent_other[db2$rent_other_num$"_parent_index"],  db2$rent_emp$job_rent)
db2$rent_emp$rent_price <-db2$rent_other_num$rent_price[db2$rent_other_num$"_parent_index"]

################################################
############# SET PROPER INDEXS ################




# db2$main$"_index" <- as.numeric(db2$main$"_index") + nfirst
# db2$emp$"_parent_index" <- nrow(db$emp) + db2$emp$"_parent_index"
# db2$self_emp$"_parent_index" <- nrow(db$self_emp) + db2$self_emp$"_parent_index"
# db2$ag_crop$"_parent_index" <- nrow(db$ag_crop) + db2$ag_crop$"_parent_index"
# db2$forest_emp$"_parent_index" <- nrow(db$forest_emp) + db2$forest_emp$"_parent_index"
# db2$animals_emp$"_parent_index" <- nrow(db$animals_emp) + db2$animals_emp$"_parent_index"
# db2$rent_emp$"_parent_index" <- nrow(db$rent_emp) + db2$rent_emp$"_parent_index"

db2$self_emp_source$"_parent_index" <- db2$self_emp_source$"_parent_index"+nrow(db$self_emp_source) 
db2$wage_emp_sources$"_parent_index" <- db2$wage_emp_source$"_parent_index"+nrow(db$wage_emp_sources) 
db2$forest_person_emp$"_parent_index" <- db2$forest_person_emp$"_parent_index"+nrow(db$forest_emp)
db2$animals_person_emp$"_parent_index" <- db2$animals_person_emp$"_parent_index"+nrow(db$animals_emp)

#The rest should be corrected below!

###############################################
############# MERGE ###########################

# Perpare the Dataframes that are the same across both DB and DB2
db2ind<- which(!grepl("other", names(db2))) # Get all the non-"other" data frames  
dbind<- which(!grepl("other", names(db)))  
remove <-which(names(db2) %in% setdiff(names(db2[db2ind]), names(db[dbind]))) # Find all the non-other DFs 
db2ind<-db2ind[which(db2ind %ni% remove)]
remove <-which(names(db) %in% setdiff(names(db[dbind]), names(db2[db2ind])))
dbind<-dbind[which(dbind %ni% remove)]
db2names <- names(db2[db2ind])
dbnames <- names(db[dbind])

#set database
d <- list()

#Do main first 
tempdf <-merge(db$main, db2$main, all = TRUE, sort = FALSE)
add <- setdiff(colnames(db2$main), colnames(db$main)) # Identitify all columns that are in db2 but not in db
for(j in 1:length(add)){                              # Add those columns in 
  tempdf[[paste(add[[j]])]] <- rep(NA, nrow(tempdf))
}

tempdf[(nfirst+1):nrow(tempdf), add] <-  db2$main[, add] # Fill in those columns with appropiate data
d[["main"]] <- tempdf # replace main

#Do the remaining dataframes
db2namesshort <- sort(db2names[2:length(db2names)])
dbnamesshort <- sort(dbnames[2:length(dbnames)])

#Index updating
for(i in dbnamesshort){
  print(i)
  db2[[i]]$'_parent_index'=ifelse(db2[[i]]$'_parent_table_name' == "main", db2[[i]]$'_parent_index'+nfirst, db2[[i]]$'_parent_index')
  db2[[i]]$'_index' = length(db[[i]]$'_index') +as.numeric(db2[[i]]$'_index')
}

for(i in dbnamesshort){
  tempdf <-merge(db[[i]], db2[[i]], all = TRUE, sort = FALSE) # Do merge
  add <- setdiff(colnames(db2[[i]]), colnames(db[[i]]))       # Identify new columns to add
  if(length(add)>0){                                          # Looop through columns create dummy column and fill it
    for(j in 1:length(add)){
      tempdf[[paste(add[[j]])]] <- rep(NA, nrow(tempdf))      # Create dummy column
      tempdf[(nrow(db[[i]])+1):nrow(tempdf), add[[j]]] <- db2[[i]][,add[[j]]] # Fill it
    }
  }
  d[[i]] <- tempdf
}


#Index Repairs
d$main$"_index"[(nfirst+1):nrow(d$main)] <- (nfirst+1):nrow(d$main)
# Quick Fix on sold
db2$sold_emp$"_parent_index" <- db2$sold_emp$"_parent_index" + nfirst
db2$sold_emp$"_parent_table_name" <- "main"


#########################################################################################
################# ADD IN MISSING DATA FRAMES AND FILL NON MATCHED COLUMNS ###############

# Store Credit
d$store_credit<-merge(db$store_credit_rep, db2$store_credit_rep, all = TRUE, sort = FALSE)
d$store_credit<-select(d$store_credit, !contains("..."))

# Gifting give
d$gift_give<-merge(db$gift_give_rep, db2$gift_give_rep, all = TRUE, sort = FALSE)
d$gift_give<-select(d$gift_give, !contains("..."))

# Gifting rec
d$gift_rec<-merge(db$gift_rec_rep, db2$gift_rec_rep, all = TRUE, sort = FALSE)
d$gift_rec<-select(d$gift_rec, !contains("..."))

# Work trips
d$safari<-db2$work_trip_rep
d$safari<-select(d$safari, !contains("..."))

# Mwani
d$mwani <-db2$mwani_emp_source
d$mwani<-select(d$mwani, !contains("..."))

# Siblings# Work trips
  d$safari<-db2$work_trip_rep
d$safari<-select(d$safari, !contains("..."))

d$spouse_siblings <-db2$spouse_siblings
d$spouse_siblings <-select(d$spouse_siblings, !contains("..."))


# Check for duplicated indexes
for(i in names(d)){
  if(any(duplicated(d[[i]]$"_index"))){
    print(paste("duplicates", which(duplicated(d[[i]]$"_index"))))
    stop(paste("found duplicated index in", i)) 
  }
}

#Update hhm_detial to hhm_details
names(d)[[which(names(d)=="hhm_detials")]] = "hhm_details"

###################################################
###### RELABLE IMPROPERLY LABLED HOUSEHOLDS #######

d$main$hhh[427] <- "z2wjrv" # There has been a strange error where the household ID has not been asigned correctly

#####################################
###### CHECK EMP RECORDS ############


for (i in 2:length(db2)){
  print(i)
  db2[[i]]<- db2[[i]][order(db2[[i]]$"_index"), ]
}


# Assign additional DFS
d$hhm_check <- db2$hhm_check
d$hhm_check$"_parent_index" <- db2$hhm_check$"_parent_index" + nfirst
d$left <-db2$left
d$left$"_parent_index" <- db2$left$"_parent_index" + nfirst



# FIX INDEXING
updateindexes<-c("self_emp_source", "wage_emp_sources", "animals_emp", "forest_emp", "ag_crop", "gift_give_rep", "gift_rec_rep", "rent_emp", "store_credit_rep", "hhm_check", "left", "hhm_details", "mwani", "spouse_siblings", "safari")

for(i in updateindexes){
  print(i)
  d[[i]]$"_parent_table_name" <- "main"
  for(j in 1:nrow(d[[i]])){
    uuid <- d[[i]]$"_submission__uuid"[j]
    pind<-which(d$main$"_uuid" == uuid)
    d[[i]]$"_parent_index"[j] <-pind
  }
}

#####################################################################
############### DOUBLE CHECK INDEXING ###############################


# CREATE MORE COMPREHENSIVE UNIT TESTS

