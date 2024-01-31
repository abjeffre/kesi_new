#######################################################################################
############# FIX WRONG DATES FOR WHEN ABUU AND BAKARI CAME TO GERMANY ################

##########################
### STRATEGY #############

# 1. Identify all the cases where we have duplicate cases where the date is entered as the same day
# 2. One of the probably needs to changed and the other does not.  Identify which one to change
# 2. Generate a prompt that asks what day we should change it to.
# 3. Change the today date, submission times and period to whatever it should be
# 4. Save all of this data so that it can simply be loaded in and replace the old data. 


library(readxl)
# Get Corrections
bakari_germany_corrections <- read_excel("manual_corrections/bakari_germany_corrections.xlsx")
abuu_germany_corrections <- read_excel("manual_corrections/abuu_germany_corrections.xlsx", 
                                       sheet = "EXAMPLE")
germany_corrections <- rbind(bakari_germany_corrections, abuu_germany_corrections)
str(germany_corrections)

library(readxl)

names<-germany_corrections$`Kiongozi ya Kaya`
uhid <- unique(d$main$hhm_hhh[d$main$shehia %in% c("konde", "msuka_magharibi", "kipange", "kifundi") & d$main$period %in% c(31, 32)])
# Dropped out

# FUZZY MATCH NAMES
dist = as.data.frame(matrix(NA, nrow = length(uhid), ncol = length(names)))
cnt <- 1
for(i in names){
  dist[,cnt] = stringdist(tolower(i), tolower(uhid))
  cnt <- cnt+1
}
df = data.frame(names = names, uhid = uhid[apply(dist, 2, which.min)])

df[95, "uhid"] <- "Muhamadi Mussa Hamadi" 


germany_corrections$`Kiongozi ya Kaya` <- df$uhid


###################################
########## GET INSTANCE TO FIX ####
#Dropped out
uhid <- uhid[-123]

out = list()
names = c()
cnt = 1
for(i in uhid){
  ind=which(d$main$hhm_hhh==i)
  ind = ind[which(ind < 8000 & ind > 1000)]
  temp=ind[c(which(duplicated(d$main$today[ind], fromLast = TRUE)), which(duplicated(d$main$today[ind], fromLast = FALSE)))]
  # if it is zero 
  if(length(temp) == 0){
    temp=ind[which(d$main$period[ind] == 36)]
  }
  if(length(temp) == 0){
    temp=ind[which(d$main$period[ind] == 37)]
  }
  out[[cnt]] = temp
  names <- c(names, i)
  cnt = cnt+1
}

for(i in 1:length(uhid)){

  cind = which(germany_corrections$`Kiongozi ya Kaya` == uhid[i])
  order = germany_corrections[cind,c(2,3)]
  # Get the date of the previous interview
  entry_date<-d$main$today[out[[i]][1]]
  entry_period <- d$main$period[out[[i]][1]]
  last_before_ind<-which(d$main$hhm_hhh[1:c(out[[i]][1]-3)] == uhid[i])
  last_before_ind<-last_before_ind[which.max(last_before_ind)]
  last_before_date <- d$main$today[last_before_ind]
  last_before_period <- d$main$period[last_before_ind] 
  last_before_sub_date <- d$main$`_submission_time`[last_before_ind]
  # replace the timestamps for the time they were in germany with 14 days after the last recorded date
  ind=out[[i]][which(order ==1)]
  uuid <- d$main$`_uuid`[ind]
  d$main$start[ind] <- d$main$start[ind]-days(14)
  d$main$end[ind] <- d$main$end[ind] - days(14)
  d$main$`_submission_time`[ind] <- d$main$`_submission_time`[ind] - days(14)
  d$main$period[ind] <- 35
  # Then do this for all other dataframes.
  for(j in 2:length(d)){
    if(names(d)[j] %ni% c("wealth", "gift_rec_rep", "gift_give_rep", "rent_emp", "emp", "self_emp", "store_credit_rep", "spouse_siblings")){
      coltime = names(d[[j]])[which(grepl("submission_time", names(d[[j]])))]
      if(length(coltime)< 1) stop("There is no time here... ask yourself why")
      coluuid = names(d[[j]])[which(grepl("uuid", names(d[[j]])))]
      if(length(coluuid)< 1) stop("There is no ids here... ask yourself why")
      edit_ind<-which(d[[j]][, coluuid] == uuid)
      if(length(edit_ind) > 0){
        d[[j]][edit_ind, coltime] = as.data.frame(d[[j]][edit_ind, coltime]) - as.duration(days(14))
        d[[j]]$period[edit_ind] = 35
      }
    }
  }  
}





