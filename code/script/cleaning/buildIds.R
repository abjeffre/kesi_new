########################################
########## Process data ################

SAVE = FALSE

#######################################
##
## Change Shehia Placeholder
## Find a way to deal with present or absent! 
## Properly integrate functions into normal workflow
## Get a way to add new members to these lists properly
##
#######################################

if(MAKE != TRUE) source("code/script/cleaning/getdate.r")

##########################################
########## CORRECTIONS ####################

d$main$shehia[848] <- "kipange"
##################################



id_maker <- function(n, reserved = "", seed = NA, nchars = NA){
  my_let <- letters
  my_num <- 0:9
  if(is.na(seed) | !is.numeric(seed)) set.seed(as.numeric(as.POSIXlt(Sys.time())))
  if(!is.na(seed) & is.numeric(seed)) set.seed(seed)
  output <- replicate(n, paste(sample(c(my_let, my_num), nchars, replace=TRUE),
                               collapse=''))
  rejected <- duplicated(output) | output %in% reserved |
    substr(output, 1, 1) %in% my_num
  while (any(rejected)) {
    output <- output[-which(rejected)]
    remaining <- n - length(output)
    output <- c(output, replicate(remaining, paste(sample(c(my_let, my_num), nchars,
                                                          replace=TRUE), collapse="")))
    rejected <- duplicated(output) | output %in% reserved |
      substr(output, 1, 1) %in% my_num
  }
  output
}


 hhmID=id_maker(10000, seed = 1984, nchar = 6)
 saveRDS(hhmID, "data/hhmID.RDS")
 hhID=id_maker(100, seed = 1991, nchar = 6, reserved = hhmID)
 saveRDS(hhID, "data/hhID.RDS")

 kondeandkwamanda=id_maker(22, seed = 1991, nchar = 6, reserved = c(hhmID, hhID))
 hhID = c(hhID, kondeandkwamanda)
 saveRDS(hhID, "data/hhID.RDS")

# Adding Salma Juma Makarani
 salmaJuma=id_maker(1, seed = 1991, nchar = 6, reserved = c(hhmID, hhID))
 hhID = c(hhID, salmaJuma)
 saveRDS(hhID, "data/hhID.RDS")

# ADD in new Konde and new mkoani, chokocho, mtambili, and chumbageni
# 30 new in konde
# and 150 new across all the other southern shehia
# Adding Salma Juma Makarani 
 addingsouth=id_maker(184, seed = 1991, nchar = 6, reserved = c(hhmID, hhID, salmaJuma))
 hhID = c(hhID, addingsouth)
 saveRDS(hhID, "data/hhID.RDS")

 
 dropout=id_maker(1, seed = 1991, nchar = 6, reserved = c(hhmID, hhID, addingsouth, salmaJuma))
 hhID = c(hhID, addingsouth, dropout)
 saveRDS(hhID, "data/hhID.RDS")
 
###############################################################
hhmID<-readRDS("data/hhmID.RDS")
hhID<-readRDS("data/hhID.RDS")



#give all households HHID
uhh<-unique(db$main$hhm_hhh[1:nfirst]) # GET ALL UNIQUE HOUSESE FROM FIRST 1:66
luhh <- length(uhh) 
d$main$hh_ID = NA
d$main$hh_ID[1:nfirst] <- hhID[1:nfirst] # Assign for HHDID


# Give new households proper hh_ID
d$main$hh_ID[(nfirst+1):nrow(d$main)] <- d$main$hhh[(nfirst+1):nrow(d$main)]
# After the first two weeks this is going to have locked in!

# New additions
uhh<-unique(d$main$hhm_hhh)
uhh<-unique(uhh)[!is.na((uhh))]
uhhKondeManda<-uhh[(nfirst+1):length(uhh)]
hhIDKondeManda <- hhID[(nfirst+1):length(uhh)]
#put them in

cnt<- 1
newshehia <-c()
for(i in uhhKondeManda){
  ind=which(d$main$hhm_hhh==i)
  d$main$hh_ID[ind] = hhIDKondeManda[cnt]
  d$main$hhh[ind] = hhIDKondeManda[cnt]
  newshehia <- c(newshehia, d$main$shehia[ind][1])
  cnt <- cnt+1
}

###################################################################################
############# Give everybody in a household a unique ID ###########################

#####################################################################
################ ACCOUNT FOR UPDATED DATA ###########################

# Adding food security

namesdb <- names(db$hhm_detials)
namesdb2 <- names(db2$hhm_detials)
new<- namesdb2[which(!namesdb2 %in% namesdb)]
db$hhm_detials[,new] <- NA
db$hhm_detials<-db$hhm_detials[,namesdb2]

# merge dataframes
d$hhm_details <-as.data.frame(rbind(db$hhm_detials, db2$hhm_detials))


d$hhm_details$hh_ID <- d$main$hh_ID[d$hhm_details$"_parent_index"]
uhhid  = unique(d$main$hh_ID)

 cnt <-1
 d$hhm_details$hhm_ID <- NA

 for(i in 1:length(uhhid)){
    print(i)
    inds=which(d$hhm_details$hh_ID == uhhid[i])
    d$hhm_details$hhm_ID[cnt:(cnt+length(inds)-1)] <- hhmID[cnt:(cnt+length(inds)-1)]
   cnt <- length(inds)+cnt
 }

# Make sure main has all household head names
 
 for(i in uhhid){
   ind<-which(d$main$hh_ID == i)
   hhh<-d$main$hhm_hhh[ind][1]
   d$main$hhm_hhh[ind] <- hhh
 }
 
 

# After the first two weeks this is going to have locked in!
 
d$hhm_details$hhm_name[which(d$hhm_details$hhm_name== "Khalfan Hamad Khamis" & d$hhm_details$hh_ID == "i3gqiy")[2]] <- "Omar Hamad Khamis"
 


########################################
########## CORRECTIONS ##################

d$main$shehia[66] <- "kipange"


