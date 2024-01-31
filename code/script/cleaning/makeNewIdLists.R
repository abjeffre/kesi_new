############################################################################
################# MAKE NEW IDS #############################################


#construct dataframes for exporting
hhhead <- data.frame(hh_ID = c(d$main$hh_ID[1:nfirst],hhIDKondeManda),
                     hhh = c(d$main$hhm_hhh[1:nfirst],uhhKondeManda),
                     shehia = c(d$main$shehia[1:nfirst],newshehia)
)# PLACE HOLDER YOUR MUST CHANGE
# Remove the false Mohammed Mkubwa Nassor

#add in marker for the wave they were added
hhhead$wave <- c(rep(1, 66), rep(2, 56), rep(3, nrow(hhhead)-(66+56))) 
hhhead<- hhhead[-which(hhhead$hhh=="Moh'd Mkubwa Nassor"), ]

# Save
write.csv(hhhead, "survey_media/hhhead.csv", row.names = FALSE)



hhmembers <- data.frame(hhm_ID = d$hhm_details$hhm_ID,
                        hhm = d$hhm_details$hhm_name,
                        children = as.numeric(d$hhm_details$hhm_children),
                        marital = d$hhm_details$hhm_marital,
                        wives = d$hhm_details$hhm_wives,
                        hus_wives = d$hhm_details$hhm_husbnads_wives,
                        education = d$hhm_details$hhm_education,
                        gender =  d$hhm_details$hhm_gender
)

# Add in Keys
hhmembers$shehia = NA
hhmembers$hhh = NA
hhmembers$hh_ID = NA
for(i in 1:nrow(hhmembers)){
  hhmembers$shehia[i] = d$main$shehia[d$main$"_uuid" == d$hhm_details$"_submission__uuid"[i]] # PLACE HOLDER THAT MUST BE CHANGED!
  hhmembers$hhh[i] = d$main$hhm_hhh[d$main$"_uuid" == d$hhm_details$"_submission__uuid"[i]]
  hhmembers$hh_ID[i] = d$main$hh_ID[d$main$"_uuid" == d$hhm_details$"_submission__uuid"[i]]
}


# Remove household members from Husina Mohammed Abdallah



ind = which(hhmembers$hh_ID == d$main$hh_ID[d$main$hhm_hhh == "Husna Muhammed Abdalla"][1])
remove<-ind[which(hhmembers$hhm[ind] %in% c("Sabra Hamadi Omar", "Abdillahi Hamadi Omar"))]
hhmembers<-hhmembers[-remove, ]
# Remove any NA
hhmembers<-hhmembers[-which(hhmembers$hhm=="NA"),]

write.csv(hhmembers, "survey_media/hhmembers.csv", row.names = FALSE)