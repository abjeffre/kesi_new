#############################
####### Get SURVEY ##########

getSurvey=function(hhID, period, data=d){
  uuid=data$main$'_uuid'[which(data$main$hh_ID==hhID & data$main$period==period)]
  out = list()
  for(i in 1:length(data)){
    coln = names(data[[i]])[which(grepl("uuid", names(data[[i]])))]
    toadd=which(data[[i]][,coln]==uuid)
    if(length(toadd >0)){
      out[[names(data)[i]]]<-data[[i]][toadd,]
    }
  }
  # Clean up main
  out[[1]] <- out[[1]][which((!is.na(out[[1]]) & out[[1]] != 0))]
  return(out)
}


