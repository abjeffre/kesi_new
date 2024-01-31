#################################################
############# CALCULATE GGP #####################



mp <- max(dl$flow$period)

market_activity = list()
for(j in unique(dl$flow$shehia)){
  sum <- c()
  for(i in 2:mp){
    ind = ((dl$flow$shehia==j & dl$flow$period==i) & dl$flow$main %in% dl$cash_in[-which(dl$cash_in == "rent")]) 
    observations<-length(unique(dl$flow$hhID[(dl$flow$period==i & dl$flow$shehia==j)]))
    print(paste(observations, j, i))
    sum <- c(sum, sum(dl$flow$value[ind]*1/observations, na.rm =T))
  }
  market_activity[[j]]<-sum 
}


plot(market_activity[[1]]+market_activity[[2]]+ market_activity[[3]]+market_activity[[4]], type = "l")
plot(market_activity[[2]], type = "l")

# Now I need to add them all together - standarize and seperate again.

# Make a quick one that just measures the economic status of kifundi and msuka
msu_and_kif <- c(market_activity[[1]] + market_activity[[2]])
msu_and_kif<- standardize(msu_and_kif)




all <-  c(market_activity[[1]], market_activity[[2]], market_activity[[3]], market_activity[[4]])
all_sd <-standardize(all)


# 

