################################################################################
################################ GET BASE PLOTS ################################


library(ggplot2)
library(viridis)


getGraphs<-function(shehiaF = c("kifundi", "msuka_magharibi"),
                    data_inc = dl$flow,
                    data_hours = dl$hours,
                    data_prices = dl$prices,
                    data = dl,
                    data_base = d){
  
  
  ########################################
  ############## SEPERATE PLOT LIST ######
  plots <- list()
  
  mp = max(data_base$main$period)
  
  # Make Time Serise List of Data
  incts <- list()
  for(t in 1:max(data_base$main$period)){
    a<-which(data_inc$period == t)
    inc <-data_inc[a, ]
    uhhid <- unique(inc$hhID)
    df<-data.frame(hhid = uhhid,
                   shehia = rep(NA, length(uhhid)),
                   inc = rep(NA, length(uhhid)),
                   by = rep(NA, length(uhhid)),
                   wage = rep(NA, length(uhhid)),
                   self =rep(NA, length(uhhid)),
                   animal_sold = rep(NA, length(uhhid)),
                   slaughtered = rep(NA, length(uhhid))
    )
    for(i in uhhid){
      a<-which(inc$hhID ==i)
      df[df$hhid ==i ,"inc"]=sum(as.numeric(inc$value)[a], na.rm = T)
      df[df$hhid ==i ,"by"]=ifelse(length(unique(inc$by[a])) ==1, unique(inc$by[a]), "both")
      df[df$hhid ==i ,"shehia"]=unique(inc$shehia[a])
      for(j in unique(data_inc$main)){
        a<-which(inc$hhID ==i & inc$main == j)
        df[df$hhid ==i ,j]=sum(as.numeric(inc$value)[a], na.rm = T)
      }#end main
    }#end uhhid
    incts[[t]] <- df  
  }
  
  
  
  ###################################
  ############# BY HOUSEHOLD ########
  
  
  uhhid <- unique(data_inc$hhID)
  
  dfts<-data.frame(hhid = NA,
                   shehia = NA,
                   inc = NA,
                   by = NA,
                   wage = NA,
                   self =NA,
                   animal_sold = NA,
                   slaughtered = NA,
                   forest_sold = NA,
                   forest_sub = NA,
                   uvuvi = NA,
                   fundi = NA,
                   julian = NA,
                   period = NA
  )
  
  bad <-c()
  
  for(i in uhhid){
    a=which(data_inc$hhID== i)
    timestamp <-sort(unique(data_inc[a, "julian"]))
    shehia <- unique(data_inc[a,"shehia"])[1]
    cnt <- 1
    if(length(timestamp) >8){bad <- c(bad, i)}
    for(t in timestamp){
      ind=which(data_inc[,"hhID"]== i & data_inc[,"julian"]==t)
      df<-data.frame(hhid = i,
                     shehia = shehia,
                     inc = sum(data_inc[ind, "value"], na.rm = T),
                     by = unique(data_inc[ind, "by"])[1],
                     wage = sum(data_inc[ind, "value"][data_inc[ind, "main"] == "wage"]),
                     self =sum(data_inc[ind, "value"][data_inc[ind, "main"] == "self"]),
                     animal_sold = sum(data_inc[ind, "value"][data_inc[ind, "main"] == "animal_sold"]),
                     slaughtered = sum(data_inc[ind, "value"][data_inc[ind, "main"] == "slaughtered"]),
                     forest_sold = sum(data_inc[ind, "value"][data_inc[ind, "main"] == "forest_sold"]),
                     forest_sub = sum(data_inc[ind, "value"][data_inc[ind, "main"] == "forest_sub"]),
                     uvuvi = sum(data_inc[ind, "value"][data_inc[ind, "main"] == "uvuvi"]),
                     fundi = sum(data_inc[ind, "value"][data_inc[ind, "main"] == "fundi"]),
                     julian = t,
                     period = cnt
      )
      dfts<-rbind(dfts, df)
      cnt <- cnt+1
    }#end time stamp  
  }#end ids
  
  #Get means for coloring 
  dfts <- dfts[2:nrow(dfts),]
  dfts$inc[which(is.na(dfts$inc))]
  dfts$std <- NA
  dfts$mean <- NA
  for(i in uhhid){
    a<-which(dfts$hhid==i)
    dfts$mean[a] <- mean(dfts[a, "inc"])
    dfts$std[a] <- sd(dfts[a, "inc"])
  
  
  #Lines
  plots[["hhincomeline"]]= ggplot(dfts[dfts$shehia%in%shehiaF, ], aes(x=julian, y= inc, group = hhid, color = log(std/mean)))+
    geom_line()+
    geom_point(size =.75) +
    scale_color_viridis(discrete=FALSE, alpha = .5) +
    theme_classic() +
    coord_cartesian(ylim=c(0, 3500000)) 
 }
  
  #dots
  dfts$id <- as.integer(as.factor(dfts$hhid))
  plots[["hhincomedot"]]=ggplot(dfts[dfts$shehia%in%shehiaF, ], aes(x=period, y= reorder(as.factor(hhid), mean), group = as.factor(id), size = inc, color = (mean/std)),  ylim=c(0, 70)) +
    geom_point() +
    scale_color_viridis(discrete=FALSE, alpha = .5) +
    theme_bw() +
    ggtitle(paste0("Household Production", shehiaF))
  #
  # df <- data.frame(time=NA, value=NA, id=NA)
  # for(i in 1:length(incts)){
  #   n=length(unique(incts[[i]]$hhid))
  #   temp <- data.frame(time=rep(i, n), value=incts[[i]]$inc, id=incts[[i]]$hhid)
  #   df <- rbind(df, temp)
  #   
  # }
  # df <- incts[[1]]


  ########################################################################################
  ################ REVENUE ###############################################################

  
  check <- c("inc", "wage", "self", "animal_sold", "slaughtered","ag_sold", "ag_sub", "forest_sold", "forest_sub", "fundi", "uvuvi", "sold", "rent")
  
  
  
  uhhid <- unique(data_inc$hhID)
  types = unique(data_inc$main)
  nt<- length(types) # Get number of columns
  temp = matrix(NA, nrow = 1, ncol =nt) # make matrix
  dft <- as.data.frame(temp)
  colnames(dft) <- types
  dft$shehia = NA
  dft$hhid = NA
  dft$by = NA
  dft$julian = NA
  dft$period = NA
  
  
  bad <-c()
  
  for(i in uhhid){
    a=which(data_inc$hhID== i)
    timestamp <-sort(unique(data_base$main$julian[which(data_base$main$hh_ID==i)]))
    shehia <- unique(data_inc[a,"shehia"])[1]
    cnt <- 1
    if(length(timestamp) >8){bad <- c(bad, i)}
    for(t in timestamp){
      ind=which(data_inc[,"hhID"]== i & data_inc[,"julian"]==t)
      temp = matrix(NA, nrow = 1, ncol =nt) # make matrix
      df <- as.data.frame(temp)
      colnames(df) <- types
      for(j in types){
        df[,j]=sum(data_inc[ind, "value"][data_inc[ind, "main"] == j])
      }
      
      df$shehia = shehia
      df$hhid = i
      df$by = unique(data_inc[ind, "by"])[1]
      df$julian = t
      df$period = cnt
      dft<-rbind(dft, df)
      cnt <- cnt+1
    }#end time stamp  
  }#end ids
  
  dft <- dft[2:nrow(dft),]
  
  
  # Generate In flow
  cash_in <- c("wage", "self", "fundi", "uvuvi", "ag_sold", "ag_sub", "animal_sold", "forest_sold", "rent", "sold")
  sub_in <- c("ag_sub", "slaughtered", "forest_sub")
  cred_in <- c("store_credit", "debt_borrowed", "credit_returned")
  sav_in <- c("vsla_withdraw", "saving_withdraw")
  gift_in <-c("gift_rec")
  gift_out <-c("gift_give")
  sav_out <- c("saving_save", "vsla_save")
  cred_out <- c("credit_lent", "debt_returned")
  
  
  # Make consumption graph
  
  init <- function(x){
    df <- data.frame(matrix(ncol = 4, nrow =x ))
    colnames(df) <- c("time", "id", "value", "shehia")
    return(df)
  }
  
  dfc <- init(0)
  
  
  for(i in uhhid){
    a=which(data_base$con$hhID== i)
    timestamp <-sort(unique(data_base$con[a, "julian"]))
    shehia <- unique(data_base$con[a,"shehia"])[1]
    cnt <- 1
    for(t in timestamp){
      ind=which(data_base$con[,"hhID"]== i & data_base$con[,"julian"]==t)
      temp = matrix(NA, nrow = 1, ncol =nt) # make matrix
      df <- init(1)
      df$value <- sum(data_base$con[ind, which(names(data_base$con) == "con_food"):which(names(data_base$con) == "con_kukodishwa")], na.rm =  T)
      df$id  = i
      df$time = t
      df$shehia <- data_base$con$shehia[ind]
      dfc<-rbind(dfc, df)
    }#end time stamp  
  }#end ids
  
  dfc$std <- NA
  dfc$mean <- NA
  for(i in uhhid){
    a<-which(dfc$id==i)
    dfc$mean[a] <- mean(dfc[a, "value"], na.rm = T)
    dfc$std[a] <- sd(dfc[a, "value"], na.rm = T)
  }
  
# CONSUMPTION DOTS
  dfc$hhid<-dfc$id
  dfc$id <- as.integer(as.factor(dfc$hhid))
  plots[["hhcondot"]]=ggplot(dfc[dfc$shehia%in%shehiaF, ], aes(x=time, y= reorder(as.factor(hhid), mean), group = as.factor(id), size = value, color = (mean/std)),  ylim=c(0, 70)) +
    geom_point() +
    scale_color_viridis(discrete=FALSE, alpha = .5) +
    theme_bw() +
    ggtitle(paste("Consumption by household in", shehiaF))
  #
  
  
  
  # THIS MUST BE REPLACED WITH A PROPER ESTIMATION STRATEGY
  dft[is.na(dft)] <- 0
  

  expenditures <- dfc$value + dft[,gift_out]+ rowSums(dft[,cred_out])# + rowSums(dft[,sav_out])  # Get Expenditures
  revenue <- rowSums(dft[, cash_in]) + dft[,gift_in]  + rowSums(dft[,cred_in])# + rowSums(dft[,sav_in]) # Get Revenue
  dft$profit<-(revenue - expenditures)
  dft$mean <- NA
  dft$std<- NA
  for(i in uhhid){
    a<-which(dft$hhid==i)
    dft$mean[a] <- sum(dft[a, "profit"])
    dft$std[a] <- sd(dft[a, "profit"])
  }
  
  df<-dft[-166,]
  
  plots[["revenuejulian"]]=ggplot(df[df$shehia%in%shehiaF, ], aes(x=julian, y= profit, group = hhid, color = shehia))+
    geom_line()+
    geom_point(size =.75) +
    coord_cartesian(ylim=c(-2500000, 2500000)) +
    #scale_color_viridis(discrete=FALSE, alpha = .5) +
    theme_classic() +
    geom_hline(yintercept = 0, color = "red") +
    ggtitle(paste("Per Period Revenue in", shehiaF))
  
  
  ####################################
  ######### CUM SUM ##################
  
  cumsum <-c()
  for(i in unique(df$hhid)){
    ind <- which(df$hhid==i)
    cumsum<-c(cumsum, cumsum(df$profit[ind]))
  }
  
  df$cumsum <- cumsum
  
  # Get time
  med_time <-as.POSIXct.Date(c())
  for(i in 1:mp){d
    a<-median(d$main$today[d$main$period==i])
    med_time <- c(med_time, a)
  }
  
  df$period <- med_time[df$period]
  
  plots[["netchange"]]=ggplot(df[df$shehia%in%shehiaF, ], aes(x=julian, y= cumsum, group = hhid, color = shehia))+
    geom_line(alpha = .5, size =1.5)+
    geom_point(size =1) +
    #coord_cartesian() +
    theme_classic() +
    geom_hline(yintercept = 0, color = "red") +
    ggtitle(paste("Mabadaliko ya Mali")) +
    ylab("Mabadaliko ya Mali") +
    xlab("Tarehe tangu tumeanza") +
    theme_bw() + 
    geom_vline(xintercept= as.POSIXct("2022-05-03 01:00:00 CET"), color = "red") +
    geom_vline(xintercept= as.POSIXct("2022-07-01 01:00:00 CET"), color = "red")+
    annotate("rect", xmin = as.POSIXct("2022-03-28 01:00:00 CET"), xmax = as.POSIXct("2022-06-01 01:00:00 CET"), ymin = 0, ymax = 210000,
             alpha = .2) +
    annotate("text", x = as.POSIXct("2022-04-28 01:00:00 CET"), y = 210000, label = "Rainy Season", size = 3) +
    annotate("text", x = as.POSIXct("2022-05-20 01:00:00 CET"), y = 190000, label = "Eid el Fitr", size = 3, color  = "red") +
    annotate("text", x = as.POSIXct("2022-07-20 01:00:00 CET"), y = 190000, label = "Eid el Adha", size = 3, color  = "red") +
    ylab("Themani ya Mali")#+
  #scale_color_viridis(discrete=FALSE, alpha = .5) 
  
  
  
  ##########################################
  ########## CUM SUM WITH WEALTH ###########
  
  cumsum <-c()
  for(i in unique(df$hhid)){
    ind <- which(df$hhid==i)
    cumsum<-c(cumsum, cumsum(df$profit[ind]))
  }
  
  df$cumsum <- cumsum
  
  # Construct Wealth!
  # Should be moved into cleaning!
  values <-select(d$main, ends_with("val"))
  items  <- select(d$main, ends_with('num'))
  items <-select(items, !contains("ag_"))
  items <-select(items, !contains("animals_"))
  items <-select(items, !contains("gift_"))
  items <-select(items, !contains("work_trip_"))
  d$main$wealth_total=rowSums(items*values[,-which(names(values) == "wealth_other_val")], na.rm =T) + values$wealth_other_val
  
  for(i in unique(df$hhid)){
    temp<-d$main$wealth_total[which(d$main$hh_ID ==i)]
    temp<-temp[!is.na(temp)][1]
    df$cumsum[which(df$hhid==i)]<-df$cumsum[which(df$hhid==i)]+temp
  } 
  
  
  
  plots[["netchange_wealth"]]=ggplot(df[df$shehia%in%shehiaF, ], aes(x=julian, y= cumsum, group = hhid, color = shehia))+
    geom_line(alpha = .5, size =1.5)+
    geom_point(size =1) +
    coord_cartesian(ylim=c(-6000000, 420000000)) +
    theme_classic() +
    geom_hline(yintercept = 0, color = "red") +
    ggtitle(paste("Net change in wealth in villages"))+
    theme_bw() + 
    scale_x_continuous(name="Date",
                       breaks=c(1,30,61,90,120,150,180,210,240),
                       labels=c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")) +
    geom_vline(xintercept= 7*14, color = "red") +
    geom_vline(xintercept= 12*14, color = "red")+
    annotate("rect", xmin = 2.8*14, xmax = 8.8*14, ymin = 0, ymax = 1700000000,
             alpha = .2) +
    annotate("text", x = 5*14, y = 410000000, label = "Rainy Season", size = 3) +
    annotate("text", x = 8.35*14, y = 410000000, label = "Eid el Fitr", size = 3, color  = "red") +
    annotate("text", x = 13.55*14, y = 410000000, label = "Eid el Adha", size = 3, color  = "red") +
    ylab("Wealth")
  
  plots[["netchange_wealth_poor"]]=ggplot(df[df$shehia%in%shehiaF, ], aes(x=julian, y= cumsum, group = hhid, color = shehia))+
    geom_line(alpha = .5, size =1.5)+
    geom_point(size =1) +
    coord_cartesian(ylim=c(-600000, 22000000)) +
    theme_classic() +
    geom_hline(yintercept = 0, color = "red") +
    ggtitle(paste("Net change in wealth in villages - the poor")) +
  theme_bw() + 
    scale_x_continuous(name="Date",
                       breaks=c(1,30,61,90,120,150,180,210,240),
                       labels=c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")) +
    geom_vline(xintercept= 7*14, color = "red") +
    geom_vline(xintercept= 12*14, color = "red")+
    annotate("rect", xmin = 2.8*14, xmax = 8.8*14, ymin = 0, ymax = 170000000,
             alpha = .2) +
    annotate("text", x = 5*14, y = 17000000, label = "Rainy Season", size = 3) +
    annotate("text", x = 8.35*14, y = 17000000, label = "Eid el Fitr", size = 3, color  = "red") +
    annotate("text", x = 13.55*14, y = 17000000, label = "Eid el Adha", size = 3, color  = "red") +
    ylab("Wealth")
  
  #+
  
  
  
#################################################
################ PROP HARVESTING X ##############

  
  ###########################################################################
  ################# Proportion Getting X ####################################
  
  check=dl$revenue
  library(ggplot2)
  
  nt<-length(incts)
  
  nt<-length(incts)
  init <- function(x){
    df <- data.frame(matrix(ncol = 3, nrow =x ))
    colnames(df) <- c("time", "variable", "value")
    return(df)
  }
  
  df <- init(0)
  
  for(t in 1:nt){
    for(i in check[2:length(check)]){
      temp <- init(1)
      temp$time = t
      temp$variable = i
      temp$value <-sum((incts[[t]][,i]> 0) &(incts[[t]]$shehia%in% shehiaF))/length(incts[[t]][incts[[t]]$shehia%in%shehiaF,i])
      df<-rbind(df, temp)
    }
  }
  
  
  library(viridis)
  library(hrbrthemes)
  plots[["percent_incclass"]]=ggplot(df, aes(x=time, y=value, fill=variable)) + 
    geom_area(alpha=0.6 , size=.5, colour="white") +
    scale_fill_viridis(discrete = T) +
    theme_ipsum() + 
    ggtitle(paste0("Percent of Sample working in X, in ", shehiaF))
  
  
 ################
 ### Total Inc ##

  nt<-length(incts)
  
  nt<-length(incts)
  init <- function(x){
    df <- data.frame(matrix(ncol = 9, nrow =x ))
    colnames(df) <- c("time", "variable", "value", "0%", "25%", "50%", "75%", "100%", "shehia")
    return(df)
  }
  
  df <- init(0)
  
  for(t in 1:nt){
    for(i in check[2:length(check)]){
      temp <- init(1)
      temp$time = t
      temp$variable = i
      temp$value <-sum(incts[[t]][,i][(incts[[t]][,i]> 0) & (incts[[t]]$"shehia"%in%shehiaF)])
      temp[4:8]<-quantile(incts[[t]][,i][(incts[[t]][,i]> 0) & (incts[[t]]$"shehia"%in%shehiaF)], na.rm = T)
      df<-rbind(df, temp)
    }
  }
  
  
  # Get time
  med_time <-as.POSIXct.Date(c())
  for(i in 1:mp){d
    a<-median(d$main$today[d$main$period==i])
    med_time <- c(med_time, a)
  }
  
  df$time <- med_time[df$time]
  
  # RENAME IN SWAHILI
  df$variable[df$variable=="ag_sold"] <- "kilimo chakula"
  df$variable[df$variable=="ag_sub"] <- "kilimo pesa"
  df$variable[df$variable=="animal_sold"] <- "ufugaji pesa"
  df$variable[df$variable=="slaughtered"] <- "ufugaji chakula"
  df$variable[df$variable=="forest_sub"] <- "msitu matumizi"
  df$variable[df$variable=="forest_sold"] <- "msitu pesa"
  df$variable[df$variable=="rent"] <- "codi"
  df$variable[df$variable=="sold"] <- "kuuza"
  df$variable[df$variable=="wage"] <- "mshahara"
  df$variable[df$variable=="self"] <- "kujiajiri"
  
  
  # RENAME IN English
  df$variable[df$variable=="ag_sold"] <- "Ag Subsitence"
  df$variable[df$variable=="ag_sub"] <- "Ag Cash"
  df$variable[df$variable=="animal_sold"] <- "Livestock Cash"
  df$variable[df$variable=="slaughtered"] <- "Livestock Subsistence"
  df$variable[df$variable=="forest_sub"] <- "Forest Subsistence"
  df$variable[df$variable=="forest_sold"] <- "Forest Cash"
  df$variable[df$variable=="rent"] <- "Rentals"
  df$variable[df$variable=="sold"] <- "Pawns"
  df$variable[df$variable=="wage"] <- "Salary"
  df$variable[df$variable=="self"] <- "Self-Employment"
  df$variable[df$variable=="milk"] <- "Milk"
  df$variable[df$variable=="uvuvi"] <- "Fishing"
  df$variable[df$variable=="fundi"] <- "Crafting/Repairs"
  
  
    
  library(hrbrthemes)
  
  plots[["total_income"]]=ggplot(df, aes(x=time, y=value, fill=variable)) + 
    geom_area(alpha=0.6 , size=.5, colour="white") +
    scale_fill_viridis(discrete = T) +
    ggthemes::theme_clean() +
    xlab("Time") +
    ylab("Total Income") +
    ggtitle(paste0("Seasonality in Income")) +
    annotate("rect", xmin = as.POSIXct("2022-05-25 01:00:00 CET"), xmax = as.POSIXct("2022-07-15 01:00:00 CET"), ymin = 0, ymax = 34000000,
             alpha = .2) #+
  
    # geom_vline(xintercept= as.POSIXct("2022-05-03 01:00:00 CET"), color = "red") +
    # geom_vline(xintercept= as.POSIXct("2022-07-01 01:00:00 CET"), color = "red")+
    # annotate("text", x = as.POSIXct("2022-04-28 01:00:00 CET"), y = 25100000, label = "Rainy Season", size = 3) +
    # annotate("text", x = as.POSIXct("2022-05-20 01:00:00 CET"), y = 23900000, label = "Eid el Fitr", size = 3, color  = "red") +
    # annotate("text", x = as.POSIXct("2022-07-20 01:00:00 CET"), y = 23900000, label = "Eid el Adha", size = 3, color  = "red") 
    
  
  

  
#################################################################################
############################ MISC STUFF #########################################
  
  
  library(ggplot2)
  
  nt<-length(incts)
  
  nt<-length(incts)
  init <- function(x){
    df <- data.frame(matrix(ncol = 7, nrow =x ))
    colnames(df) <- c("time", "value", "0%", "25%", "50%", "75%", "100%")
    return(df)
  }
  
  df <- init(0)
  
  
  
  entropy <- function(x) -sum(ifelse(x == 1 | x== 0, 0, x*log(x)))
  
  for(t in 1:nt){
    temp <- init(1)
    temp$time = t
    dat<-incts[[t]]
    dat2 <- incts[[t]][, which(names(incts[[t]]) == "wage"):which(names(incts[[t]]) == "sold")]
    dat2 <- dat2/dat$inc
    dat2<-apply(dat2, 1, entropy)
    temp$value <-median(dat2, na.rm = T)
    temp[3:7]<-quantile(dat2, na.rm = T)
    df<-rbind(df, temp)
  }
  
  
  names(df)[4] <- "lower"
  names(df)[6] <- "upper"
  plots[["diversification"]] <- ggplot(df, aes(x=time, y=value)) +
    geom_ribbon(aes(ymin=lower, ymax=upper), 
                alpha=0.3, colour=NA) +
    geom_line(size = 1)
  
  
  
  
  ##############################################################################
  ##################### Household Inqueality ###################################
  
  
  
  library(ggplot2)
  
  nt<-length(incts)
  
  nt<-length(incts)
  init <- function(x){
    df <- data.frame(matrix(ncol = 7, nrow =x ))
    colnames(df) <- c("time", "value", "0%", "25%", "50%", "75%", "100%")
    return(df)
  }
  
  df <- init(0)
  
  entropy <- function(x) -sum(ifelse(x == 1 | x== 0, 0, x*log(x)))
  
  for(t in 1:nt){
    temp <- init(1)
    temp$time = t
    dat<-incts[[t]]
    dat2<-theil(dat$inc)
    temp$value <-median(dat2, na.rm = T)
    temp[3:7]<-quantile(dat2, na.rm = T)
    df<-rbind(df, temp)
  }
  
  
  names(df)[4] <- "lower"
  names(df)[6] <- "upper"
  plots[["hhineq"]]<-ggplot(df, aes(x=time, y=value)) +
    geom_line(size = 1)
  
  print("got here 1")
  
  #############################################################################
  ##################### INEQUALITY IN CAREER TYPES ############################
  
  
  
  library(ggplot2)
  
  nt<-length(incts)
  
  nt<-length(incts)
  init <- function(x){
    df <- data.frame(matrix(ncol = 3, nrow =x ))
    colnames(df) <- c("time", "value", "variable")
    return(df)
  }
  
  df <- init(0)
  
  for(t in 1:nt){
    temp <- init(1)
    temp$time = t
    dat<-incts[[t]]
    dat2 <- incts[[t]][, which(names(incts[[t]]) == "wage"):which(names(incts[[t]]) == "sold")]
    tnames <- names(dat2)
    dat2 <- lengthen(dat2)
    dat2$group <- tnames[dat2$group]
    dat2 <- dat2[dat2$value > 0, ]
    dat2 <- dat2[!is.na(dat2$value), ]
    dat2$group <- as.factor(dat2$group)
    names <- levels(dat2$group)
    a<-theil_d(dat2$value, dat2$group)
    a<- a$TheilBet + a$TheilContribWith
    dt<-data.frame(matrix(ncol = length(names), nrow = 1))
    names(dt) <- names
    dt[1, ] <- a
    for(i in 1:length(a)){ 
    # for(i in 1:ncol(a)){ old version
      temp$value <-a[i]
      # temp$value <-a[,i] Old version
      temp$time <- t
      temp$variable <- names[i]
      df<-rbind(df, temp)
    }
  }
  
  
  
  
  plots[["inequality_incclass"]]=ggplot(df, aes(x=time, y=value, colour=as.factor(variable))) +
    geom_line(aes(color = variable), size = 1) + ggtitle("Contribution to inequality by sector")
  
  
  
  
  
  ################################################################################
  ############################## CONSUMPTION #####################################
  
  # Make consumption graph
  
  
  nt<-length(incts)
  
  contemp <-data_base$con[, which(names(data_base$con) == "con_food"):which(names(data_base$con) == "con_other")]
  check <- names(contemp)
  
  nt<-max(data_base$con$period)
  init <- function(x){
    df <- data.frame(matrix(ncol = 3, nrow =x ))
    colnames(df) <- c("time", "variable", "value")
    return(df)
  }
  
  df <- init(0)
  
  for(t in 1:nt){
    for(i in check[1:length(check)]){
      temp <- init(1)
      temp$time = t
      temp$variable = i
      dftemp <- contemp[(data_base$con$period==t) & (data_base$main$shehia%in%shehiaF), i]
      temp$value <-sum(dftemp, na.rm =T)
      df<-rbind(df, temp)
    }
  }
  
  
  plots[["con_disag"]]=ggplot(df, aes(x=time, y=value, fill=variable)) + 
    geom_area(alpha=0.6 , size=.5, colour="white") +
    scale_fill_viridis(discrete = T) +
    theme_ipsum() + 
    ggtitle("Consumption") +
    ylab("Total consumption")
  

  print("got here 2")
  ######################################################
  ################ STORE CREDIT ########################
  
  
  
  nt<-max(data_base$con$period)
  init <- function(x){
    df <- data.frame(matrix(ncol = 3, nrow =x ))
    colnames(df) <- c("time", "variable", "value")
    return(df)
  }
  
  df <- init(0)
  check = shehiaF
  
  
  df <- init(0)
  for(t in 1:nt){
    for(i in check){
      temp <- init(1)
      temp$time = t
      temp$variable = i
      dftemp <- data_base$store_credit$value[data_base$store_credit$period==t & data_base$store_credit$shehia == i]
      temp$value <-sum(dftemp, na.rm =T)#/length(dftemp)
      df<-rbind(df, temp)
    }
  }
  
  plots[["store_credit"]]=ggplot(df, aes(x = time, y = value)) + 
    geom_line(aes(color = variable), size = 1) +
    theme_minimal() +
    coord_cartesian(ylim=c(0, 1000000)) + ggtitle(paste("Total store credit in", shehiaF))
  

  
######################################################
############ PERCENT BORROWING #######################
  
  
  df <- init(0)
  for(t in 1:nt){
    for(i in shehiaF){
      temp <- init(1)
      temp$time = t
      temp$variable = i
      dftemp <- (data_base$store_credit$value>0)[data_base$store_credit$shehia==i & data_base$store_credit$period ==t]
      temp$value <-sum(dftemp, na.rm =T)/sum(data_base$main$period==t & data_base$main$shehia == i)
      df<-rbind(df, temp)
    }
  }
  
  plots[["storecredit_percent"]]<- ggplot(df, aes(x = time, y = value)) + 
    geom_line(aes(color = variable), size = 1) +
    theme_minimal() + ylim(0, 1) + ggtitle(paste("percent getting store credit", shehiaF))
  
  
  
  
  
  ##########################################
  ############### CREDITORS ################
  
  

  
  df <- init(0)
  for(t in 1:nt){
    for(i in shehiaF){
      temp <- init(1)
      temp$time = t
      temp$variable = i
      dftemp <- data_base$debt$creditor_amount_lent[data_base$debt$period==t & data_base$debt$shehia == i]>0
      temp$value <-sum(dftemp, na.rm =T)/sum(data_base$main$shehia==i & data_base$main$period==t)
      df<-rbind(df, temp)
    }
  }
  
  plots[["percent_lending"]]=ggplot(df, aes(x = time, y = value)) + 
    geom_line(aes(color = variable), size = 1) +
    theme_minimal() + ylim(0, 1) + ggtitle("Percent lending credit") + ylab("percent lending")
  
  
  
  ############################################
  ########## CREDIT AMOUNT GIVEN #############
  
  
  
  df <- init(0)
  for(t in 1:nt){
    for(i in shehiaF){
      temp <- init(1)
      temp$time = t
      temp$variable = i
      dftemp <- data_base$debt$creditor_amount_lent[data_base$debt$period==t & data_base$debt$shehia == i]
      temp$value <-sum(dftemp, na.rm =T)
      df<-rbind(df, temp)
    }
  }
  
  plots[["credit_given"]]=ggplot(df, aes(x = time, y = value)) + 
    geom_line(aes(color = variable), size = 1) +
    theme_minimal() + coord_cartesian(ylim=c(0, 500000)) + ggtitle("Total amount of credit given")
  
  
  
  df <- init(0)
  for(t in 1:nt){
    for(i in shehiaF){
      temp <- init(1)
      temp$time = t
      temp$variable = i
      dftemp <- data_base$debt$creditor_amout_returned[data_base$debt$period==t & data_base$debt$shehia == i]
      temp$value <-sum(dftemp, na.rm =T)
      df<-rbind(df, temp)
    }
  }
  
  
  plots[["credit_returned"]]=ggplot(df, aes(x = time, y = value)) + 
    geom_line(aes(color = variable), size = 1) +
    theme_minimal() + coord_cartesian(ylim=c(0, 500000)) + ggtitle("Total amount of credit returned")
  
  ##########################################
  ############### Debtor ################
  
  df <- init(0)
  for(t in 1:nt){
    for(i in shehiaF){
      temp <- init(1)
      temp$time = t
      temp$variable = i
      dftemp <- data_base$debt$debtor_amount_borrowed[data_base$debt$period==t & data_base$debt$shehia == i]>0
      temp$value <-sum(dftemp, na.rm =T)/length(dftemp)
      df<-rbind(df, temp)
    }
  }
  
  plots[["percentborrowing"]] =ggplot(df, aes(x = time, y = value)) + 
    geom_line(aes(color = variable), size = 1) +
    theme_minimal() + ylim(0, 1)+ylab("percent borrowing") + ggtitle("Percent Borrowing")
  
  ############################################
  ########## Debtor AMOUNT Borrowed #############
  
  # Amount borrowed sum
  df <- init(0)
  for(t in 1:nt){
    for(i in shehiaF){
      temp <- init(1)
      temp$time = t
      temp$variable = i
      dftemp <- data_base$debt$debtor_amount_borrowed[data_base$debt$period==t & data_base$debt$shehia == i]
      temp$value <-sum(dftemp, na.rm =T)
      df<-rbind(df, temp)
    }
  }
  
  plots[["debt_borrowed"]]=ggplot(df, aes(x = time, y = value)) + 
    geom_line(aes(color = variable), size = 1) +
    theme_minimal() + coord_cartesian(ylim=c(0, 500000)) + ggtitle("Average debt borrowed")
  
  
  
  
  # Amount returned SUM
  df <- init(0)
  for(t in 1:nt){
    for(i in shehiaF){
      temp <- init(1)
      temp$time = t
      temp$variable = i
      dftemp <-data_base$debt$debtor_amount_returned[data_base$debt$period==t & data_base$debt$shehia == i]
      temp$value <-sum(dftemp, na.rm =T)
      df<-rbind(df, temp)
    }
  }
  
  plots[["debt_returned"]]=ggplot(df, aes(x = time, y = value)) + 
    geom_line(aes(color = variable), size = 1) +
    theme_minimal() + coord_cartesian(ylim=c(0, 500000)) + ggtitle("Average debt returned")
  
  print("got here 3")
  
  ######################################################################
  ################## GIFTS #############################################
  
  
  

  df <- init(0)
  for(t in 1:nt){
    for(i in shehiaF){
      temp <- init(1)
      temp$time = t
      temp$variable = i
      dftemp <- data_base$gift_give$hhID[data_base$gift_give$period==t & data_base$gift_give$shehia == i]
      temp$value <-length(unique(dftemp))/sum(data_base$main$shehia==i & data_base$main$period==t)
      df<-rbind(df, temp)
    }
  }
  
  plots[["gift_give_perc"]]=ggplot(df, aes(x = time, y = value)) + 
    geom_line(aes(color = variable), size = 1) +
    theme_minimal() + ylim(0, 1) + ggtitle("Percent of Household Giving Gifts")
  
  
  
  
  df <- init(0)
  for(t in 1:nt){
    for(i in shehiaF){
      temp <- init(1)
      temp$time = t
      temp$variable = i
      dftemp <- data_base$gift_give$value[data_base$gift_give$period==t & data_base$gift_give$shehia == i]
      temp$value <-sum(dftemp, na.rm = T)
      df<-rbind(df, temp)
    }
  }
  
  plots[["value"]] <- ggplot(df, aes(x = time, y = value)) + 
    geom_line(aes(color = variable), size = 1) +
    theme_minimal() + coord_cartesian(ylim=c(0, 1000000)) +ggtitle("Total value of gifts given")
  
  
  
  
  ######################################################################################
  ################## GIFTS RECIEVE ########################################################
  
  
  df <- init(0)
  for(t in 1:nt){
    for(i in shehiaF){
      temp <- init(1)
      temp$time = t
      temp$variable = i
      dftemp <- data_base$gift_rec$hhID[data_base$gift_rec$period==t & data_base$gift_rec$shehia == i]
      temp$value <-length(unique(dftemp))/sum(data_base$main$shehia==i & data_base$main$period==t) 
      df<-rbind(df, temp)
    }
  }
  
  plots[["gift_rec_perc"]]=ggplot(df, aes(x = time, y = value)) + 
    geom_line(aes(color = variable), size = 1) +
    theme_minimal() + ylim(0, 1) + ggtitle("Percent of Household Recieving Gifts")
  
  
  
  
  
  df <- init(0)
  for(t in 1:nt){
    for(i in shehiaF){
      temp <- init(1)
      temp$time = t
      temp$variable = i
      dftemp <- data_base$gift_rec$value[data_base$gift_rec$period==t & data_base$gift_rec$shehia == i]
      temp$value <-sum(dftemp, na.rm = T)
      df<-rbind(df, temp)
    }
  }
  
  plots[["value"]] <- ggplot(df, aes(x = time, y = value)) + 
    geom_line(aes(color = variable), size = 1) +
    theme_minimal() + coord_cartesian(ylim=c(0, 1000000)) + ggtitle("Total value of gifts recieved")
  
  
    
################################################################################
####################### INC_MISC #################################################
  
  ##########################################################################
  ###################### AG CROP TYPE ######################################
  
  dc<-init(0)
  un <- unique(data_base$ag_crop$type)
  for(i in un){
    for(j in 1:max(data_base$ag_crop$period)){
      a <- which((data_base$ag_crop$type==i & data_base$ag_crop$period==j) & data_base$ag_crop$shehia %in% shehiaF)
      temp<-init(1)
      temp$time = j
      temp$variable = i
      temp$value = nrow(data_base$ag_crop[a,])/sum(data_base$main$period == j & data_base$main$shehia %in% shehiaF)
      dc<-rbind(dc, temp)
    }#end time
  }#end crop type
  
  library(hrbrthemes)
  plots[["percent_harvesting_ag_crop"]]=ggplot(dc, aes(x=time, y=value, fill=variable)) + 
    geom_area(alpha=0.6 , size=.5, colour="white") +
    scale_fill_viridis(discrete = T) +
    theme_ipsum() + 
    ggtitle("Proportion harvesting Ag crop")
  
  
  
  
  ########################################################
  ################ AG CROPS BY INCOME ####################
  
  u=tolower(paste(data_base$ag_crop$type, data_base$ag_crop$unit, sep ="_"))
  u <- gsub("piece", "kipande", u)
  u<- gsub(" ", "_", u)
  u[grepl("themai", u)] <- "themai"
  data_base$ag_crop$value <- NA
  for(i in 1:nrow(data_base$ag_crop)){
    if(!is.na(data_base$ag_crop$type[i])){
      temp <- dpl[,u[i]]
      data_base$ag_crop$value[i] <- as.double(temp)*data_base$ag_crop$harvest[i]
    }
  }
  
  dc<-init(0)
  un <- unique(data_base$ag_crop$type)
  for(i in un){
    for(j in 1:max(data_base$ag_crop$period)){
      a <- which((data_base$ag_crop$type==i & data_base$ag_crop$period==j) & data_base$ag_crop$shehia %in%shehiaF)
      temp<-init(1)
      temp$time = j
      temp$variable = i
      temp$value = sum(data_base$ag_crop$value[a], na.rm = T)
      dc<-rbind(dc, temp)
    }#end time
  }#end crop type
  
  
  df=dc[!dc$variable == "mwani",]
  df=df[!df$variable == "karafu",]
  
  df$value[is.nan(df$value)] <- 0
  
  library(hrbrthemes)
  plots[["total_ag_crop_value"]] =ggplot(df, aes(x=time, y=value, fill=variable)) + 
    geom_area(alpha=0.6 , size=.5, colour="white") +
    scale_fill_viridis(discrete = T) +
    theme_ipsum() + 
    ggtitle("Ag crops total value")
  
  

  
  df=dc[dc$variable == "mwani",]
  df=rbind(df,dc[dc$variable == "karafu",])
  
  df$value[is.nan(df$value)] <- 0
  
  library(hrbrthemes)
  plots[["cloves_and_mwani"]] =ggplot(df, aes(x=time, y=value, fill=variable)) + 
    geom_area(alpha=0.6 , size=.5, colour="white") +
    scale_fill_viridis(discrete = T) +
    theme_ipsum() + 
    ggtitle("Mwani and Cloves")
  
  
  ###########################################################
  ################## FOREST GOOODS ##########################
  
  
  
  u=tolower(paste(data_base$forest_crop$type, data_base$forest_crop$unit, sep ="_"))
  u<- gsub(" ", "_", u)
  u <- gsub("piece", "kipande", u)
  u[grepl("themai", u)] <- "themai"
  u[u=="firewood_mzigo_kichwa"] <-  "kuni_mzigo_kichwa"
  
  
  data_base$forest_crop$value <- NA
  for(i in 1:nrow(data_base$forest_crop)){
    if(!is.na(data_base$forest_crop$type[i])){
      temp <- dpl[,u[i]]
      data_base$forest_crop$value[i] <- as.double(temp)*data_base$forest_crop$harvest[i]
    }
  }
  
  # Partially clean the data
  inds<-which(data_base$forest_crop$type %in% c("Alipasua boriti", "Kukata Magogo kwa ajili ya kutengeneza mkaa", "Kukata fito", "mbao", "Alikata miti ya kujengea",
                                 "Matawi Ya mjiti", "Kukata Miti ya Kujengea kibanda"))
  data_base$forest_crop$type[inds] = "timber"
  data_base$forest_crop$type[which(data_base$forest_crop$type =="Kubanja Chikichi")] <- "mchikichi"
  
  dc<-init(0)
  un <- unique(data_base$forest_crop$type)
  for(i in un){
    for(j in 1:max(data_base$forest_crop$period)){
      a <- which((data_base$forest_crop$type==i & data_base$forest_crop$period==j) & data_base$forest_crop$shehia %in% shehiaF)
      temp<-init(1)
      temp$time = j
      temp$variable = i
      temp$value = sum(data_base$forest_crop$value[a], na.rm = T)
      dc<-rbind(dc, temp)
    }#end time
  }#end crop type
  
  dc$value[is.nan(dc$value)] <- 0
  
  
  plots[["forest_crop_sum"]] <- ggplot(dc, aes(x=time, y=value, fill=variable)) + 
    geom_area(alpha=0.6 , size=.5, colour="white") +
    scale_fill_viridis(discrete = T) +
    theme_ipsum() + 
    ggtitle("Forest Crops total Value") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 38))
  
  
##########################################
############ HOURS #######################
  
  
  nt<-max(data_hours$period)
  init <- function(x){
    df <- data.frame(matrix(ncol = 3, nrow =x ))
    colnames(df) <- c("time", "variable", "value")
    return(df)
  }
  
  df <- init(0)
  
  for(t in 1:nt){
    for(i in unique(data_hours$main)){
      temp <- init(1)
      temp$time = t
      temp$variable = i
      inds <- which((data_hours$period==t & data_hours$main == i) & data_hours$shehia  %in% shehiaF)
      temp$value <- sum(data_hours$hours[inds], na.rm = TRUE)
      df<-rbind(df, temp)
    }
  }
  
  library(hrbrthemes)
  plots[["hours_sum"]]<-ggplot(df, aes(x=time, y=value, fill=variable)) + 
    geom_area(alpha=0.6 , size=.5, colour="white") +
    scale_fill_viridis(discrete = T) +
    theme_ipsum() + 
    ylim(0, 6000) +
    ggtitle(paste("Total hours spent working in XXX, in", shehiaF))
  
  print("got here 4")

############################################################################
################ DIVERSIFICATION ###########################################
  # 
  # 
  # if(shehiaF == "konde"){
  #   nt<-13:length(incts)
  # }else{
  #   nt<-1:length(incts)
  # }
  # 
  # init <- function(x){
  #   df <- data.frame(matrix(ncol = 8, nrow =x ))
  #   colnames(df) <- c("time", "value", "0%", "25%", "50%", "75%", "100%", "shehia")
  #   return(df)
  # }
  # 
  # df <- init(0)
  # 
  # 
  # 
  # entropy <- function(x) -sum(ifelse(x == 1 | x== 0, 0, x*log(x)))
  # 
  # for(t in nt){
  #   for(i in shehiaF){
  #     temp <- init(1)
  #     temp$time = t
  #     dat<-incts[[t]]
  #     dat2 <- incts[[t]][incts[[t]]$shehia ==i, which(names(incts[[t]]) == "wage"):which(names(incts[[t]]) == "sold")]
  #     dat2 <- dat2/dat$inc
  #     dat2<-apply(dat2, 1, entropy)
  #     temp$value <-median(dat2, na.rm = T)
  #     temp[3:7]<-quantile(dat2, na.rm = T)
  #     temp$shehia = i
  #     df<-rbind(df, temp)
  #   }
  # }
  # 
  # 
  # names(df)[4] <- "lower"
  # names(df)[6] <- "upper"
  # plots[["diversification"]] <- ggplot(df, aes(x=time, y=value, group = shehia, color = shehia)) +
  #   geom_ribbon(aes(ymin=lower, ymax=upper), 
  #               alpha=0.3, color=NA) +
  #   geom_line(size = 1)
  # 
  # 
  
  
  
  
  income=plot_grid(plots[["netchange"]],
            plots[["hhincomedot"]],
            plots[["percent_incclass"]],
            plots[["revenuejulian"]],
            plots[["hhcondot"]],
            plots[["total_income"]]
              )
  
  inctypes = plot_grid(plots[["percent_harvesting_ag_crop"]],
                       plots[["total_ag_crop_value"]],
                       plots[["forest_crop_sum"]],
                       plots[["hours_sum"]])
#                       plots[["diversification"]])
  
  credit <- plot_grid(plots[["store_credit"]],
                      plots[["credit_given"]],
                      plots[["debt_borrowed"]],
                      plots[["storecredit_percent"]],
                      plots[["credit_returned"]],
                      plots[["debt_returned"]])
  
  gift <- plot_grid(plots[["value"]],
                    plots[["value"]],
                    plots[["gift_give_perc"]],
                    plots[["gift_rec_perc"]])
  

  return(list(income = income,
         inctypes=inctypes,
         credit=credit,
         gift=gift))  
}
 
  

  
  
  
  
  
  
  
  
  
  
  
  