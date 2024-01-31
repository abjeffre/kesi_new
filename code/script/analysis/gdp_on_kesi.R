#######################
###### PACKAGES #######

library("DutchDayDummies")
library("lubridate")

when_is_ramadan <- function(years){
  seq_of_dates <- seq_days(
    from = paste0(min(years), "-1-1"),
    to = paste0(max(years), "-12-31")
  )
  idates <- sapply(seq_of_dates, ummalqura_algorithm)
  holiday_dates <- seq_of_dates[stringr::str_detect(idates, "^1-9-....") == TRUE]
  return(holiday_dates)
}


when_is_ramadan(2012)
############################################################################
#################### KESI DATA ############################################
dk<-read.csv("data/kesi_data2.csv")
dk$TAREHE[9] <- "03/12/2011"
dk$TAREHE[37] <- "12/31/2011"
dk$TAREHE[433] <- "02/17/2017"
dk$TAREHE[433] <- "3/16/2018"
dk$TAREHE[469] <- "08/24/2018"
dk$TAREHE[477] <- "10/05/2018"
dk$TAREHE[80] <- "10/29/2018"
dk$TAREHE[678] <- "03/29/2022"

dk$date<- as.Date(dk$TAREHE, c("%m/%d/%Y"))

# Define survey Periods
dfp <- data.frame(min = unique(df_bkmm$dmin), max =unique(df_bkmm$dmax))


##############################################
# Get Periods with Exact Matches #############
kesi_obs <-c()
for(i in 2:11){
  temp<-which(dk$date >= dfp$min[i] & dk$date <= dfp$max[i]+1)
  kesi_obs <- c(kesi_obs, length(temp)) 
}

gdp <- c()
for(i in 2:11){
  gdp <- c(gdp, sum(df_bkmm$gsp[df_bkmm$period==i]))
}


plot(kesi_obs ~ gdp)
traing_data = list(y = kesi_obs, gdp = gdp, N = length(gdp))
# Few datapoints
mts=stan("code/stan_models/training_set.stan", chains = 1, cores = 1, data = traing_data, iter = 2000)
precis(mts)


################################################
########## DO FOR YEAR #########################

kesi_obs_summed = c()
month <- c()
ramadan = c()
years = c()
pid =c()
for(i in 2:26){
  for(year in 2011:2021){
    temp<-which(((((month(dk$date) >= month(dfp$min[i])) &(day(dk$date) >= day(dfp$min[i]))) & year(dk$date) == year)) &
                (((month(dk$date) <= month(dfp$max[i]+1))& (day(dk$date) <= day(dfp$max[i]+1))) & year(dk$date) == year))
    kesi_obs_summed = c(kesi_obs_summed, length(temp))
    month= c(month, month(dfp$max[i]-7))
    #Determine if ramathan is within the period
    # Months and days of ramathan
    rmonth = month(as_date(when_is_ramadan(year):(when_is_ramadan(year) + 32)))
    rday = day(as_date(when_is_ramadan(year):(when_is_ramadan(year) + 32)))
    ramadan_temp = c()
    for(k in 1:length(rmonth)) ramadan_temp=c(ramadan_temp, paste0(rmonth[k],"-", rday[k]))
    #Months and days of period
    pmonth = month(as_date(dfp$min[i]:(dfp$max[i]+1)))
    pday = day(as_date(dfp$min[i]:(dfp$max[i]+1)))
    period = c()
    for(k in 1:length(pmonth)) period=c(period, paste0(pmonth[k],"-", pday[k]))
      pid =  c(pid, i)
    # Check for overlapping days of ramadan and period
    ramadan = c(ramadan, ifelse(any(ramadan_temp %in% period), 1, 0))
    years <-c(years, year)    
  }
}

###############################################
############# A DATA FRAME FOR ANALYSIS #######


gdp <- c()
for(i in 2:26){
  gdp <- c(gdp, sum(df_bkmm$gsp[df_bkmm$period==i]))
}
gdp=rep(gdp,11)

df <- data.frame(year = years, 
                 period = pid,
                 month = month,
                 ramadan = ramadan,
                 kesi=kesi_obs_summed,
                 gdp =gdp)


df[order(df$year, df$month, df$period), ]
df$pid = 1:nrow(df)
# plot(kesi_obs_summed ~ gdp + month)
# summary(glm(kesi_obs_summed ~ gdp + month + ramadan + years, family = "poisson"))


# Make matrix for DMAT
L = length(unique(df$year))
Dmat = matrix(NA, L, L)
for(i in 1:L){
  for(j in 1:L){
  Dmat[i, j] = abs(i-j)
  }  
}
Dmat=Dmat/1

standata1 <- list(x=as.integer(as.factor(df$year)),
                  y=df$kesi,
                  p = as.integer(as.factor(df$month)),
                  gdp = df$gdp,
                  N=length(df$pid),
                  L = L, # factor c of basis functions for GP for f1
                  DmatX= Dmat,
                  LP = length(unique(df$month)),
                  P1 = 1:length(unique(df$month)))  # number of basis functions for GP for f1

m1=stan("code/stan_models/gp1.stan", chains = 1, cores = 1, data = standata1, iter = 2000)
m2=stan("code/stan_models/gp2.stan", chains = 1, cores = 1, data = standata1, iter = 2000)
m3=stan("code/stan_models/gp3.stan", chains = 1, cores = 1, data = standata1, iter = 2000)

#########################################
############ PLOTTTING ##################

dev.off()
par(mfrow=c(1, 2))
post <- extract.samples(m2)

sim <- function(x, p){
  with(post,
       rpois(50, exp(a + kX[,x] + kp[,p]))
  )
} 

plot(NULL, xlim = c(0, 273), ylim = c(0, 20))
smoothed = matrix(NA, nrow = nrow(df), ncol = 50)
mu = c()
for(i in 1:nrow(df)){
  #  points(rep(i, 50)+rnorm(50, 0,.2), sim(standata1$x[i], standata1$p[i], standata1$gdp[i])+rnorm(50, 0,.2), pch = 16, col = col.alpha("black", .2))
  mu=c(mu, mean(sim(standata1$x[i], standata1$p[i])))
  smoothed[i,]=sim(standata1$x[i], standata1$p[i])
}

for(i in 1:50){
  lines(smooth.spline(df$pid, smoothed[,i]), type = "l", col = col.alpha("#CA2015", .3))
}

points(df$pid, df$kesi, col = col.alpha("#030A18", .5), cex = 1, pch = 16)

# With gdp
post <- extract.samples(m3)

sim <- function(x, p, gdp){
  with(post,
  rpois(50, exp(a + b*gdp + kX[,x] + kp[,p]))
  )
} 


plot(NULL, xlim = c(0, 273), ylim = c(0, 20))
smoothed = matrix(NA, nrow = nrow(df), ncol = 50)
mu = c()
for(i in 1:nrow(df)){
#  points(rep(i, 50)+rnorm(50, 0,.2), sim(standata1$x[i], standata1$p[i], standata1$gdp[i])+rnorm(50, 0,.2), pch = 16, col = col.alpha("black", .2))
  mu=c(mu, mean(sim(standata1$x[i], standata1$p[i], standata1$gdp[i])))
  smoothed[i,]=sim(standata1$x[i], standata1$p[i], standata1$gdp[i])
}

for(i in 1:50){
  lines(smooth.spline(df$pid, smoothed[,i]), type = "l", col = col.alpha("#CA2015", .3))
}
points(df$pid, df$kesi, col = col.alpha("#030A18", .5), cex = 1, pch = 16)


#########################################################
########## BACK CASTING #################################
post1 <- extract.samples(m2)
post2 <- extract.samples(mts)
post3 <- extract.samples(m3)
sim <- function(x, p, gdp){
       rpois(50, exp(post2$a + post2$b*gdp + post3$kX[,x] + post3$kp[,p]))
} 

plot(NULL, xlim = c(0, 273), ylim = c(0, 20))
smoothed = matrix(NA, nrow = nrow(df), ncol = 50)
mu = c()
for(i in 1:nrow(df)){
  #  points(rep(i, 50)+rnorm(50, 0,.2), sim(standata1$x[i], standata1$p[i], standata1$gdp[i])+rnorm(50, 0,.2), pch = 16, col = col.alpha("black", .2))
  mu=c(mu, mean(sim(standata1$x[i], standata1$p[i], standata1$gdp[i])))
  smoothed[i,]=sim(standata1$x[i], standata1$p[i], standata1$gdp[i])
}

for(i in 1:50){
  lines(smooth.spline(df$pid, smoothed[,i]), type = "l", col = col.alpha("#CA2015", .3))
}

lines(smooth.spline(df$pid, mu), type = "l", col = "purple", lwd = 2)
points(df$pid, df$kesi, col = col.alpha("#030A18", .5), cex = 1, pch = 16)



#########################################################
############## 