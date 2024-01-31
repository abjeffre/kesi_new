############################################################################
#################### KASI DATA ############################################
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
# Drop 2010 as we only have one month

out<-matrix(NA, ncol = 12, nrow = 11)
cnt <- 1
for(i in 2011:2021){
  inds<-month(dk$date[which(year(dk$date)== i)])
  tab <- rep(0, 12)
  for(j in 1:12){
    tab[j] <- sum(inds == j)
  }
  out[cnt, ] <- tab
  cnt <- cnt+1
}

val <-c()
month <-c()
for(i in 1:12){
  val<-c(val, out[,i])
  month <- c(month, rep(i, length(out[,i])))
}

df <- data.frame(val = val,
                 month = month)
ggplot(df, aes(x = as.factor(month), y = val)) +
  geom_violin() +
  geom_point() 


out<-c()
date <-as.Date(c())
cnt <- 1
for(i in 2011:2021){
  inds<-month(dk$date[which(year(dk$date)== i)])
  tab <- rep(0, 12)
  for(j in 1:12){
    tab[j] <- sum(inds == j)
  }
  date<-c(date, as.Date(paste0(1:12, "/1/", i), c("%m/%d/%Y")))
  out <-  c(out, tab)
  cnt <- cnt+1
}


caught<-out
p <- month(date)
y <- year(date)-(min(year(date))-1)
P1 <- 1:12
LP <- 12

LY <- max(y)
Dmat <- matrix(NA, ncol = LY, nrow = LY)
for(i in 1:LY){
  for(j in 1:LY){
    Dmat[i,j] = abs(i-j)
  }
}

Dmat = Dmat
dat <- list(N = length(caught),
            caught = caught,
            p = p,
            y = y,
            P1 = P1,
            LP = LP,
            LY = LY,
            Dmat = Dmat)

mgp <-stan("C:/Users/jeffrey_andrews/OneDrive/Documents/kesi/code/stan_models/gp_estimates.stan", cores = 1, chains = 1, iter = 500, data = dat)

# FIX MARCH BY ADDING THE ADDITIONAL VALUES TO APRIL
out<-c()
date <-as.Date(c())
cnt <- 1
for(i in 2011:2021){
  inds<-month(dk$date[which(year(dk$date)== i)])
  tab <- rep(0, 12)
  for(j in 1:12){
    tab[j] <- sum(inds == j)
  }
  excess<- tab[3]-tab[2]
  if(excess > 0){
    print("yes")
    tab[3] <- tab[3]-excess 
    tab[4] <- tab[4]+ceiling(excess*2/3)
    tab[5] <- tab[5]+floor(excess*1/3)
  }
  date<-c(date, as.Date(paste0(1:12, "/1/", i), c("%m/%d/%Y")))
  out <-  c(out, tab)
  cnt <- cnt+1
}

caught<-out
p <- month(date)
y <- year(date)-(min(year(date))-1)
P1 <- 1:12
LP <- 12

LY <- max(y)
Dmat <- matrix(NA, ncol = LY, nrow = LY)
for(i in 1:LY){
  for(j in 1:LY){
    Dmat[i,j] = abs(i-j)
  }
}

# GET RAMATHAN MONTHS
ramathan<-rep(0, 11*12)
# 2011
ramathan[8] <- 1
# 2012
ramathan[7+12*1] <- 11/30
ramathan[8+12*1] <- 20/31
#2013
ramathan[7+12*2] <- 20/30
ramathan[8+12*2] <- 11/31
#2014
ramathan[6+12*3] <- 2/31
ramathan[7+12*3] <- 29/30
#2015
ramathan[6+12*4] <- 13/31
ramathan[7+12*4] <- 18/30
#2016
ramathan[6+12*5] <- 24/31
ramathan[7+12*5] <- 6/30
#2017
ramathan[5+12*6] <- 3/30
ramathan[6+12*6] <- 26/31
#2018
ramathan[5+12*7] <- 14/30
ramathan[6+12*7] <- 15/31
#2019
ramathan[5+12*8] <- 24/30
ramathan[6+12*8] <- 4/31
#2020
ramathan[4+12*9] <- 6/31
ramathan[5+12*9] <- 24/30
#2020
ramathan[4+12*10] <- 17/31
ramathan[5+12*10] <- 13/30


Dmat = Dmat
dat <- list(N = length(caught),
            caught = caught,
            p = p,
            y = y,
            P1 = P1,
            LP = LP,
            LY = LY,
            Dmat = Dmat,
            r= ramathan)

mgp <-stan("C:/Users/jeffrey_andrews/OneDrive/Documents/Kasi_DF/code/gp_estimates.stan", cores = 1, chains = 1, iter = 500, data = dat)
post <- extract.samples(mgp)
plot(colMeans(post$kp), type = "l", ylim = c(-2,2))
PI<-apply(post$kp,2,PI)
rethinking::shade(PI, 1:12)
dens(post$br)