########################################################################################
###################### HOUSEHOLD DIVRESITY #############################################
source("C:/Users/jeffr/OneDrive/Documents/Risk/code/script/analysis/DOL.R")

###############################################
############# LOOP PREP #######################

mt <- max(d$main$period, na.rm =T)
uid <- unique(dp$hhid)
dolc <- init(0)
cnt <- 1

#################################################
################ LOOP ###########################
for(i in uid){
  for(j in 1:mt){
    ind <- which(dp$hhid == i & dp$period == j)
    if(length(ind)>0){
      v <- init(1)
      # Identify all the unique diffrent types of occupations
      utype <- unique(dp$cat[ind])
      temp <-c()
      for(k in utype){
        temp<-c(temp, sum(dp$hours[ind][dp$cat[ind]==k]))
      }
      v$dol <- shi(temp)
      v$hours <- sum(temp, na.rm =T)
      v$nocs <- length(utype)
      v$occs <- paste(utype, collapse = " ")
      v$name <- dp$name[ind][1]
      v$adults <- dp$adults[ind][1] # This is incorrect.  AND NEEDS TO BE CHANGED.
      v$sex <- d$hhm_details$gender[which(d$hhm_details$hh_ID==i)[1]]
      v$age <- d$hhm_details$age[which(d$hhm_details$hh_ID==i)[1]]
      v$education <- d$hhm_details$education_factor[which(d$hhm_details$hh_ID==i)[1]]
      v$marital <- d$hhm_details$marital[which(d$hhm_details$hh_ID==i)[1]]
      v$wealth <- dp$wealth[ind][1]
      v$date <- dp$date[ind][1]
      v$shehia <- dp$shehia[ind][1]
      v$hhid <- dp$hhid[ind][1]
      v$hmid <- i
      v$sibs_local_head <- dp$sibs_local_head[ind][1]
      v$sibs_dist_head <- dp$sibs_dist_head[ind][1]
      v$sibs_local_spouse <- dp$sibs_local_spouse[ind][1]
      v$sibs_dist_spouse <- dp$sibs_dist_spouse[ind][1]
      v$interviewer <- dp$interviewer[ind][1]
      v$period <- dp$period[ind][1]
      dolc<-rbind(dolc, v)
    }
  }
}


dat_hh <- data.frame(dol=dolc$dol,
                     s = dolc$shehia,
                     hid = dolc$hhid,
                     ad = dolc$adults,
                     sib =  dolc$sibs_local_spouse +  dolc$sibs_local_head,
                     g = dolc$sex,
                     edu = as.integer(dolc$education),
                     ag = dolc$age,
                     p = month(dolc$date),
                     w = log(dolc$wealth),
                     p = month(dolc$date),
                     y = ifelse(dolc$period > 24, 2, 1),
                     r2 = ifelse(dolc$period > 12, 2, 1), # Addition of Konde
                     r3 = ifelse(dolc$period > 16, 2, 1), # Addition of Mwani as seperate. 
                     Int = dolc$interviewer)




dat_hh<-dat_hh[-which(dat_hh$s %in% c("chokocho", "mkoani", "chumbageni", "mtambili")),] # REMOVE THE SOUTHERN SHEHIA
dat_hh <- dat_hh[complete.cases(dat_hh),] # Clean
dat_hh <- dat_hh[dat_hh$ag>age_for_adults,] # Clean
dat_hh<-dat_hh[dat_hh$ad != 0,]

dat_hh <- as.list(dat_hh)


temp<-dat_hh$ag
temp[dat_hh$ag>17 & dat_hh$ag <28] <- 1
temp[dat_hh$ag>27 & dat_hh$ag <38] <- 2
temp[dat_hh$ag>37 & dat_hh$ag <48] <- 3
temp[dat_hh$ag>47 & dat_hh$ag <58] <- 4
temp[dat_hh$ag>57 & dat_hh$ag <68] <- 5
temp[dat_hh$ag>67 & dat_hh$ag <78] <- 6
temp[dat_hh$ag>77 & dat_hh$ag <88] <- 6
temp[dat_hh$ag>87 & dat_hh$ag <98] <- 6

# temp<-dat2$P
n <- length(unique(temp))
ua <- sort(unique(temp))
DmatAG <- matrix(NA, ncol = n, nrow = n)
for(i in 1:n){
  for(j in 1:n){
    DmatAG[i,j] <- abs(ua[i]-ua[j]) 
  }
}
dat_hh$ag <- temp

# Adults gaussian Processs
temp<-dat_hh$ad

n <- max(max(unique(temp)),length(unique(temp)))
ua <- 1:n
DmatAD <- matrix(NA, ncol = n, nrow = n)
for(i in 1:n){
  for(j in 1:n){
    DmatAD[i,j] <- abs(ua[i]-ua[j]) 
  }
}



# EDUCATION
temp<-dat_hh$edu
# temp<-dat2$P
n <- max(unique(temp))
ua <- 1:n
DmatED <- matrix(NA, ncol = n, nrow = n)
for(i in 1:n){
  for(j in 1:n){
    DmatED[i,j] <- abs(ua[i]-ua[j]) 
  }
}

# Distance Matricies
dat_hh$DmatAG <- DmatAG
dat_hh$DmatED <- DmatED/10
dat_hh$DmatAD <- DmatAD/10

dat_hh$p <- as.integer(as.factor(dat_hh$p))
dat_hh$ag <- as.integer(as.factor(dat_hh$ag))
dat_hh$edu <- as.integer(as.factor(dat_hh$edu))


# Helper values
dat_hh$LAG <- length(unique(dat_hh$ag))
dat_hh$LP <- length(unique(dat_hh$p))
dat_hh$LAD <- nrow(dat_hh$DmatAD)
dat_hh$LED <- nrow(dat_hh$DmatED)
dat_hh$NOBS <- length(dat_hh$p)
dat_hh$N <- length(dat_hh$p)
dat_hh$P1 <- 1:12
dat_hh$NS <- length(unique(dat_hh$s))
dat_hh$NHH <- length(unique(dat_hh$hid))

# Other Vars
dat_hh$s <-as.integer(as.factor(dat_hh$s))
dat_hh$g <-as.integer(as.factor(dat_hh$g))
dat_hh$I <-as.integer(as.factor(dat_hh$I))
dat_hh$sib = standardize(dat_hh$sib)

dat_hh$dol <- standardize(dat_hh$dol)
dat_hh$w <- standardize(dat_hh$w)
dat_hh$hid <- as.integer(as.factor(dat_hh$hid))
dat_hh$m <- as.integer(as.factor(dat_hh$m))
# 



########################################################################
################### RUN ################################################

m1dolREPhh <- stan("C:/Users/jeffr/OneDrive/Documents/Risk/code/stan_models/random_effects_proper_par_hh.stan", chains = 4, cores = 4, data = dat_hh, iter = 1000)
saveRDS(m1dolREPhh, "m1dolREPhh.RDS")

#########################################################################
################ PROCESS AND PLOT #######################################

model <- readRDS("C:/Users/jeffr/OneDrive/Documents/Risk/m1dolREPhh.RDS")
post <- extract.samples(model)
precis(model)


# sib
kif<-(with(post, (bad[,3]*sigma_bad+mu_bad)-(bad[,1]*sigma_bad+mu_bad))) # Konde - kifundi
kip<-(with(post, (bad[,3]*sigma_bad+mu_bad)-(bad[,2]*sigma_bad+mu_bad)))# Konde - kipange
msu<-(with(post, (bad[,3]*sigma_bad+mu_bad)-(bad[,4]*sigma_bad+mu_bad)))# Konde - msuka


id<- c(rep('kif', 2000), rep("kip", 2000), rep("msu", 2000))
val <- c(kif, kip, msu)

df<-data.frame(id = id,
               val = val)


data_summary <- function(x) {
  m <- mean(x)
  ymin <- unlist(PI(x, .95)[[1]])
  ymax <- unlist(PI(x, .95)[[2]])
  return(c(y=m,ymin=ymin,ymax=ymax))
}

ggplot(df, aes(x=id, y=val)) + 
  geom_violin(trim = .95)  +
  stat_summary(fun.y = mean,geom = "point") + 
  stat_summary(fun.data = data_summary) +
  geom_hline(yintercept=0) + 
  ylim(-.38, .5) +
  coord_flip() +
  ylab("Posterior parameter contrast") +
  xlab("Site")


kif<-(with(post, (bad[,1]*sigma_bad+mu_bad))) 
kip<-(with(post, (bad[,2]*sigma_bad+mu_bad)))
kon<-(with(post, (bad[,3]*sigma_bad+mu_bad)))
msu<-(with(post, (bad[,4]*sigma_bad+mu_bad)))

dens(kon, xlim = c(-1, 1), col = "blue")
dens(kif, add =T, col = "red")
dens(kip, add =T, col = "green")
dens(msu, add =T, col = "goldenrod")




# sib
kif<-(with(post, (bsib[,3]*sigma_bsib+mu_bsib)-(bsib[,1]*sigma_bsib+mu_bsib))) # Konde - kifundi
kip<-(with(post, (bsib[,3]*sigma_bsib+mu_bsib)-(bsib[,2]*sigma_bsib+mu_bsib)))# Konde - kipange
msu<-(with(post, (bsib[,3]*sigma_bsib+mu_bsib)-(bsib[,4]*sigma_bsib+mu_bsib)))# Konde - msuka


id<- c(rep('kif', 2000), rep("kip", 2000), rep("msu", 2000))
val <- c(kif, kip, msu)

df<-data.frame(id = id,
               val = val)


data_summary <- function(x) {
  m <- mean(x)
  ymin <- unlist(PI(x, .95)[[1]])
  ymax <- unlist(PI(x, .95)[[2]])
  return(c(y=m,ymin=ymin,ymax=ymax))
}

ggplot(df, aes(x=id, y=val)) + 
  geom_violin(trim = .95)  +
  stat_summary(fun.y = mean,geom = "point") + 
  stat_summary(fun.data = data_summary) +
  geom_hline(yintercept=0) + 
  ylim(-.75, .75) +
  coord_flip() +
  ylab("Posterior parameter contrast") +
  xlab("Site")


kif<-(with(post, (bsib[,1]*sigma_bsib+mu_bsib))) 
kip<-(with(post, (bsib[,2]*sigma_bsib+mu_bsib)))
kon<-(with(post, (bsib[,3]*sigma_bsib+mu_bsib)))
msu<-(with(post, (bsib[,4]*sigma_bsib+mu_bsib)))

dens(kon, xlim = c(-1, 1), col = "blue")
dens(kif, add =T, col = "red")
dens(kip, add =T, col = "green")
dens(msu, add =T, col = "goldenrod")



# w
kif<-(with(post, (bw[,3]*sigma_bw+mu_bw)-(bw[,1]*sigma_bw+mu_bw))) # Konde - kifundi
kip<-(with(post, (bw[,3]*sigma_bw+mu_bw)-(bw[,2]*sigma_bw+mu_bw)))# Konde - kipange
msu<-(with(post, (bw[,3]*sigma_bw+mu_bw)-(bw[,4]*sigma_bw+mu_bw)))# Konde - msuka


id<- c(rep('kif', 2000), rep("kip", 2000), rep("msu", 2000))
val <- c(kif, kip, msu)

df<-data.frame(id = id,
               val = val)


data_summary <- function(x) {
  m <- mean(x)
  ymin <- unlist(PI(x, .95)[[1]])
  ymax <- unlist(PI(x, .95)[[2]])
  return(c(y=m,ymin=ymin,ymax=ymax))
}

ggplot(df, aes(x=id, y=val)) + 
  geom_violin(trim = .95)  +
  stat_summary(fun.y = mean,geom = "point") + 
  stat_summary(fun.data = data_summary) +
  geom_hline(yintercept=0) + 
  ylim(-.75, .75) +
  coord_flip() +
  ylab("Posterior parameter contrast") +
  xlab("Site")


kif<-(with(post, (bw[,1]*sigma_bw+mu_bw))) 
kip<-(with(post, (bw[,2]*sigma_bw+mu_bw)))
kon<-(with(post, (bw[,3]*sigma_bw+mu_bw)))
msu<-(with(post, (bw[,4]*sigma_bw+mu_bw)))

dens(kon, xlim = c(-1, 1), col = "blue")
dens(kif, add =T, col = "red")
dens(kip, add =T, col = "green")
dens(msu, add =T, col = "goldenrod")




