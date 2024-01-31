##############################################################################
############################## RUN DOL INDIVIDUAL MODELS #####################




############################################
####### Indicate MakeFile is being used ####

if(MAKE != TRUE){
  source("code/script/cleaning/scrapekobo.R")
  source("code/script/cleaning/getdate.R")
  source("code/script/cleaning/buildids.R")
  source("code/script/cleaning/cleaning.R")
  source("code/script/analysis/DOL.R")
  source("code/script/analysis/generate_gdp.R")
}


########################################################
##################### DATA PREP #########################


dat_inv <- data.frame(dol=dol$dol,
                      s = dol$shehia,
                      noc = dol$noc,
                      ad = dol$adults,
                      ch = dol$children,
                      te = dol$teens,
                      sp = dol$spouses,
                      w=log(dol$wealth),
                      edu = as.integer(dol$education),
                      sib_h_l = dol$sibs_local_head,
                      sib_h_d = dol$sibs_dist_head,
                      sib_s_l = dol$sibs_local_spouse,
                      sib_s_d = dol$sibs_dist_spouse,
                      sib =  dol$sibs_local_spouse + dol$sibs_local_head, 
                      g = dol$sex,
                      fag = ifelse(is.nan(dol$famgift), 0, dol$famgift) ,
                      frg = ifelse(is.nan(dol$frigift), 0, dol$frigift) ,
                      ag = dol$age,
                      m = dol$marital,
                      p = dol$period, #change to period and limit period to when konde was added TODO
                      y = ifelse(dol$period > 24, 2, 1),
                      r2 = ifelse(dol$period > 12, 2, 1), # Addition of Konde
                      r3 = ifelse(dol$period > 16, 2, 1), # Addition of Mwani as seperate. 
                      I = dol$interviewer,
                      id = dol$hmid,
                      hid = dol$hhid,
                      h = dol$hours)



dat_inv<-dat_inv[-which(dat_inv$s %in% c("chokocho", "mkoani", "chumbageni", "mtambili")),] # REMOVE THE SOUTHERN SHEHIA
# There are some cases in which hours have not been reported and this is causing the 
dat_inv <- dat_inv[complete.cases(dat_inv),] # Clean
dat_inv <- dat_inv[dat_inv$ag>age_for_adults,] # Clean
dat_inv<-dat_inv[dat_inv$ad != 0,]#neeeds to be removed when we remove first observation of house TODO

dat_inv <- as.list(dat_inv) # Make List

# # CREATE FEEDERS FOR GPs
# # Age Gaussian Process

# CREATE FEEDERS FOR GPs
# Age Gaussian Process
# temp<-dat_inv2$ag
# temp[dat_inv$ag>17 & dat_inv$ag <28] <- 1
# temp[$ag>27 & dat_inv$ag <38] <- 2
# temp[dat_inv$ag>37 & dat_inv$ag <48] <- 3
# temp[dat_inv$ag>47 & dat_inv$ag <58] <- 4
# temp[dat_inv$ag>57 & dat_inv$ag <68] <- 5
# temp[dat_inv$ag>67 & dat_inv$ag <78] <- 6
# temp[dat_inv$ag>77 & dat_inv$ag <88] <- 6
# temp[dat_inv$ag>87 & dat_inv$ag <98] <- 6

# n <- length(unique(temp))
# ua <- sort(unique(temp))
# DmatAG <- matrix(NA, ncol = n, nrow = n)
# for(i in 1:n){
#   for(j in 1:n){
#     DmatAG[i,j] <- abs(ua[i]-ua[j]) 
#   }
# }
# 
# # Adults gaussian Processs
# temp<-dat_inv$ad
# # temp<-dat2$P
# n <- max(max(unique(temp)),length(unique(temp)))
# ua <- 1:n
# DmatAD <- matrix(NA, ncol = n, nrow = n)
# for(i in 1:n){
#   for(j in 1:n){
#     DmatAD[i,j] <- abs(ua[i]-ua[j]) 
#   }
# }
# 
# 
# # EDUCATION
# temp<-dat_inv$edu
# # temp<-dat2$P
# n <- max(unique(temp))
# ua <- 1:n
# DmatED <- matrix(NA, ncol = n, nrow = n)
# for(i in 1:n){
#   for(j in 1:n){
#     DmatED[i,j] <- abs(ua[i]-ua[j]) 
#   }
# }
# 
# # Distance Matricies
# dat_inv$DmatAG <- DmatAG/10
# dat_inv$DmatED <- DmatED/10
# dat_inv$DmatAD <- DmatAD/10

# This is for de-meaning for inter vs. intra group comparisons 

# # GROUP LEVEL OFFSETS
# dat_inv$adh <- NA
# for(i in unique(dat_inv$hid)){
#   ind = which(dat_inv$hid==i)
#   dat_inv$adh[ind] <- dat_inv$ad[ind]-mean(dat_inv$ad[ind])
# }
# 
# # INDIVIDUAL LEVEL 
# dat_inv$adi <- NA
# for(i in unique(dat_inv$id)){
#   ind = which(dat_inv$id==i)
#   dat_inv$adi[ind] <- dat_inv$ad[ind]-mean(dat_inv$ad[ind])
# }

#
# dat_inv$sib_fac <- as.integer(as.factor(dat_inv$sib))
# dat_inv$ad_fac <- as.integer(as.factor(dat_inv$ad))
# dat_inv$p <- as.integer(as.factor(dat_inv$p))
# dat_inv$ag_fac <- as.integer(as.factor(dat_inv$ag))
# dat_inv$edu <- as.integer(as.factor(dat_inv$edu))


# Helper values
dat_inv$LAG <- length(unique(dat_inv$ag))
dat_inv$LP <- length(unique(dat_inv$p))
dat_inv$LAD <- nrow(dat_inv$DmatAD)
dat_inv$LED <- nrow(dat_inv$DmatED)
dat_inv$N <- length(dat_inv$p)
dat_inv2$P1 <- 1:length(unique(dat_inv2$p))
dat_inv$NS <- length(unique(dat_inv$s))
dat_inv$NID <- length(unique(dat_inv$id))
dat_inv$NHH <- length(unique(dat_inv$hid))

# # Other Vars
# dat_inv$s <-as.integer(as.factor(dat_inv$s))
# dat_inv$g <-as.integer(as.factor(dat_inv$g))
# dat_inv$I <-as.integer(as.factor(dat_inv$I))
# dat_inv$id <- as.integer(as.factor(dat_inv$id))
# dat_inv$hid <- as.integer(as.factor(dat_inv$hid))
# dat_inv$sib = standardize(dat_inv$sib_h_l + dat_inv$sib_s_l)
# dat_inv$dol <- standardize(dat_inv$dol)
# dat_inv$w <- standardize(dat_inv$w)
# dat_inv$h <- standardize(dat_inv$h)
# dat_inv$noc <- standardize(dat_inv$noc)
# 
# GENERATE DF WHERE ONLY THOSE WHO APPEARE MORE THAN ONCE ARE REPSETENDED
dat_inv2 <- dat_inv
a<-table(dat_inv2$id)
only_one_id<-attributes(a)$dimnames[[1]][which(a == 1)]
for(i in names(dat_inv2)){
  if(length(dat_inv2[[i]]) > 100){
    print(i)
    dat_inv2[[i]]<-dat_inv2[[i]][-which(dat_inv$id %in% only_one_id)]
  }
}


# CREATE FEEDERS FOR GPs
# Age Gaussian Process
temp<-dat_inv2$ag
temp[dat_inv2$ag>17 & dat_inv2$ag <28] <- 1
temp[dat_inv2$ag>27 & dat_inv2$ag <38] <- 2
temp[dat_inv2$ag>37 & dat_inv2$ag <48] <- 3
temp[dat_inv2$ag>47 & dat_inv2$ag <58] <- 4
temp[dat_inv2$ag>57 & dat_inv2$ag <68] <- 5
temp[dat_inv2$ag>67 & dat_inv2$ag <78] <- 6
temp[dat_inv2$ag>77 & dat_inv2$ag <88] <- 6
temp[dat_inv2$ag>87 & dat_inv2$ag <98] <- 6

# temp<-dat2$P
n <- length(unique(temp))
ua <- sort(unique(temp))
DmatAG <- matrix(NA, ncol = n, nrow = n)
for(i in 1:n){
  for(j in 1:n){
    DmatAG[i,j] <- abs(ua[i]-ua[j]) 
  }
}
dat_inv2$ag <- temp

# Adults gaussian Processs
temp<-dat_inv2$ad

n <- max(max(unique(temp)),length(unique(temp)))
ua <- 1:n
DmatAD <- matrix(NA, ncol = n, nrow = n)
for(i in 1:n){
  for(j in 1:n){
    DmatAD[i,j] <- abs(ua[i]-ua[j]) 
  }
}




# Sibs gaussian Processs
temp<-dat_inv2$sib
# temp<-dat2$P
n <- max(max(unique(temp)),length(unique(temp)))
ua <- 1:n
DmatSIB <- matrix(NA, ncol = n, nrow = n)
for(i in 1:n){
  for(j in 1:n){
    DmatSIB[i,j] <- abs(ua[i]-ua[j]) 
  }
}


# EDUCATION
temp<-dat_inv2$edu
# temp<-dat2$P
n <- max(unique(temp))
ua <- 1:n
DmatED <- matrix(NA, ncol = n, nrow = n)
for(i in 1:n){
  for(j in 1:n){
    DmatED[i,j] <- abs(ua[i]-ua[j]) 
  }
}



# temp<-dat2$P
n <- length(unique(dat_inv2$p))
ua <- 1:n
DmatP <- matrix(NA, ncol = n, nrow = n)
for(i in 1:n){
  for(j in 1:n){
    DmatP[i,j] <- abs(ua[i]-ua[j]) 
  }
}



# Distance Matricies
dat_inv2$DmatAG <- DmatAG/10
dat_inv2$DmatED <- DmatED/10
dat_inv2$DmatAD <- DmatAD/10
dat_inv2$DmatP <- DmatP/10
dat_inv2$DmatSIB <- DmatAD/10
dat_inv2$DmatSIB <- DmatSIB/10


# Demeaning for intra-versus inter household, indiviudal and shehia level effects.
# GROUP LEVEL OFFSETS
dat_inv2$adh <- NA
for(i in unique(dat_inv2$hid)){
  ind = which(dat_inv2$hid==i)
  dat_inv2$adh[ind] <- dat_inv2$ad[ind]-mean(dat_inv2$ad[ind])
}

# INDIVIDUAL LEVEL 
dat_inv2$adi <- NA
for(i in unique(dat_inv2$id)){
  ind = which(dat_inv2$id==i)
  dat_inv2$adi[ind] <- dat_inv2$ad[ind]-mean(dat_inv2$ad[ind])
}



dat_inv2$ad_med <- NA
for(i in unique(dat_inv2$id)){
  ind <- which(dat_inv2$id==i)
  dat_inv2$ad_med[ind] <- median(dat_inv2$ad[ind])   
}

# Data Prep - getting everything in proper type
dat_inv2$ad_fac <- as.integer(as.factor(dat_inv2$ad))
dat_inv2$ad <- standardize((dat_inv2$ad))
dat_inv2$ch <- standardize((dat_inv2$ch))
dat_inv2$te <- standardize((dat_inv2$te))
dat_inv2$sp <- standardize((dat_inv2$sp))
dat_inv2$p <- as.integer(as.factor(dat_inv2$p))
dat_inv2$ag <- as.integer(as.factor(dat_inv2$ag))
dat_inv2$edu <- as.integer(as.factor(dat_inv2$edu))


# Helper values
dat_inv2$LAG <- length(unique(dat_inv2$ag))
dat_inv2$LP <- length(unique(dat_inv2$p))
dat_inv2$LAD <- nrow(dat_inv2$DmatAD)
dat_inv2$LED <- nrow(dat_inv2$DmatED)
dat_inv2$LSIB <- nrow(dat_inv2$DmatSIB)
dat_inv2$NOBS <- length(dat_inv2$p)
dat_inv2$N <- length(dat_inv2$p)
dat_inv2$P1 <- 1:length(unique(dat_inv2$p))
dat_inv2$NS <- length(unique(dat_inv2$s))
dat_inv2$NID <- length(unique(dat_inv2$id))
dat_inv2$NHH <- length(unique(dat_inv2$hid))

# Other Vars
dat_inv2$s <-as.integer(as.factor(dat_inv2$s))
dat_inv2$g <-as.integer(as.factor(dat_inv2$g))
dat_inv2$I <-as.integer(as.factor(dat_inv2$I))
dat_inv2$sib = standardize(dat_inv2$sib_h_l + dat_inv2$sib_s_l)
dat_inv2$dol <- standardize(dat_inv2$dol)
dat_inv2$fag <- standardize(log1p(dat_inv2$fag))
dat_inv2$frg <- standardize(log1p(dat_inv2$frg))
dat_inv2$h <- standardize(dat_inv2$h)
dat_inv2$w <- standardize(dat_inv2$w)
dat_inv2$id <- as.integer(as.factor(dat_inv2$id))
dat_inv2$hid <- as.integer(as.factor(dat_inv2$hid))
dat_inv2$m <- as.integer(as.factor(dat_inv2$m))
dat_inv2$noc <- standardize(dat_inv2$noc)


########################################################
############ COMMUNITY LEVEL THINGS ####################

population = standardize(c(300, 200, 10000, 900))
stores = standardize(c(3, 10, 375, 92))
distance = standardize(c(-141, -52, 0, -46))
# we are selecting from the 13th time period onwards because the sample sizes across all shehia are the same here 
# If this is not done then we have to be takinbg the average 
gdp = standardize(c(sum(dl$flow$value[(dl$flow$shehia=="kifundi" & dl$flow$period %in% 13:28) &  dl$flow$main %in% dl$cash_in], na.rm = T),
        sum(dl$flow$value[(dl$flow$shehia=="kipange" & dl$flow$period %in% 13:28) &  dl$flow$main %in% dl$cash_in], na.rm = T),
        sum(dl$flow$value[(dl$flow$shehia=="konde" & dl$flow$period %in% 13:28) &  dl$flow$main %in% dl$cash_in], na.rm = T),
        sum(dl$flow$value[(dl$flow$shehia=="msuka_magharibi" & dl$flow$period %in% 13:28) &  dl$flow$main %in% dl$cash_in], na.rm = T)))
Y = data.frame(population, stores, distance, gdp)
dat_inv2$Y = Y
dat_inv2$shehia = 1:4

########################################################
########### RUN MODELS ##################################

m1 <- stan("code/stan_models/random_effects_only.stan", chains = 4, cores = 4, data = dat_inv2, iter = 1000)
saveRDS(m1, paste0("data/random_effects_only", Sys.Date(), ".RDS"))
precis(m1)        


# 
# m1dolREP <- stan("C:/Users/jeffr/OneDrive/Documents/Risk/code/stan_models/random_effects_proper_par.stan", chains = 4, cores = 4, data = dat_inv2, iter = 1000)
# saveRDS(m1dolREP, paste0("m1dolREP", Sys.date(), ".RDS")
# 
# m1dolGIFT <- stan("C:/Users/jeffrey_andrews/OneDrive/Documents/Risk/code/stan_models/random_effects_proper_par_gift.stan", chains = 4, cores = 4, data = dat_inv2, iter = 1000)
# saveRDS(m1dolGIFT, "m1dolGIFT.RDS")
# 
# m1dolGIFTFULL <- stan("C:/Users/jeffrey_andrews/OneDrive/Documents/Risk/code/stan_models/random_effects_proper_par_gift_full_np.stan", chains = 4, cores = 4, data = dat_inv2, iter = 1000)
# saveRDS(m1dolGIFTFULL, "m1dolGIFTFULL.RDS")
# 
# m1dolGIFTFULLH <- stan("C:/Users/jeffrey_andrews/OneDrive/Documents/Risk/code/stan_models/random_effects_proper_par_gift_full_np_h.stan", chains = 4, cores = 4, data = dat_inv2, iter = 1000)
# saveRDS(m1dolGIFTFULLH, "m1dolGIFTFULLH.RDS")
# 
# simpledol <- stan("C:/Users/jeffrey_andrews/OneDrive/Documents/Risk/code/stan_models/dol_simple.stan", chains = 4, cores = 4, data = dat_inv2, iter = 1000)
# saveRDS(simpledol, "simpledol.RDS")
# 
# test <- stan("C:/Users/jeffrey_andrews/OneDrive/Documents/Risk/code/stan_models/latenet.stan", chains = 4, cores = 4, data = dat_inv2, iter = 1000)
# 
# test <- stan("C:/Users/jeffrey_andrews/OneDrive/Documents/Risk/code/stan_models/simple_latent.stan", chains = 4, cores = 4, data = dat_inv2, iter = 1000)
# saveRDS(test, "test.RDS")
# test2 <- stan("C:/Users/jeffrey_andrews/OneDrive/Documents/Risk/code/stan_models/simple_latent.stan", chains = 4, cores = 4, data = dat_inv2, iter = 1000)
# saveRDS(test2, "test2.RDS")
# 
 test3 <- stan("code/stan_models/simple_latent_Z_inf.stan", chains = 8, cores = 8, data = dat_inv2, iter = 500)
 saveRDS(test3, "test3.RDS")
 full <- stan("code/stan_models/test_full_model.stan", chains = 8, cores = 8, data = dat_inv2, iter = 2000)
 saveRDS(full, "full.RDS")
# 
# dat_inv2$ad <- dat_inv2$ad_fac
# m1dolREGP <- stan("C:/Users/jeffr/OneDrive/Documents/Risk/code/stan_models/random_effects_proper_par_GP.stan", chains = 4, cores = 4, data = dat_inv2, iter = 1000)
# saveRDS(m1dolREGP, "m1dolREGP.RDS")
# 
# 
# dat_inv2$ad <- dat_inv2$ad_fac
# dat_inv2$sib <- dat_inv2$sib_fac
# m1dolGPfull <- stan("C:/Users/jeffr/OneDrive/Documents/Risk/code/stan_models/random_effects_proper_par_GP_full.stan", chains = 4, cores = 4, data = dat_inv2, iter = 1000)
# saveRDS(m1dolGPfull, "m1dolGPfull.RDS")
# 
# #######################################################################
# ################ EFFECT OF FAMILY ####################################
# model <- readRDS("output/test3.RDS")
# post <- extract.samples(model)
# a<-precis(simpledol,2)
# 
# tail(a[which(a$Rhat4> 100),])
# ok<-sort(a$Rhat4, decreasing = T, index.return	= T)
# a[ok$ix[1:100],]
# 
# kif<-(with(post, (bad[,3]*sigma_bad+mu_bad)-(bad[,1]*sigma_bad+mu_bad))) # Konde - kifundi
# kip<-(with(post, (bad[,3]*sigma_bad+mu_bad)-(bad[,2]*sigma_bad+mu_bad)))# Konde - kipange
# msu<-(with(post, (bad[,3]*sigma_bad+mu_bad)-(bad[,4]*sigma_bad+mu_bad)))# Konde - msuka
# 
# 
# id<- c(rep('Kifundi Nuclear Household', 2000), rep("Kipange Nuclear Household", 2000), rep("Msuka Nuclear Household", 2000))
# val <- c(kif, kip, msu)
# 
# df<-data.frame(id = id,
#                val = val)
# 
# 
# data_summary <- function(x) {
#   m <- mean(x)
#   ymin <- unlist(PI(x, .95)[[1]])
#   ymax <- unlist(PI(x, .95)[[2]])
#   return(c(y=m,ymin=ymin,ymax=ymax))
# }
# 
# ggplot(df, aes(x=id, y=val)) + 
#   geom_violin(trim = .95)  +
#   stat_summary(fun.y = mean,geom = "point") + 
#   stat_summary(fun.data = data_summary) +
#   geom_hline(yintercept=0) + 
#   ylim(-.38, .5) +
#   coord_flip() +
#   ylab("Posterior parameter contrast") +
#   xlab("")
# 
# 
# kif<-(with(post, (bad[,1]*sigma_bad+mu_bad))) 
# kip<-(with(post, (bad[,2]*sigma_bad+mu_bad)))
# kon<-(with(post, (bad[,3]*sigma_bad+mu_bad)))
# msu<-(with(post, (bad[,4]*sigma_bad+mu_bad)))
# 
# dens(kon, xlim = c(-.5, .2), col = "blue")
# dens(kif, add =T, col = "red")
# 
# 
# ##########################################################
# ################ SIBS ####################################
# 
# 
# kif<-(with(post, (bsib[,3]*sigma_bsib+mu_bsib)-(bsib[,1]*sigma_bsib+mu_bsib))) # Konde - kifundi
# kip<-(with(post, (bsib[,3]*sigma_bsib+mu_bsib)-(bsib[,2]*sigma_bsib+mu_bsib)))# Konde - kipange
# msu<-(with(post, (bsib[,3]*sigma_bsib+mu_bsib)-(bsib[,4]*sigma_bsib+mu_bsib)))# Konde - msuka
# 
# 
# id<- c(rep('Kifundi Extended Family', 2000), rep("Kipange Extended Family", 2000), rep("Msuka Extended Family", 2000))
# val <- c(kif, kip, msu)
# 
# df2<-data.frame(id = id,
#                val = val)
# 
# 
# ggplot(df2, aes(x=id, y=val)) + 
#   geom_violin() +
#   stat_summary(fun.y = mean,geom = "point") + 
#   stat_summary(fun.data = data_summary) +
#   geom_hline(yintercept=0) + 
#   ylim(-.38, .38) +
#   coord_flip() +
#   ylab("Posterior parameter contrast") +
#   xlab("Site")
# 
# 
# 
# 
# kif<-(with(post, (bsib[,1]*sigma_bsib+mu_bsib))) 
# kip<-(with(post, (bsib[,2]*sigma_bsib+mu_bsib)))
# kon<-(with(post, (bsib[,3]*sigma_bsib+mu_bsib)))
# msu<-(with(post, (bsib[,4]*sigma_bsib+mu_bsib)))
# 
# 
# 
# dens(kon, xlim = c(-.5, .2), col = "blue")
# dens(kif, add =T, col = "red")
# dens(kip, add =T, col = "green")
# dens(msu, add =T, col = "goldenrod")
# 
# 
# 
# ############################################################
# ################ WEALTH ####################################
# # 
# # 
# # kif<-(with(post, (bw[,3]*sigma_bw+mu_bw)-(bw[,1]*sigma_bw+mu_bw))) # Konde - kifundi
# # kip<-(with(post, (bw[,3]*sigma_bw+mu_bw)-(bw[,2]*sigma_bw+mu_bw)))# Konde - kipange
# # msu<-(with(post, (bw[,3]*sigma_bw+mu_bw)-(bw[,4]*sigma_bw+mu_bw)))# Konde - msuka
# # 
# # 
# # id<- c(rep('kif', 2000), rep("kip", 2000), rep("msu", 2000))
# # val <- c(kif, kip, msu)
# # 
# # df<-data.frame(id = id,
# #                val = val)
# # 
# # 
# # ggplot(df, aes(x=id, y=val)) + 
# #   geom_violin() +
# #   stat_summary(fun.y = mean,geom = "point") + 
# #   stat_summary(fun.data = data_summary) +
# #   geom_hline(yintercept=0) + 
# #   ylim(-.5, .5) +
# #   coord_flip() +
# #   ylab("Posterior parameter contrast") +
# #   xlab("Site")
# # 
# # 
# # 
# # 
# # 
# # kif<-(with(post, (bw[,1]*sigma_bw+mu_bw))) 
# # kip<-(with(post, (bw[,2]*sigma_bw+mu_bw)))
# # kon<-(with(post, (bw[,3]*sigma_bw+mu_bw)))
# # msu<-(with(post, (bw[,4]*sigma_bw+mu_bw)))
# # 
# # dens(kon)
# # dens(kif)
# # dens(kip)
# 
# ##################################################################################
# ############### FAMILY GIFTS #####################################################
# 
# 
# kif<-(with(post, (bfag[,3]*sigma_bfag+mu_bfag)-(bfag[,1]*sigma_bfag+mu_bfag))) # Konde - kifundi
# kip<-(with(post, (bfag[,3]*sigma_bfag+mu_bfag)-(bfag[,2]*sigma_bfag+mu_bfag)))# Konde - kipange
# msu<-(with(post, (bfag[,3]*sigma_bfag+mu_bfag)-(bfag[,4]*sigma_bfag+mu_bfag)))# Konde - msuka
# 
# 
# id<- c(rep('Kifundi Family Gift', 2000), rep("Kipange Family Gift", 2000), rep("Msuka Family Gift", 2000))
# val <- c(kif, kip, msu)
# 
# df3<-data.frame(id = id,
#                val = val)
# 
# 
# data_summary <- function(x) {
#   m <- mean(x)
#   ymin <- unlist(PI(x, .95)[[1]])
#   ymax <- unlist(PI(x, .95)[[2]])
#   return(c(y=m,ymin=ymin,ymax=ymax))
# }
# 
# ggplot(df3, aes(x=id, y=val)) + 
#   geom_violin(trim = .95)  +
#   stat_summary(fun.y = mean,geom = "point") + 
#   stat_summary(fun.data = data_summary) +
#   geom_hline(yintercept=0) + 
#   ylim(-.38, .5) +
#   coord_flip() +
#   ylab("Posterior parameter contrast") +
#   xlab("Site")
# 
# 
# kif<-(with(post, (bfag[,1]*sigma_bfag+mu_bfag))) 
# kip<-(with(post, (bfag[,2]*sigma_bfag+mu_bfag)))
# kon<-(with(post, (bfag[,3]*sigma_bfag+mu_bfag)))
# msu<-(with(post, (bfag[,4]*sigma_bfag+mu_bfag)))
# 
# dens(kon, xlim = c(-.5, .2), col = "blue")
# dens(kif, add =T, col = "red")
# dens(kip, add =T, col = "green")
# dens(msu, add =T, col = "goldenrod")
# 
# ####################################################################
# ############## FRIEND GIFS #########################################
# 
# 
# kif<-(with(post, (bfrg[,3]*sigma_bfrg+mu_bfrg)-(bfrg[,1]*sigma_bfrg+mu_bfrg))) # Konde - kifundi
# kip<-(with(post, (bfrg[,3]*sigma_bfrg+mu_bfrg)-(bfrg[,2]*sigma_bfrg+mu_bfrg)))# Konde - kipange
# msu<-(with(post, (bfrg[,3]*sigma_bfrg+mu_bfrg)-(bfrg[,4]*sigma_bfrg+mu_bfrg)))# Konde - msuka
# 
# 
# id<- c(rep('Kifundi Friends Gift', 2000), rep("Kipange Friends Gift", 2000), rep("Msuka Friends Gift", 2000))
# val <- c(kif, kip, msu)
# 
# df4<-data.frame(id = id,
#                val = val)
# 
# 
# data_summary <- function(x) {
#   m <- mean(x)
#   ymin <- unlist(PI(x, .95)[[1]])
#   ymax <- unlist(PI(x, .95)[[2]])
#   return(c(y=m,ymin=ymin,ymax=ymax))
# }
# 
# ggplot(df4, aes(x=id, y=val)) + 
#   geom_violin(trim = .95)  +
#   stat_summary(fun.y = mean,geom = "point") + 
#   stat_summary(fun.data = data_summary) +
#   geom_hline(yintercept=0) + 
#   ylim(-.38, .5) +
#   coord_flip() +
#   ylab("Posterior parameter contrast") +
#   xlab("Site")
# 
# 
# kif<-(with(post, (bfrg[,1]*sigma_bfrg+mu_bfrg))) 
# kip<-(with(post, (bfrg[,2]*sigma_bfrg+mu_bfrg)))
# kon<-(with(post, (bfrg[,3]*sigma_bfrg+mu_bfrg)))
# msu<-(with(post, (bfrg[,4]*sigma_bfrg+mu_bfrg)))
# 
# dens(kon, xlim = c(-.5, .2), col = "blue")
# dens(kif, add =T, col = "red")
# 
# 
# 
# dens(kon, xlim = c(-.5, .2), col = "blue")
# dens(kif, add =T, col = "red")
# dens(kip, add =T, col = "green")
# dens(msu, add =T, col = "goldenrod")
# 
# ####################################################################
# ############## FRIEND GIFS #########################################
# library(matrixStats)
# 
# kif<-(with(post, (mu_a + bs[,3]*sigma_s + rowMedians(bg[,3,]) + rowMedians(bi[,3,]) + rowMedians(kED[,3,]) + rowMedians(kAG[,3,]) + rowMedians(bp[,7:12]))-
#              (mu_a +bs[,1]*sigma_s + rowMedians(bg[,1,]) + rowMedians(bi[,1,]) + rowMedians(kED[,1,]) + rowMedians(kAG[,1,]) + rowMedians(bp[,1,])))) # Konde - kifundi
# kip<-(with(post, (mu_a +bs[,3]*sigma_s + rowMedians(bg[,3,]) + rowMedians(bi[,3,]) + rowMedians(kED[,3,]) + rowMedians(kAG[,3,]) + rowMedians(bp[,3,]))-
#              (mu_a +bs[,2]*sigma_s + rowMedians(bg[,2,]) + rowMedians(bi[,2,]) + rowMedians(kED[,2,]) + rowMedians(kAG[,2,]) + rowMedians(bp[,2,])))) # Konde - kifundi
# msu<-(with(post, (mu_a +bs[,3]*sigma_s + rowMedians(bg[,3,]) + rowMedians(bi[,3,]) + rowMedians(kED[,3,]) + rowMedians(kAG[,3,]) + rowMedians(bp[,3,]))-
#              (mu_a +bs[,4]*sigma_s + rowMedians(bg[,4,]) + rowMedians(bi[,4,]) + rowMedians(kED[,4,]) + rowMedians(kAG[,4,]) + rowMedians(bp[,4,])))) # Konde - kifundi
# 
# 
# id<- c(rep('Kifundi Intercept', 2000), rep("Kipange Intercept", 2000), rep("Msuka Intercept", 2000))
# val <- c(kif, kip, msu)
# 
# df5<-data.frame(id = id,
#                 val = val)
# 
# #################################################################
# ############# ALL ###############################################
# 
# df_new<- rbind(rbind(rbind(rbind(df, df2), df3), df4),df5)
# ggplot(df_new, aes(x=id, y=val, fill = id)) + 
#   geom_violin(trim = FALSE)  +
#   theme_classic() +
#   #theme_clean() +
#   scale_fill_manual(values=rep(c("#264653", "#2A8D8F", "#E9C46A", "#F4A261", "#E76F51"),3)) +  
#   stat_summary(fun.y = mean,geom = "point") + 
#   stat_summary(fun.data = data_summary) +
#   geom_hline(yintercept=0, color = "red", alpha = .5) + 
#   ylim(-.5, .5) +
#   coord_flip() +
#   ylab("Posterior parameter contrast") +
#   xlab("") + theme(legend.position="none") +
#   geom_vline(xintercept = 4.5, linetype = "dashed", color = "grey") +
#   geom_vline(xintercept = 8.5, linetype = "dashed", color = "grey") 
#   
# out <- c()
# for(i in 1:ncol(post$bad)){
#   out<-c(out, cor(post$bad[,2], post$sigma_bad))
# }
# 
# plot(out)
# 
# 
# ####################################################################################
# ##################### HOURS ########################################################
# 
# 
# 
# kif<-(with(post, (bh[,3]*sigma_bh+mu_bh)-(bh[,1]*sigma_bh+mu_bh))) # Konde - kifundi
# kip<-(with(post, (bh[,3]*sigma_bh+mu_bh)-(bh[,2]*sigma_bh+mu_bh)))# Konde - kipange
# msu<-(with(post, (bh[,3]*sigma_bh+mu_bh)-(bh[,4]*sigma_bh+mu_bh)))# Konde - msuka
# 
# 
# id<- c(rep('Kifundi Friends Gift', 2000), rep("Kipange Friends Gift", 2000), rep("Msuka Friends Gift", 2000))
# val <- c(kif, kip, msu)
# 
# df4<-data.frame(id = id,
#                 val = val)
# 
# 
# data_summary <- function(x) {
#   m <- mean(x)
#   ymin <- unlist(PI(x, .95)[[1]])
#   ymax <- unlist(PI(x, .95)[[2]])
#   return(c(y=m,ymin=ymin,ymax=ymax))
# }
# 
# ggplot(df4, aes(x=id, y=val)) + 
#   geom_violin(trim = .95)  +
#   stat_summary(fun.y = mean,geom = "point") + 
#   stat_summary(fun.data = data_summary) +
#   geom_hline(yintercept=0) + 
#   ylim(-.38, .5) +
#   coord_flip() +
#   ylab("Posterior parameter contrast") +
#   xlab("Site")
# 
# 
# 
# kif<-(with(post, (bh[,1]*sigma_bh+mu_bh))) 
# kip<-(with(post, (bh[,2]*sigma_bh+mu_bh)))
# kon<-(with(post, (bh[,3]*sigma_bh+mu_bh)))
# msu<-(with(post, (bh[,4]*sigma_bh+mu_bh)))
# 
# dens(kon, xlim = c(-.5, .5), col = "blue")
# dens(kif, add =T, col = "red")
# 
# 
# 
# dens(kon, xlim = c(-.5, .5), col = "blue")
# dens(kif, add =T, col = "red")
# dens(kip, add =T, col = "green")
# dens(msu, add =T, col = "goldenrod")


####################################################################################
###################### GRAVE YARD ##################################################
# 
# 
# m1dolRE <- stan("C:/Users/jeffr/OneDrive/Documents/Risk/code/stan_models/random_effects_dol.stan", chains = 1, cores = 1, data = dat_inv2, iter = 1000)
# saveRDS(m1dolRE, "m1dolRE.RDS")
# 
# 
# m1dolRET <- stan("C:/Users/jeffr/OneDrive/Documents/Risk/code/stan_models/random_effects_test.stan", chains = 1, cores = 1, data = dat_inv2, iter = 1000)
# saveRDS(m1dolRET, "m1dolRET.RDS")
# 
# m1dolREMWMA <- stan("C:/Users/jeffrey_andrews/OneDrive/Documents/Risk/code/stan_models/random_effects_dol_wma.stan", chains = 1, cores = 1, data = dat_inv2, iter = 1000)
# saveRDS(m1dolREMWMA , "m1dolREWMA.RDS")
# 
# 
# 
# random_effects_dol_wma_tnc <- stan("C:/Users/jeffrey_andrews/OneDrive/Documents/Risk/code/stan_models/random_effects_dol_wma_tnc.stan", chains = 1, cores = 1, data = dat_inv2, iter = 1000)
# saveRDS(random_effects_dol_wma_tnc , "random_effects_dol_wma_tnc.RDS")
# 
# 
# 
# random_effects_dol_wma_tnc_sp <- stan("C:/Users/jeffr/OneDrive/Documents/Risk/code/stan_models/random_effects_dol_wma_tnc_sp.stan", chains = 1, cores = 1, data = dat_inv2, iter = 1000)
# saveRDS(random_effects_dol_wma_tnc_sp, "random_effects_dol_wma_tnc_sp.RDS")
# 
# 
# m1dolwoRE <- stan("C:/Users/jeffr/OneDrive/Documents/Risk/code/stan_models/dol_wo_RE.stan", chains = 1, cores = 1, data = dat_inv2, iter = 1000)
# saveRDS(m1dolwoRE, "m1dolwoRE.RDS")
# 
# m1dolwoRES <- stan("C:/Users/jeffr/OneDrive/Documents/Risk/code/stan_models/dol_wo_RE_Seas.stan", chains = 1, cores = 1, data = dat_inv2, iter = 1000)
# saveRDS(m1dolwoRES, "m1dolwoRES.RDS")
# 
# 
# ##################################################
# ########### GENERATE FOR ONLY PAST JULY ##########
# 
# # GENERATE DF WHERE ONLY THOSE WHO APPEARE MORE THAN ONCE ARE REPSETENDED
# dat_inv3 <- dat_inv
# 
# 
# a<-table(dat_inv3$id)
# only_one<-as.numeric(attributes(a)$dimnames[[1]][which(a == 1)])
# for(i in names(dat_inv3)){
#   if(length(dat_inv3[[i]]) > 100){
#     print(i)
#     dat_inv3[[i]]<-dat_inv3[[i]][-which(dat_inv$id %in% only_one)]
#   }
# }
# 
# 
# a<-table(dat_inv3$p)
# for(i in names(dat_inv3)){
#   if(length(dat_inv3[[i]]) > 100){
#     print(i)
#     dat_inv3[[i]]<-dat_inv3[[i]][-which((dat_inv2$p < 7 & dat_inv2$y ==2022))]
#   }
# }
# #####################################################################
# #####################################################################
# #######################
# ############NOTE THAT THIS IS A MAJOR CHEAT, JUST PEAKING
# #######################
# ######################################################################
# ######################################################################
# 
# dat_inv3$p <- as.integer(as.factor(dat_inv3$p))
# 
# # CREATE FEEDERS FOR GPs
# # Age Gaussian Process
# temp<-dat_inv3$ag
# temp[dat_inv3$ag>17 & dat_inv3$ag <28] <- 1
# temp[dat_inv3$ag>27 & dat_inv3$ag <38] <- 2
# temp[dat_inv3$ag>37 & dat_inv3$ag <48] <- 3
# temp[dat_inv3$ag>47 & dat_inv3$ag <58] <- 4
# temp[dat_inv3$ag>57 & dat_inv3$ag <68] <- 5
# temp[dat_inv3$ag>67 & dat_inv3$ag <78] <- 6
# temp[dat_inv3$ag>77 & dat_inv3$ag <88] <- 7
# temp[dat_inv3$ag>87 & dat_inv3$ag <98] <- 8
# 
# # temp<-dat2$P
# n <- length(unique(temp))
# ua <- sort(unique(temp))
# DmatAG <- matrix(NA, ncol = n, nrow = n)
# for(i in 1:n){
#   for(j in 1:n){
#     DmatAG[i,j] <- abs(ua[i]-ua[j]) 
#   }
# }
# dat_inv3$ag <- temp
# 
# # Adults gaussian Processs
# temp<-dat_inv3$ad
# 
# n <- max(max(unique(temp)),length(unique(temp)))
# ua <- 1:n
# DmatAD <- matrix(NA, ncol = n, nrow = n)
# for(i in 1:n){
#   for(j in 1:n){
#     DmatAD[i,j] <- abs(ua[i]-ua[j]) 
#   }
# }
# 
# 
# 
# # EDUCATION
# temp<-dat_inv3$edu
# # temp<-dat2$P
# n <- max(unique(temp))
# ua <- 1:n
# DmatED <- matrix(NA, ncol = n, nrow = n)
# for(i in 1:n){
#   for(j in 1:n){
#     DmatED[i,j] <- abs(ua[i]-ua[j]) 
#   }
# }
# 
# # Distance Matricies
# dat_inv3$DmatAG <- DmatAG
# dat_inv3$DmatED <- DmatED/10
# dat_inv3$DmatAD <- DmatAD/10
# 
# 
# # GROUP LEVEL OFFSETS
# dat_inv3$adh <- NA
# for(i in unique(dat_inv3$hid)){
#   ind = which(dat_inv3$hid==i)
#   dat_inv3$adh[ind] <- dat_inv3$ad[ind]-mean(dat_inv3$ad[ind])
# }
# 
# # INDIVIDUAL LEVEL 
# dat_inv3$adi <- NA
# for(i in unique(dat_inv3$id)){
#   ind = which(dat_inv3$id==i)
#   dat_inv3$adi[ind] <- dat_inv3$ad[ind]-mean(dat_inv3$ad[ind])
# }
# 
# 
# 
# dat_inv3$ad_med <- NA
# for(i in unique(dat_inv3$id)){
#   ind <- which(dat_inv3$id==i)
#   dat_inv3$ad_med[ind] <- median(dat_inv3$ad[ind])   
# }
# 
# 
# # 
# # dat_inv3$dol_mean <- NA
# # for(i in unique(dat_inv3$id)){
# #   ind <- which(dat_inv3$hid==i)
# #   dat_inv3$dol_mean[ind] <- median(dat_inv3$dol[ind])   
# # }
# 
# 
# dat_inv3$ad_fac <- as.integer(as.factor(dat_inv3$ad))
# dat_inv3$ad <- standardize((dat_inv3$ad))
# dat_inv3$ch <- standardize((dat_inv3$ch))
# dat_inv3$te <- standardize((dat_inv3$te))
# dat_inv3$sp <- standardize((dat_inv3$sp))
# 
# dat_inv3$p <- as.integer(as.factor(dat_inv3$p))
# dat_inv3$ag <- as.integer(as.factor(dat_inv3$ag))
# dat_inv3$edu <- as.integer(as.factor(dat_inv3$edu))
# 
# 
# # Helper values
# dat_inv3$LAG <- length(unique(dat_inv3$ag))
# dat_inv3$LP <- length(unique(dat_inv3$p))
# dat_inv3$LAD <- nrow(dat_inv3$DmatAD)
# dat_inv3$LED <- nrow(dat_inv3$DmatED)
# dat_inv3$NOBS <- length(dat_inv3$p)
# dat_inv3$N <- length(dat_inv3$p)
# dat_inv3$P1 <- 1:max(dat_inv3$p)
# dat_inv3$NS <- length(unique(dat_inv3$s))
# dat_inv3$NID <- length(unique(dat_inv3$id))
# dat_inv3$NHH <- length(unique(dat_inv3$hid))
# 
# # Other Vars
# dat_inv3$s <-as.integer(as.factor(dat_inv3$s))
# dat_inv3$g <-as.integer(as.factor(dat_inv3$g))
# dat_inv3$I <-as.integer(as.factor(dat_inv3$I))
# dat_inv3$sib = standardize(dat_inv3$sib_h_l + dat_inv3$sib_s_l)
# 
# dat_inv3$dol <- standardize(dat_inv3$dol)
# dat_inv3$w <- standardize(dat_inv3$w)
# dat_inv3$id <- as.integer(as.factor(dat_inv3$id))
# dat_inv3$hid <- as.integer(as.factor(dat_inv3$hid))
# dat_inv3$m <- as.integer(as.factor(dat_inv3$m))
# # 
# dat_inv3$noc <- standardize(dat_inv3$noc)
# 
# 
# 
# 
