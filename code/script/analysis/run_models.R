#####################################################################
#################### RUN FULL MODEL #################################
library(rethinking)

data$softplus_alpha <- 5
month_means <- matrix(NA, ncol = data$K, nrow = data$NM)
for(i in 1:12){
  for(k in 1:data$K)
    month_means[i, k] <- mean(data$y[which(data$month[data$obs_ind] == i),k])
}
data$month_means <- month_means

# SECTOR SPECIFIC
sectors <- stan("code/stan_models/soft_sectors_periodized.stan", data = data, init =0, cores = 4, chains = 4, iter = 1000)
saveRDS(sectors, "data/sectors.RDS")


# Full Model Summed across Sectors
full <- stan("code/stan_models/soft_full_periodized.stan", data = data, init =0, cores = 4, chains = 4, iter = 1000)
saveRDS(full, "data/full.RDS")

# Only environmental 

environ <- stan("code/stan_models/no_gdp_model.stan", data = data, init =0, cores = 4, chains = 4, iter = 2000)
saveRDS(environ, "data/environ.RDS")


#################################
##### Just one more TEST ########

# Income across all sectors summed
full_wz <- stan("code/stan_models/full_log_model_wz.stan", data = data, init =0, cores = 4, chains = 4, iter = 1000)
saveRDS(full_wz, "data/full_wz.RDS")


sectors_l <- stan("code/stan_models/sectors_low_var.stan", data = data, init =0, cores = 4, chains = 4, iter = 2000)
saveRDS(sectors_l, "data/sectors_l.RDS")

sectors_h <- stan("code/stan_models/sectors_high_var.stan", data = data, init =0, cores = 4, chains = 4, iter = 2000)
saveRDS(sectors_h, "data/sectors_high.RDS")

sectors_simple <- stan("code/stan_models/sectors_log.stan", data = data, init =0, cores = 4, chains = 4, iter = 2000)
saveRDS(sectors_simple, "data/sectors_simple.RDS")


mh<-readRDS("data/sectors_high.RDS")
ml<-readRDS("data/sectors_l.RDS")
ms<-readRDS("data/sectors_simple.RDS")
m <- readRDS("data/sectors.RDS")
m15 <- readRDS("data/sectorsz15.RDS")
mg <- readRDS("data/sectors_gamma_mu.RDS")
compare(ms, ml, mh, m, sectors_l, m15, sectors_h, sectors_g)

#######################################################
############ Z 1.5 ####################################

pars = c("kbp", "gamma_mu", "kWind", "kWaves", "kMoisture", "kZscore", "kHeat", "br", "sigma_r", "phi", "a", "bgdp", 
           "kp", "kX", "btimber", "br_k", "sigma_r_k", "kMoistureKesi", "kHeatKesi", "kLag")


# Income across all sectrors remains seperate
sectors2 <- stan("code/stan_models/sectors.stan", data = data, init =0, cores = 4, chains = 4, iter = 1000)
saveRDS(sectors, "data/sectorsz15.RDS")

sectors_l <- stan("code/stan_models/sectors_low_var.stan", data = data, init =0, cores = 4, chains = 4, iter = 1000)
saveRDS(sectors_l, "data/sectors_lz15.RDS")

sectors_h <- stan("code/stan_models/sectors_high_var.stan", data = data, init =0, cores = 4, chains = 4, iter = 1000)
saveRDS(sectors_h, "data/sectors_highz15.RDS")

sectors_gmu <- stan("code/stan_models/sectors_gamma_mu.stan", data = data, init =0, cores = 4, chains = 4, iter = 2000)
saveRDS(sectors_gamma_mu, "data/sectors_gamma_mu.RDS")

sectors_c <- stan("code/stan_models/sectors_mean_constant.stan", data = data, init =0, cores = 4, chains = 4, iter = 2000)
saveRDS(sectors_c, "data/sectors_mean_constant.RDS")

sectors_m <- stan("code/stan_models/sectors_all_moisture.stan", data = data, init =0, cores = 4, chains = 4, iter = 500, pars = pars)
saveRDS(sectors_m, "data/sectors_all_moisture.RDS")

sectors_condensed2 <- rstan::stan("code/stan_models/sectors_gdp_simp.stan", data = data, init =0, cores = 4, chains = 4, iter = 2000)
saveRDS(sectors_condensed2, "data/sectors_condensed2.RDS")

sectors_condensed4 <- rstan::stan("code/stan_models/sectors_periodized.stan", data = data, init =0, cores = 4, chains = 4, iter = 2000)
saveRDS(sectors_condensed3, "data/sectors_condensed3.RDS")

test <- rstan::stan("code/stan_models/testing_gamma_cov.stan", data = data, init =0, cores = 4, chains = 4, iter = 2000)
# Heat is averaged across all lag periods
sectors_condensed3 <- rstan::stan("code/stan_models/sectors_gdp_simp.stan", data = data, init =0, cores = 4, chains = 4, iter = 2000)
saveRDS(sectors_condensed3, "data/sectors_condensed3.RDS")

# Heat is instantious in lag period
sectors_condensed4 <- rstan::stan("code/stan_models/sectors_gdp_simp.stan", data = data, init =0, cores = 4, chains = 4, iter = 2000)
saveRDS(sectors_condensed4, "data/sectors_condensed4.RDS")

sectors_condensed4 <- rstan::stan("code/stan_models/sectors_periodized.stan", data = data, init =0, cores = 4, chains = 4, iter = 2000)
saveRDS(sectors_condensed4, "data/sectors_condensed4.RDS")

saveRDS(sectors_condensed4, "data/sectors_condensed4.RDS")

test <- stan("code/stan_models/soft_sectors_periodized.stan", data = data, init =0, cores = 4, chains = 4, iter = 1000)

#Very small bgdp prior
test2 <- stan("code/stan_models/soft_sectors_periodized.stan", data = data, init =0, cores = 4, chains = 4, iter = 1000)


mod <- cmdstan_model("code/stan_models/soft_sectors_periodized.stan")
fit_optimize <- mod$optimize(data = data)

fit_optimize <- mod$pathfinder(data = data)

fit_optimize$output()