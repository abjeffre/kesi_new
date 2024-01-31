#####################################################################
#################### RUN SECTOR MODELS ###############################
library(rethinking)

# ms1=stan("code/stan_models/recover_sec.stan", data = data , chains = 1, iter = 1000)
# ms2=stan("code/stan_models/sec_heirarchical.stan", data = data , chains = 1, iter = 1000)
# ms3=stan("code/stan_models/sec_heirarchical_impute.stan", data = data , chains = 1, iter = 1000)
# ms4=stan("code/stan_models/sec_heirarchical_impute_predict.stan", data = data , chains = 1, iter = 300)
# ms5=stan("code/stan_models/sec_heirarchical_impute_predict_full_merged.stan", data = data , chains = 1, iter = 2000)
# ms6=stan("code/stan_models/sec_heirarchical_impute_predict_full_merged_ramadan.stan", data = data , chains = 1, iter = 400)
# ms7=stan("code/stan_models/sec_heirarchical_impute_predict_full_merged_ramadan_random_non_centered.stan", data = data , init = 0, cores = 1, chains = 1, iter = 300)
# ms8=stan("code/stan_models/sec_heirarchical_impute_predict_full_merged_ramadan_random_non_centered2.stan", data = data, init =0, cores = 1, chains = 1, iter = 300)
# ms8=stan("code/stan_models/sec_heirarchical_impute_predict_full_merged_ramadan_random_non_centered_gp2.stan", data = data, init =0, cores = 1, chains = 1, iter = 300)
ms9=stan("code/stan_models/sec_heirarchical_impute_predict_full_merged_ramadan_random_non_centered_gp.stan", data = data, init =0, cores = 4, chains = 4, iter = 2000)


# ms10=stan("code/stan_models/sec_heirarchical_impute_predict_full_merged_ramadan_random_non_centered_gp_26.stan", data = data, init =0, cores = 1, chains = 1, iter = 300)


precis(ms10)

