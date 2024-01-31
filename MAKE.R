###############################################
############## MAKE FILE ######################

#######################
##### LOAD PACKAGES ###
#devtools::install_github("Glender/DutchDayDummies")
library("DutchDayDummies")
library(tidyverse)
require(readxl)
require(ggplot2)
require(cowplot)
require(viridis)
require(dplyr)
require(stringr)
require(rethinking)
require(lubridate)
require(gridBase)
require(stringr)
require(vioplot)
require("openxlsx")
require(stringdist)

#######################
## IDENTIFY COMPUTER ##
set_project_wd <- function(folder){
  user=Sys.info()[[6]]
  if(user=="jeffrey_andrews") setwd(paste0("C:/Users/jeffrey_andrews/OneDrive/Documents/", folder))
  else if(user=="Jeff") setwd(paste0("C:/Users/Jeff/OneDrive/Documents/", folder))
  else if(user == 'jeffr')  setwd(paste0("C:/Users/jeffr/OneDrive/Documents/", folder))
  else if(user == 'pretelli')  setwd(paste0("C:/Users/pretelli/HiDrive/Academia/", folder))
  else if(user == 'mclark1')  setwd(paste0("C:/Users/mclark1/Documents/Pemba_Project/", folder))
  else if(user =='Matt') setwd(paste0("C:/Users/Matt/Documents/Pemba_Project/",folder))
}
set_project_wd("kesi")

############################################
############ GET FUNCTIONS #################

for(i in dir("code/functions/")){
  source(paste0("code/functions/",i))
}
source("code/functions.R")

#############################################
############ IDENTIFY MAKEFILE###############
MAKE = TRUE
  
#############################################
############ GET BASE DATA ##################

source("code/script/cleaning/scrapekobo.R")
source("code/script/cleaning/getdate.R")
source("code/script/cleaning/buildIds.R")
source("code/script/cleaning/cleaning.R")
source("code/script/cleaning/fix_dates_during_germany_trip.R")
#source("code/script/cleaning/getData.R")

##################################################################
################ DEFINE VARIABLES FOR DATA PROCESSING ############

# This variable determines which shehia are used to constrcut the average household income
use_shehia <- c("kifundi", "msuka_magharibi", "konde", "kipange", "chokocho", "mtambili", "mkoani", "chumbageni")
# Define Z score for the amount of predicted variability in all bio-physical drivers
z = 2
# Define years for the simulation of future bio-physical data
years = 2
# The scaling factor variable scales GDP to avoid over/underflow in stan and allows for the parameterization to be build to the actual scale of the income data
scaling_factor = 1

############################################
########## PROCESS DATA FOR ANALYSIS #######



source("code/script/analysis/job_categorization.R")
#source("code/script/analysis/build_inc_seperate_ag.R")
source("code/script/analysis/build_inc.R")
#source("code/script/analysis/build_inc_no_miguu.R") # This switches on or off the collection of crabs and other shore creatures as fishing
source("code/script/cleaning/AggregatePeriodizeAllPredictors.R")

source("code/script/cleaning/MergeCasesAndPredictors.R")

#############################################
############ ANALYSIS## #####################

source("code/script/analysis/get_analysis_data.R")
#source("code/script/analysis/simulate_future_BioPhysical_and_gp_prepz15.R")
source("code/script/analysis/simulate_future_BioPhysical_and_gp_prep.R")
source("code/script/analysis/get_gdp_and_timber_price.R")
source("code/script/analysis/run_models.R")

#############################################
########### MAIN FIGURE PLOTS ###############
source("code/plotting/base_plot.R")
source("code/plotting/plot_climate_effects.R")
source("code/plotting/heatmaps.R")
source("code/plotting/generate_zscore_dosage.R")

source("code/plotting/PosteriorPredsEnvOnly.R")  #env only model needs to run before this!
source("code/plotting/PosteriorPredsWorking.R")  #Need to make sure the full model object is in the correct place
# source("code/script/analysis/plot_gdp.R")
