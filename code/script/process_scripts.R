#############################################
############ EASY RUN TO GET DATA ###########

set_project_wd <- function(folder){
  user=Sys.info()[[6]]
  if(user=="jeffrey_andrews") setwd(paste0("C:/Users/jeffrey_andrews/OneDrive/Documents/", folder))
  else if(user=="Jeff") setwd(paste0("C:/Users/Jeff/OneDrive/Documents/", folder))
  else if(user == 'jeffr')  setwd(paste0("C:/Users/jeffr/OneDrive/Documents/", folder))
}
set_project_wd("DOL")
source("code/script/cleaning/getData.R")
source("code/script/cleaning/getSurvey.R")
dl=getData(load_imputed = F)

