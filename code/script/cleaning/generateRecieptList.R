library(openxlsx)

for(i in unique(d$main$shehia)){
  df=data.frame("Kiongozi ya Kaya" = unique(d$main$hhm_hhh[d$main$shehia == i]))
  write.xlsx(df, paste0("administration/recipt_names_",i,".xlsx" ))
}

