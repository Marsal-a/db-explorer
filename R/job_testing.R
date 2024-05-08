## test job

nz <- connectNzSDSE()
library(dplyr)
library(dbplyr)
DP1602_AFFAIRE_GROUPE <- tbl(nz, in_catalog(sql("DTL_APPI"),"ADMIN" , "DP1602_AFFAIRE_GROUPE"))
DP1602_AFFAIRE_GROUPE %>% collect()
DP1602_AFFAIRE_GROUPE %>% collect()
DP1602_AFFAIRE_GROUPE %>% collect()
write.csv2(DP1602_AFFAIRE_GROUPE %>% head(200000) %>% collect(),"f1.csv")



rstudioapi::jobRunScript("./R/job_testing.R",
  name = "run du matin",
  encoding = "unknown",
  workingDir = NULL,
  importEnv = FALSE,
  exportEnv = ""
)