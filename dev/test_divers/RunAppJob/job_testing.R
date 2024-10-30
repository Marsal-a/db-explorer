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


## test job shinyApp : 
###https://stackoverflow.com/questions/64462965/start-r-shiny-app-as-a-background-job-programmatically

rstudioapi::jobRunScript(path = "./R/RunAppJob/test_runJob_shiny.R",importEnv = T,exportEnv = "R_GlobalEnv",name = "FIF")
Sys.sleep(5)
browseURL(url = "http://127.0.0.1:3838")

eval(parse(text = 'invisible(
      rstudioapi::jobRunScript(path = "./R/RunAppJob/test_runJob_shiny.R",importEnv = TRUE, exportEnv = "R_GlobalEnv"));
           Sys.sleep(5);
           browseURL(url = "http://127.0.0.1:3838");
           rstudioapi::executeCommand(commandId = "activateConsole")'
           )
     )

httpuv::runServer(host = "127.0.0.1", port = httpuv::randomPort(),app="./explorations/test_simple_app/")



