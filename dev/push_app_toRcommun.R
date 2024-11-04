pushAppToDir <- function(dir){
  unlink(dir,recursive = T)
  dir.create(dir)
  file.copy(from = list.files("./inst/shinyApp/", full.names = TRUE),
            to=dir,
            recursive = TRUE)
}

pushConnectorsToDir <- function(dir,file){
  file.copy(from=file,to = dir,overwrite = TRUE)
}

archiveApp <- function(appPath,archive_path){

  archive_f=paste0(archive_path,"db-explorer-",format(Sys.time(), "%Y%m%d_%H%M%S"))

  if (!dir.exists(archive_f)){
    dir.create(archive_f)
  }
  file.copy(from = list.files(appPath, full.names = TRUE),
            to=archive_f,
            recursive = TRUE)

}

master_location <- "~/R_Commun/Adam/db-explorer/"
archive_location <- "~/R_Commun/Adam/archive/"

push_db_explorer <- function(launch=FALSE){
  current_branch <- gert::git_branch()
  if(current_branch=="master"){
    branch_dir <- master_location
    ask<-askYesNo("Pousser sur le répertoire master?",prompts = c("y","n","NA"),default = FALSE)
    if(ask){
      archiveApp(appPath = master_location,archive_path = archive_location)
      pushAppToDir(master_location)
      pushConnectorsToDir(master_location,"./inst/connecteursJustice.R")
    }
  }else{
    branch_dir<-paste0("~/R_Commun/Adam/db-explorer","-",fs::path_sanitize(current_branch),"/")
    pushAppToDir(branch_dir)
    pushConnectorsToDir(branch_dir,"./inst/connecteursJustice.R")

  }
  if(launch)
    launch_shiny_backgroundJob(branch_dir,name = current_branch)

}

push_db_explorer(launch = T)
