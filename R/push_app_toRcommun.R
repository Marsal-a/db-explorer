pushAppToDir <- function(dir){
  unlink(dir,recursive = T)
  dir.create(dir)
  file.copy(from = list.files("./shiny/db-explorer/", full.names = TRUE), 
            to=dir, 
            recursive = TRUE)
}


master_location <- "~/R_Commun/Adam/db-explorer/"

push_db_explorer <- function(){
  current_branch <- gert::git_branch()
  if(current_branch=="master"){
    ask<-askYesNo("Pousser sur le répertoire master?",prompts = c("y","n","NA"),default = FALSE)
    if(ask){
      pushAppToDir(master_location)  
    }
  }else{
    branch_dir<-paste0("~/R_Commun/Adam/db-explorer","-",fs::path_sanitize(current_branch),"/")
    pushAppToDir(branch_dir)
    launch_shiny_backgroundJob(branch_dir,name = current_branch)
  }
    
}

push_db_explorer()

# out="~/R_Commun/Adam/db-explorer/"
# unlink(out,recursive = T)
# dir.create(out)
# file.copy(from = list.files("./shiny/db-explorer/", full.names = TRUE), 
#           to=out, 
#           recursive = TRUE)
# 
# 
# launch_shiny_backgroundJob(out,name = "db-explorer")

