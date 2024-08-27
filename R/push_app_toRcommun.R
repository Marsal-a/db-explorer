out="~/R_Commun/Adam/db-explorer_test/"
unlink(out,recursive = T)
dir.create(out)
file.copy(from = list.files("./shiny/db-explorer/", full.names = TRUE), 
          to=out, 
          recursive = TRUE)


launch_shiny_backgroundJob(out,name = "db-explorer")

