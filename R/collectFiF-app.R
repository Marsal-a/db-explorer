# remotes::install_github("stla/findInFiles")
## Copie FIF app

out="./explorations/FiF/"
unlink(out,recursive = T)
dir.create(out)
file.copy(from = list.files(system.file("shinyApp", package = "findInFiles"), full.names = TRUE), 
          to=out, 
          recursive = TRUE)


a<-rstudioapi::jobAdd("j1")
