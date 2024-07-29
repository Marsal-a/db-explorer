lib_to_install="~/R_Commun/Adam/custom_lib/db-explorer/"
# lib_to_install=.libPaths()[1]

run_vanilla <- function(cmd){
  
  system_cmd=paste0("R --vanilla -e '",cmd,"'")
  system(system_cmd)
}

install.packages("fontawesome",lib=lib_to_install)
install.packages("shinyAce",lib=lib_to_install)
install.packages("shinyhelper",lib=lib_to_install)
install.packages("shinyjs",lib=lib_to_install)

run_vanilla(glue::glue('install.packages("crayon",lib="{lib_to_install}",repos="https://cloud.r-project.org/")'))
run_vanilla(glue::glue('remotes::install_version("DBI","1.1-3",upgrade = "never",lib="{lib_to_install}",repos="https://cloud.r-project.org/")'))
run_vanilla(glue::glue('remotes::install_version("pillar","1.9.0",upgrade = "never",lib="{lib_to_install}",repos="https://cloud.r-project.org/")'))
run_vanilla(glue::glue('remotes::install_version("tibble","3.2.1",upgrade = "never",lib="{lib_to_install}",repos="https://cloud.r-project.org/")'))
run_vanilla(glue::glue('remotes::install_version("tidyselect","1.2.0",upgrade = "never",lib="{lib_to_install}",repos="https://cloud.r-project.org/")'))
run_vanilla(glue::glue('remotes::install_version("dplyr","1.1.2",upgrade = "never",lib="{lib_to_install}",repos="https://cloud.r-project.org/")'))
run_vanilla(glue::glue('remotes::install_version("glue","1.6.2",upgrade = "never",lib="{lib_to_install}",repos="https://cloud.r-project.org/")'))
run_vanilla(glue::glue('remotes::install_version("dbplyr","2.4-0",upgrade = "never",lib="{lib_to_install}",repos="https://cloud.r-project.org/")'))

