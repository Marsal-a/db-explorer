library(devtools)

# usethis::create_package("./",check_name = FALSE)
# usethis::use_mit_license()

## Package utilisés dans les fonctions R du packages :
usethis::use_package("rstudioapi",type = "Imports")
usethis::use_package("glue",type = "Imports")
usethis::use_package("httpuv",type = "Imports")
usethis::use_package("httr",type = "Imports")

## Packages utilisés dans l'application shiny
library(dplyr)
tibble(renv::dependencies("./inst/shinyApp/")) %>%
  group_by(Package) %>%
  summarise(r=list(gsub(getwd(),"",Source))) %>%
  print(n=100)


use_package("crayon",type="Suggests")
use_package("data.table",type="Suggests")
use_package("DBI",type="Suggests")
use_package("dbplyr",type="Suggests")
use_package("dplyr",type="Suggests")
use_package("DT",type="Suggests")
use_package("fansi",type="Suggests")
use_package("fontawesome",type="Suggests")
use_package("fs",type="Suggests")
use_package("glue",type="Suggests")
use_package("htmltools",type="Suggests")
use_package("magrittr",type="Suggests")
use_package("purrr",type="Suggests")
use_package("rlang",type="Suggests")
use_package("shiny",type="Suggests")
use_package("shinyAce",type="Suggests")
use_package("shinyBS",type="Suggests")
use_package("shinyjs",type="Suggests")
use_package("shinyWidgets",type="Suggests")
use_package("stringi",type="Suggests")
use_package("stringr",type="Suggests")
use_package("withr",type="Suggests")


