options(
  path_db_explorer = ifelse(grepl("/shiny/db-explorer", getwd()),"../../",".")
)

# browser()
source(paste0(getOption("path_db_explorer"),"/R/create_sqlite_db.R"))

db_choices=c("sqlite","nz")

list_schema_SQLite<- function(con){
  return("Default")
}

list_table_SQLite <- function(con){
  "SELECT name FROM sqlite_master WHERE type='table'"
  tables <- dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type='table'")  
  return(tables)
}
