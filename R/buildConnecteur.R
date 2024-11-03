createPostgreSQLConnector <- function(host,dbname,port=5432,filename=NULL){

  read_template <- readLines(system.file("template_connector","template_postgre.R",package="dbExplorer"))

  read_template <- gsub("{{HOST}}",host, read_template,fixed = T)
  read_template <- gsub("{{PORT]}",port, read_template,fixed = T)
  read_template <- gsub("{{DBNAME]}",dbname, read_template,fixed = T)

  if(is.null(filename)){
    filename="PostgreSQLConnector.R"
  }

  writeLines(read_template,paste0("./",filename))
  file.edit(paste0("./",filename))

}
