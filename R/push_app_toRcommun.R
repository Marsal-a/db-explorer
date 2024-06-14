out="~/R_Commun/Adam/db-explorer/"
unlink(out,recursive = T)
dir.create(out)
file.copy(from = list.files("./shiny/db-explorer/", full.names = TRUE), 
          to=out, 
          recursive = TRUE)


# options(dbexplorer_dbhost="Netezza")
# options(dbexplorer_dbhost="")

# dbexplorer_dbhost="Netezza"
# dbexplorer_schema="DTL_ADAM_MARSAL"
# dbexplorer_table = "DP1602_OBLIGATIONS"
launch_shiny_backgroundJob(out,name = "db-explorer")

# dbexplorer_dbhost=""
# dbexplorer_schema=""
# launch_shiny_backgroundJob(out,name = "db-explorer")
