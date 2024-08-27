db_choices <- names(connectors)
default_db     <- ifelse(exists("dbexplorer_dbhost"), dbexplorer_dbhost, "")
default_schema <- ifelse(exists("dbexplorer_schema"), dbexplorer_schema, "")
default_table  <- ifelse(exists("dbexplorer_table"), dbexplorer_table, "")

### Render des .md
# rmarkdown::render(paste0(getOption("path_db_explorer"),"tools/help/help_filters.md"))

addResourcePath("help","./global_tools/help/")

## Cloture des sessions au maximum aprés 12h d'ouverture
start_time <- Sys.time()
max_session_time <- 3600*12

logg_full <- c()
path_out_log <- "~/R_Commun/Adam/logs/db-explorer/"

sql_keywords <- c("SELECT", "FROM", "WHERE", "AND", "OR", "NOT", 
                  "JOIN", "INNER JOIN", "LEFT JOIN", "RIGHT JOIN", "FULL JOIN", 
                  "ON", "GROUP BY", "HAVING", "ORDER BY", "LIMIT", "OFFSET", 
                  "DISTINCT", "COUNT", "SUM", "AVG", "MAX", "MIN", "BETWEEN", 
                  "LIKE", "IN", "EXISTS", "CASE", "WHEN", "THEN", "ELSE", "END", 
                  "IS", "NULL", "TRUE", "FALSE", "ALL", "ANY", "SOME", "ASC", 
                  "DESC", "UNION", "INTERSECT", "EXCEPT", "WITH", "RECURSIVE")

n_rows_collected <- 1000


