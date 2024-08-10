
### Spécificité PostGres : on a besoin d'une invite de mdp pour la connexion
NAVIG_observer_pg_function <- function(inputName){
  obs <- observeEvent(input[[inputName]],{
    if(input[[inputName]]=="PostgreSQL - Prod"){
      showModal(pg_connexion_modal())
    }
  })
  return(obs)
}
NAVIG_observer_pg_function('navig_db')



### Spécificité Netezza : on stock en cache la liste des tables pour les schéma afin de gagner du temps lors d'un changement de schéma
### (1~3 sec par changement de schéma)
# NAVIG_all_nz_tables_function <- function(con){
#   
#   React <- reactive({
#     
#     query_nz_tables <- "
#       SELECT
#         _V_OBJECT_DATA.OBJTYPE,
#         _V_OBJECT_DATA.OBJNAME,
#         _V_OBJECT_DATA.DBNAME,
#         _V_TABLE_ONLY_STORAGE_STAT.RELTUPLES AS nrows
#       FROM _V_OBJECT_DATA
#       LEFT JOIN  _V_TABLE_ONLY_STORAGE_STAT
#       ON _V_OBJECT_DATA.OBJNAME=_V_TABLE_ONLY_STORAGE_STAT.TABLENAME
#       WHERE LOWER(_V_OBJECT_DATA.OBJTYPE) IN ('table','view')
#       ORDER BY _V_OBJECT_DATA.OBJNAME
#       "
#     all_tables <- dplyr::as_tibble(DBI::dbGetQuery(con, query_nz_tables))
#     
#     all_tables <- all_tables %>% filter(OBJTYPE %in% c("TABLE","VIEW"))
#     
#     
#     return(all_tables)
#     
#   }) %>% bindCache(con)
#   return(React)
# }
# 
# netezza_objects <- list(
#   connect_function = connectNzSDSE,
#   list_schemas_function = function(con){
#     schemas_df <- nzsdse:::nzSDSEListDb(con)
#     schemas <- schemas_df$name
#     return(schemas)
#   },
#   list_tables_function = function(con,dbname){
#     tables <- NAVIG_all_nz_tables_function(con)() %>% filter(DBNAME==dbname) %>% pull(OBJNAME)
#     return(tables)
#   },
#   remote_table_function = function(con,dbname,tablename){
#     table_name <- stringr::str_extract(tablename, "^[^ ]*")
#     if(packageVersion("dbplyr")>"2.2.0"){
#       lz <- dplyr::tbl(con, dbplyr::in_catalog(dbplyr::sql(isolate(dbname)),".", tablename))  
#     }else{
#       lz <- dplyr::tbl(con, dbplyr::in_schema(dbplyr::sql(paste0(isolate(dbname),".")), tablename))  
#     }
#   }
# )
# 
# oracle_objects <- list(
#   connect_function = connectOraSDSE,
#   list_schemas_function = function(con){
#     orasdse:::listSchemas(conn = con)
#   },
#   list_tables_function = function(con,dbname){
#     tables_table <- orasdse:::oraSDSEListObjects(con,dbname)
#     tables <- tables_table$name
#     return(tables)
#   },
#   remote_table_function = function(con,dbname,tablename){
#     lz <- dplyr::tbl(con, dbplyr::in_schema(dbplyr::sql(isolate(dbname)), tablename))
#   }
# )
# 
# 
# postgre_objects <- list(
#   connect_function = function(){
#     if(!is.null(pg_password_ok())){
#       con <- connectPostgreSDSE(user = input$username_pg, password=input$password_pg)
#       updateTextInput(session = session,inputId = "password_pg",value = "")
#     }else{
#       con <- NULL
#     }
#     return(con)
#   },
#   list_schemas_function = function(con){
#     req(input$password_pg)
#     req(input$submit_pg_login)
#     schema_table <- pgsdse:::pgsdseListSchemas(con,"prod")
#     schema <- schema_table$name
#   },
#   list_tables_function = function(con,dbname){
#     req(input$submit_pg_login)
#     
#     tables_table <- pgsdse:::pgsdseListTablesAndViews(con,"prod",dbname)
#     tables <- tables_table$name
#   },
#   remote_table_function = function(con,dbname,tablename){
#     lz <- dplyr::tbl(con, dbplyr::in_schema(dbplyr::sql(isolate(dbname)), tablename))
#   }
# )
# 
# connectors <- list("Netezza"=netezza_objects,"Oracle - Prod"=oracle_objects,"PostgreSQL - Prod" = postgre_objects)
