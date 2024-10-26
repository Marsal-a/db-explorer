sqlLite_objects <- list(
  connect_function = function(user,pw){
    con_sqlite <- dbConnect(RSQLite::SQLite(), ":memory:")
    dbWriteTable(con_sqlite, "mtcars", mtcars)
    dbWriteTable(con_sqlite, "CO2", data.frame(CO2))
    dbWriteTable(con_sqlite, "billboard", data.frame(tidyr::billboard))
    con_sqlite
  },
  req_login = FALSE,
  list_schemas_function = function(con){
    'default'
  },
  list_tables_function = function(con,dbname){
    dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type='table'")
  },
  remote_table_function = function(con,dbname,tablename){
    lz <- dplyr::tbl(con, tablename)
  }
)

netezza_objects <- list(
  connect_function = function(user,pw){
    library(nzsdse)
    connectNzSDSE()
  },
  req_login = FALSE,
  list_schemas_function = function(con){
    schemas_df <- nzsdse:::nzSDSEListDb(con)
    schemas <- schemas_df$name
    return(schemas)
  },
  list_tables_function = function(con,dbname){
    NAVIG_all_nz_tables_function <- function(con){
      
      React <- reactive({
        
        query_nz_tables <- "
      SELECT
        _V_OBJECT_DATA.OBJTYPE,
        _V_OBJECT_DATA.OBJNAME,
        _V_OBJECT_DATA.DBNAME,
        _V_TABLE_ONLY_STORAGE_STAT.RELTUPLES AS nrows
      FROM _V_OBJECT_DATA
      LEFT JOIN  _V_TABLE_ONLY_STORAGE_STAT
      ON _V_OBJECT_DATA.OBJNAME=_V_TABLE_ONLY_STORAGE_STAT.TABLENAME
      WHERE LOWER(_V_OBJECT_DATA.OBJTYPE) IN ('table','view')
      ORDER BY _V_OBJECT_DATA.OBJNAME
      "
        all_tables <- dplyr::as_tibble(DBI::dbGetQuery(con, query_nz_tables))
        
        all_tables <- all_tables %>% filter(OBJTYPE %in% c("TABLE","VIEW"))
        
        
        return(all_tables)
        
      }) %>% bindCache(con,cache = "session")
      return(React)
    }
    tables <- NAVIG_all_nz_tables_function(con)() %>% filter(DBNAME==dbname) %>% pull(OBJNAME)
    return(tables)
  },
  remote_table_function = function(con,dbname,tablename){
    table_name <- stringr::str_extract(tablename, "^[^ ]*")
    if(packageVersion("dbplyr")>"2.2.0"){
      lz <- dplyr::tbl(con, dbplyr::in_catalog(dbplyr::sql(isolate(dbname)),"ADMIN", tablename))
    }else{
      lz <- dplyr::tbl(con, dbplyr::in_schema(dbplyr::sql(paste0(isolate(dbname),"ADMIN")), tablename))
    }
  }
)

oracle_objects_prod <- list(
  connect_function = function(user,pw){
    connectOraSDSE()
  },
  req_login = FALSE,
  list_schemas_function = function(con){
    query <- "SELECT distinct TABLE_SCHEMA FROM ALL_TAB_PRIVS"
    DBI::dbGetQuery(con,query) %>% arrange(TABLE_SCHEMA) %>% pull(TABLE_SCHEMA)
    # orasdse:::listSchemas(conn = con)
  },
  list_tables_function = function(con,dbname){
    tables_table <- orasdse:::oraSDSEListObjects(con,dbname)
    tables <- tables_table$name
    return(tables)
  },
  remote_table_function = function(con,dbname,tablename){
    lz <- dplyr::tbl(con, dbplyr::in_schema(dbplyr::sql(isolate(dbname)), tablename))
  }
)

oracle_objects_test <- list(
  connect_function = function(user,pw){
    connectOraSDSE(dbname = "TEST10", dbhost = "10.33.0.24")
  },
  req_login = FALSE,
  list_schemas_function = function(con){
    query <- "SELECT distinct TABLE_SCHEMA FROM ALL_TAB_PRIVS"
    DBI::dbGetQuery(con,query) %>% arrange(TABLE_SCHEMA) %>% pull(TABLE_SCHEMA)
    # orasdse:::listSchemas(conn = con)
  },
  list_tables_function = function(con,dbname){
    tables_table <- orasdse:::oraSDSEListObjects(con,dbname)
    tables <- tables_table$name
    return(tables)
  },
  remote_table_function = function(con,dbname,tablename){
    lz <- dplyr::tbl(con, dbplyr::in_schema(dbplyr::sql(isolate(dbname)), tablename))
  }
)

postgre_objects_prod <- list(
  connect_function = function(user,pw){
    if(!is.null(user) & !is.null(pw)){
      con <- connectPostgreSDSE(user = user, password=pw)
    }else{
      con <- NULL
    }
    return(con)
  },
  req_login = TRUE,
  list_schemas_function = function(con){
    schema_table <- pgsdse:::pgsdseListSchemas(con,"prod")
    schema <- schema_table$name
  },
  list_tables_function = function(con,dbname){
    tables_table <- pgsdse:::pgsdseListTablesAndViews(con,"prod",dbname)
    tables <- tables_table$name
  },
  remote_table_function = function(con,dbname,tablename){
    lz <- dplyr::tbl(con, dbplyr::in_schema(dbplyr::sql(isolate(dbname)), tablename))
  }
)

postgre_objects_test <- list(
  connect_function = function(user,pw){
    if(!is.null(user) & !is.null(pw)){
      con <- connectPostgreSDSE(dbhost="10.21.2.217",dbport = "5432",dbname = "test",user = user, password=pw)
    }else{
      con <- NULL
    }
    return(con)
  },
  req_login = TRUE,
  list_schemas_function = function(con){
    schema_table <- pgsdse:::pgsdseListSchemas(con,"test")
    schema <- schema_table$name
  },
  list_tables_function = function(con,dbname){
    tables_table <- pgsdse:::pgsdseListTablesAndViews(con,"test",dbname)
    tables <- tables_table$name
  },
  remote_table_function = function(con,dbname,tablename){
    lz <- dplyr::tbl(con, dbplyr::in_schema(dbplyr::sql(isolate(dbname)), tablename))
  }
)

base_parquet_test <- list(
  connect_function=function(){
    invisible(NULL)
  },
  req_login = FALSE,
  list_schemas_function = function(con){
    print(con)
    schemas=c("parquet partitioned"="~/nfs-slr1/données parquet/parquet_part/",
              "parquet un-partitionned"="~/nfs-slr1/données parquet/parquet_unpart/")
  },
  list_tables_function = function(con,dbname){
    print(con)
    tables_unpart <- list.files(dbname,pattern = "*.parquet$",full.names = F,include.dirs = F)
    tables_part   <- list.dirs(dbname,recursive = F,full.names = F)
    tables <- c(tables_unpart,tables_part)
    return(tables)
  },
  remote_table_function = function(con,dbname,tablename){
    # browser()
    file=paste0(con,dbname,tablename)
    lz_ds <- arrow::open_dataset(file)
    
    return(lz_ds)
  }
)

base_duckdb_test <- list(
  connect_function=function(){
    con <- dbConnect(duckdb())
  },
  req_login = FALSE,
  list_schemas_function = function(con){
    print(con)
    schemas=c("parquet partitioned"="~/nfs-slr1/données parquet/parquet_part/",
              "parquet un-partitionned"="~/nfs-slr1/données parquet/parquet_unpart/")
    return(schemas)
  },
  list_tables_function = function(con,dbname){
    
    print(con)
    tables_unpart <- list.files(dbname,pattern = "*.parquet$",full.names = F,include.dirs = F)
    tables_part   <- list.dirs(dbname,recursive = F,full.names = F)
    
    tables <- c(tables_unpart,tables_part)
    
    return(tables)
  },
  remote_table_function = function(con,dbname,tablename){
    
    lz_ds <- NULL
    
    full_fn <-  paste0(dbname,tablename)
    if(dir.exists(full_fn)){
      tablename_pq <- paste0(dbname,tablename,"/**/*.parquet")
      lz_ds <- tbl(con,glue::glue("read_parquet('{tablename_pq}', hive_partitioning = true)"))
    }else{
      tablename_pq <- paste0(dbname,tablename)
      lz_ds <- tbl(con,glue::glue("read_parquet('{tablename_pq}', hive_partitioning = false)"))
    }
    
    return(lz_ds)
  } 
)


connectors <- list("Netezza"=netezza_objects,
                   "Oracle - Prod"=oracle_objects_prod,
                   "Oracle - Test"=oracle_objects_test,
                   "PostgreSQL - Prod" = postgre_objects_prod,
                   "PostgreSQL - Test" = postgre_objects_test,
                   "arrow" = base_parquet_test,
                   "duckdb" = base_duckdb_test)

