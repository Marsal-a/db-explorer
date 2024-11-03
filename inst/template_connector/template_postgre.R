connectors <- list()

postgreSQLConnector <- list(

  connect_function = function(user,pw){

    dbhost="{{HOST}}"
    dbport="{{PORT]}"
    dbname="{{DBNAME]}"

    if(!is.null(user) & !is.null(pw)){
      con <- DBI::dbConnect(drv = RPostgres::Postgres(),
                                dbname = dbname,
                                host = dbhost,
                                port = dbport,
                                user = user,
                                password = pw,
                                bigint = "numeric")

    }else{
      con <- NULL
    }
    return(con)
  },

  req_login = TRUE,

  list_schemas_function = function(con){

    query <- "select
                nspname as name
              from pg_catalog.pg_namespace
              where nspacl is not null
              and nspname not in ('public', 'pg_catalog', 'information_schema')
              order by name"

    schema_table <- DBI::dbGetQuery(con, query)
    schema <- schema_table$name
  },

  list_tables_function = function(con,dbname){

    query <- sprintf("
                     select
                      table_type as type,
                      table_name as name
                     from information_schema.tables
                     where table_schema not in ('pg_catalog', 'information_schema')
                     and table_schema in ('%s')
                     and table_schema not like 'pg_toast%%'
                     order by name",dbname)

    tables_table <- DBI::dbGetQuery(con, query)
    if (nrow(tables_table) > 0) {
      tables_table$type <- tolower(tables_table$type)
      tables_table[which(tables_table$type == "base table"), "type"] <- "table"
    }
    tables <- tables_table$name
  },

  remote_table_function = function(con,dbname,tablename){
    lz <- dplyr::tbl(con, dbplyr::in_schema(dbplyr::sql(isolate(dbname)), tablename))
  }
)

connectors$pg <- postgreSQLConnector
