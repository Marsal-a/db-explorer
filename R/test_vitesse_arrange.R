source("./shiny/db-explorer/global_tools/libraries.R")

ora <- connectOraSDSE()
nz <- connectNzSDSE()

arrange_alt1 <- function(connecteur,schema,table,col_order,filter=NULL){
  
  con <- connecteur()
  
  tbl_ora_1 <- tbl(con,in_schema(schema,table))
  if(!is.null(filter)){
    tbl_ora_1<-tbl_ora_1 %>% filter(!!rlang::parse_expr(filter))  
  }
  
  remote_q <- tbl_ora_1 %>% 
    arrange(!!rlang::parse_expr(col_order)) %>% 
    remote_query() %>% 
    as.character()
  
  order_q <- glue::glue("SELECT * FROM ({remote_q}) WHERE (ROWNUM<=1000)")
  df <- tibble(dbGetQuery(con,order_q))
  dbDisconnect(con)
  return(df)
}
arrange_alt2 <- function(connecteur,schema,table,col_order,filter=NULL){
  
  con <- connecteur()
  
  tbl_ora_1 <- tbl(con,in_schema(schema,table))
  if(!is.null(filter)){
    tbl_ora_1<-tbl_ora_1 %>% filter(!!rlang::parse_expr(filter))  
  }
  
  df <- tbl_ora_1 %>% 
    mutate(R=1) %>% 
    group_by(R) %>% 
    window_order(RETENT)%>% 
    # arrange(RETENT) %>% 
    mutate(n=row_number(RETENT))%>% 
    filter(n<1000) %>% collect()
  
  dbDisconnect(con)
  return(df)
}
arrange_alt3 <- function(connecteur,schema,table,col_order,filter=NULL){
  
  con <- connecteur()
  
  tbl_ora_1 <- tbl(con,in_schema(schema,table))
  if(!is.null(filter)){
    tbl_ora_1<-tbl_ora_1 %>% filter(!!rlang::parse_expr(filter))  
  }
  
  df <- tbl_ora_1 %>% 
    group_by(1) %>% 
    mutate(n=row_number(RETENT)) %>% 
    filter(n<1000) %>% 
    collect()
  
  dbDisconnect(con)
  return(df)
}

alt1<-arrange_alt1(connectOraSDSE,"PHAROS_V3","PHAROS_CPH","RETENT")
alt1f=arrange_alt1(connectOraSDSE,"PHAROS_V3","PHAROS_CPH","RETENT","C_TUS=='CPHN71'")
alt2=arrange_alt2(connectOraSDSE,"PHAROS_V3","PHAROS_CPH","RETENT")
alt2f=arrange_alt2(connectOraSDSE,"PHAROS_V3","PHAROS_CPH","RETENT","C_TUS=='CPHN71'")
alt3=arrange_alt3(connectOraSDSE,"PHAROS_V3","PHAROS_CPH","RETENT")
alt3f=arrange_alt3(connectOraSDSE,"PHAROS_V3","PHAROS_CPH","RETENT","C_TUS=='CPHN71'")

library(microbenchmark,lib.loc="~/R_Commun/Adam/custom_lib/AtelierR_data.table/")


rbench_ora<-microbenchmark(
  alt1=arrange_alt1(connectOraSDSE,"PHAROS_V3","PHAROS_CPH","RETENT"),
  alt1f=arrange_alt1(connectOraSDSE,"PHAROS_V3","PHAROS_CPH","RETENT","C_TUS=='CPHN71'"),
  alt2=arrange_alt2(connectOraSDSE,"PHAROS_V3","PHAROS_CPH","RETENT"),
  alt2f=arrange_alt2(connectOraSDSE,"PHAROS_V3","PHAROS_CPH","RETENT","C_TUS=='CPHN71'"),
  alt3=arrange_alt3(connectOraSDSE,"PHAROS_V3","PHAROS_CPH","RETENT"),
  alt3f=arrange_alt3(connectOraSDSE,"PHAROS_V3","PHAROS_CPH","RETENT","C_TUS=='CPHN71'"),
  
  times=3
)

rbench_nz<-microbenchmark(
  alt1=arrange_alt1(connectOraSDSE,"PHAROS_V3","PHAROS_CPH","RETENT"),
  alt1f=arrange_alt1(connectOraSDSE,"PHAROS_V3","PHAROS_CPH","RETENT","C_TUS=='CPHN71'"),
  alt2=arrange_alt2(connectOraSDSE,"PHAROS_V3","PHAROS_CPH","RETENT"),
  alt2f=arrange_alt2(connectOraSDSE,"PHAROS_V3","PHAROS_CPH","RETENT","C_TUS=='CPHN71'"),
  alt3=arrange_alt3(connectOraSDSE,"PHAROS_V3","PHAROS_CPH","RETENT"),
  alt3f=arrange_alt3(connectOraSDSE,"PHAROS_V3","PHAROS_CPH","RETENT","C_TUS=='CPHN71'"),
  
  times=3
)


rbench_pg<-microbenchmark(
  alt1=arrange_alt1(connectOraSDSE,"PHAROS_V3","PHAROS_CPH","RETENT"),
  alt1f=arrange_alt1(connectOraSDSE,"PHAROS_V3","PHAROS_CPH","RETENT","C_TUS=='CPHN71'"),
  alt2=arrange_alt2(connectOraSDSE,"PHAROS_V3","PHAROS_CPH","RETENT"),
  alt2f=arrange_alt2(connectOraSDSE,"PHAROS_V3","PHAROS_CPH","RETENT","C_TUS=='CPHN71'"),
  alt3=arrange_alt3(connectOraSDSE,"PHAROS_V3","PHAROS_CPH","RETENT"),
  alt3f=arrange_alt3(connectOraSDSE,"PHAROS_V3","PHAROS_CPH","RETENT","C_TUS=='CPHN71'"),
  
  times=3
)


