nz_con <- connectNzSDSE()
library(dplyr)
library(dbplyr)

source("./shiny/db-explorer/server_tools/navig_tools/decompose_filter.R")
df<-tbl(nz_con,in_schema("DTL_ADAM_MARSAL","DATA_AMENAGE"))
nzsdse::cleanDTL(nz_con,listTable = "TESTNA",askConfirm = F)
dbWriteTable(nz_con,"DTL_ADAM_MARSAL.TESTNA",data.frame(type_row=c("NA_ROW","NORMAL_ROW"),C1=c(NA,"NA"),C2=c(NULL,"NA")))


df<-tbl(nz_con,in_schema("DTL_ADAM_MARSAL","TESTNA"))


raw_filter_cmd <- "PER_ID==\"95891955\""
raw_filter_cmd <- "PER_ID==\"95891955\" &\n  DATE_EVT==\"2021-02-15\" &\n  DUREE_FERME==\"120\" &\n  DUREE_SURSIS==\"240\""
# raw_filter_cmd <- "PER_ID==\"95891955\" &\n  DATE_EVT >= \"2021-02-15\" &\n  DUREE_FERME%in%\"120\" &\n  DUREE_SURSIS %ian% \"240\""

pre_processed_filter_cmd <- preProcessFilter(raw_filter_cmd)
pre_processed_filter_cmd

decomposed_filter <- decomposeFilter(pre_processed_filter_cmd)
decomposed_filter



recomposed_filter=purrr::map(decomposed_filter, function(filter_components){
  # browser()
  
  recompose=filter_components
  
  if(get_class(df_filtered %>% head(10) %>% collect())[filter_components$column]=="date"){
    recomposed_filter=paste0(filter_components$column,
                             filter_components$operator,
                             "sql(\"TO_DATE('",filter_components$value,"', 'YYYY-MM-DD')","\")")
    
    recompose$value <- paste0("sql(\"TO_DATE('",filter_components$value,"', 'YYYY-MM-DD')","\")")
    
  }
  else{
    recomposed_filter=paste0(filter_components$column,
                             filter_components$operator,
                             paste0("'",filter_components$value,"'"))
  }
  # return(recompose)
  
})
recomposed_filter=paste(unlist(recomposed_filter),collapse = " & ")


df_filtered <- df %>% filter(!!rlang::parse_expr(recomposed_filter))
df_filtered %>% show_query()

