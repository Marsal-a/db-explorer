
nz<-connectNzSDSE()
pg<-connectPostgreSDSE()
or<-connectOraSDSE()


DP1602_MESURE <- tbl(nz, in_schema(sql("DTL_APPI."), "DP1602_MESURE"))
DP1602_MESURE %>% filter(grepl('Toyota|Mazda', MES_LIBELLE))
DP1602_MESURE %>% filter(MES_LIBELLE %like% "%surveillance%")

query="
  SELECT *
  FROM DTL_APPI.ADMIN.DP1602_MESURE 
  WHERE MES_LIBELLE LIKE ('%retrait%')
  limit 10
"
dbGetQuery(nz,query)



asj <-  tbl(pg, in_schema("asj", "asj"))
asj %>% filter(grepl('Toyota|Mazda', corresp_nom)) %>% show_query()

query="
  SELECT *
  FROM asj.asj
  WHERE (corresp_nom ~ 'MEYER')
  --WHERE ((corresp_nom) ~~ ('MEYER|SUDAREVIC'))
"

dbGetQuery(pg,query)
