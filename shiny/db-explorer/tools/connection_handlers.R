### Gestion des fonctions de connexions aux base de données

## Fonctions pour la connexion à Netezza :

all_nz_tables <- reactive({
  
  # browser()
  # print("aze")
  query_nz_tables <- "SELECT * FROM _V_OBJECT_DATA"
  all_tables <- dplyr::as_tibble(DBI::dbGetQuery(connexion(), query_nz_tables))

  all_tables <- all_tables %>% filter(OBJTYPE %in% c("TABLE","VIEW"))
  
  return(all_tables)

}) %>% bindCache(input$db) 
# %>% 
# bindEvent(c(input$db,input$password_pg,input$selected_schema))
# Le bind event fait planter l'application, surement lié à un problème du package rJava référencé ici : 
# https://forum.posit.co/t/error-in-jcheck-java-exception-no-description-because-tostring-failed/161960/11

