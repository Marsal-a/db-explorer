library(shiny)
library(shinyjs)
library(data.table)
library(DT)
library(purrr)
# library(networkD3)
library(openssl)

if(Sys.info()['nodename']=="man-iotlabdev.directory.manitou.com"){
  path_root<-"./"
  path_data<-"/var/lib/datalab_project/Problem_enrichment/shiny_data/"
  
}else if(Sys.info()['nodename']=="man-iotlabdev.directory.manitou.com"){
  path_root<-"./"
  path_data<-"/01. Data/05. Shinyapp data/"
}else{
  path_root<-"./explorations/pb_detection/"
  path_data<-
}


source(paste0(path_root,"side-functions/shiny-load_prepare_data.R"),local=T)
# source("side-functions/shiny-sankey_function.R",local=T)

js_code <- "
  shinyjs.browseURL = function(url) {
window.open(url,'_blank');
}
"



#### login U1
ui_login=function(){
  tagList(
    div(id = "login",
        wellPanel(textInput("textInput_userName", "Username"),
                  passwordInput("passwordInput_pw", "Password"),
                  br(),actionButton("Login", "Log in"))),
    tags$style(type="text/css", "#login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
  )}

ui_app=function(){
  navbarPage("Problems and incidents matching app",
             
             tabPanel("Problem matching",
                      # titlePanel("Problems incident detection"),
                      sidebarLayout(
                        sidebarPanel(
                          h3("Sélection du problème : "),
                          fluidRow(
                            column(5,selectInput("PR_STATUS",NULL,pr_status_chocies)),
                            # column(8,selectInput("PR_SELECTION","Selection du probleme",problem_list_for_selection,selectize = T))
                            # column(4,selectInput("PR_SELECTION",NULL,problem_list_for_selection,selectize = T)),
                            column(7,uiOutput("pr_selection_ui"))
                          ),
                          # wellPanel(
                          h3("Filtres incidents : "),
                          uiOutput("init_date_ui"),
                          fluidRow(
                            column(6,selectInput("filter_gamme3","Gamme machine :",filter_gamme_3_choices)),
                            column(6,uiOutput("filter_gamme_4_ui"))
                          ),
                          fluidRow(
                            column(6,selectInput("filter_categorie","Catégorie d'incident :",filter_categorie_choices)),
                            column(6,selectInput("filter_departement","Filiale / zone",filter_in_departement_choices))
                          ),
                          fluidRow(
                            column(6,selectInput("filter_symptom","Symptôme :",filter_symptom_choices)),
                            column(6,textInput("text_filter","Recherche des mots :"))
                          ),
                          dataTableOutput("incident_table_selection"),
                          useShinyjs(),
                          extendShinyjs(text = js_code, functions = 'browseURL')
                          # )
                        ),
                        mainPanel(
                          fluidRow(
                            column(1),
                            column(10,dataTableOutput("pb_in_comparison_DT")),
                            column(1)
                          ),
                          hr(),
                          fluidRow(
                            column(1),
                            column(5,
                                   fluidRow(h4('Description raccourcie du probleme')),
                                   fluidRow(htmlOutput("pb_text_clean")),
                                   fluidRow(h4('Description complete du probleme')),
                                   fluidRow(htmlOutput("pb_text",inline = T)),
                                   fluidRow(h4("Description raccourcie des tickets deja rattaches")),
                                   # fluidRow(textOutput("pb_corpus",inline = T)),
                                   # fluidRow(textOutput("pb_corpus",inline = F)),
                                   fluidRow(htmlOutput("pb_corpus",inline = F))
                            ),
                            column(5,
                                   fluidRow(h4("Descritpion raccourcie de l'incident")),
                                   fluidRow(htmlOutput("in_text_clean")),
                                   fluidRow(h4("Description complete de l'incident")),
                                   fluidRow(htmlOutput("in_text")),
                                   fluidRow(h4("Discussion de l'incident")),
                                   fluidRow(htmlOutput("in_disc"))
                            ),
                            column(1)
                          )
                        )
                        
                      )
             )
             ,tabPanel("Ticket translation",
                       column(12,
                              fluidRow(textInput("tr_in_ticket","Ticket number","I1")),
                              fluidRow(
                                column(5,
                                       fluidRow(tags$html(tags$body(h4("Description du ticket")))),
                                       fluidRow(htmlOutput("tab2_ticket_desc"))
                                ),
                                column(1),
                                column(5,
                                       fluidRow(tags$html(tags$body(h4("Traduction")))),
                                       fluidRow(htmlOutput("tab2_ticket_translation"))
                                )
                              )
                       )
             )
             ,theme="bootstrap_bis.css"
             ,tags$style("
                         body {-moz-transform: scale(0.8, 0.8); /* Moz-browsers */zoom: 0.8; /* Other non-webkit browsers */zoom: 80%; /* Webkit browsers */}
                         ")
  )
  
}


ui = (htmlOutput("page"))
# ui = ui_app()
