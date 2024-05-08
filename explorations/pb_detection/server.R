

source("ui.R")

server = (function(input, output,session) {
  
  selected_ticket=reactive({
    # req(input$incident_table_selection_rows_selected)
    incident_table()[input$incident_table_selection_rows_selected,IN_TICKET]
  })
  
  output$pr_selection_ui=renderUI({
    if(input$PR_STATUS=="all"){
      list=c("",problem_table_for_selection$PR_TICKET)
      names(list)=c("",problem_table_for_selection$label_pr_ticket)
    }else{
      
      list=c("",problem_table_for_selection[PR_STATUS_EN==input$PR_STATUS]$PR_TICKET)
      names(list)=c("",problem_table_for_selection[PR_STATUS_EN==input$PR_STATUS]$label_pr_ticket)
    }
    selectInput("PR_SELECTION",NULL,list,selected = list[1])
  })
  pr_ticket=reactive(input$PR_SELECTION)
  
  init_date_value=reactive(problems[PR_TICKET==pr_ticket(),c(as.Date(PR_SUBMIT_DATE)-365,as.Date(PR_SUBMIT_DATE)+365)])
  output$init_date_ui=renderUI({
    req(input$PR_SELECTION)
    ui=sliderInput("DateRangeSelection","Plage temporelle :",
                   min=as.Date("2015-01-01"),max=Sys.Date(),
                   value=c(init_date_value()[1],init_date_value()[2]), step = 1)
    return(ui)
  })
  in_ticket_filter_gamme3=reactive({
    if(input$filter_gamme3=="all"){
      res=incidents$IN_TICKET
    }else{
      res=incidents[IN_GAMME_EN_3==input$filter_gamme3]$IN_TICKET  
    }  
    return(res)
  })
  filter_gamme_4_choices=reactive({
    # browser()
    if(input$filter_gamme3=="All"){
      choices=incidents[,unique(IN_GAMME_EN_4)]
    }else{
      choices=incidents[IN_TICKET%in%in_ticket_filter_gamme3(),unique(IN_GAMME_EN_4)]
    }
    return(c("all",choices))
  })
  output$filter_gamme_4_ui <- renderUI({
    req(input$PR_SELECTION)
    ui=selectInput("filter_gamme4","Motorisation / Sous-gamme :",filter_gamme_4_choices())
    return(ui)
  })
  in_ticket_filter_gamme4=reactive({
    if(!is.null(input$filter_gamme4)){
      if(input$filter_gamme4=="all"){
        res=incidents$IN_TICKET
      }else{
        res=incidents[IN_GAMME_EN_4==input$filter_gamme4]$IN_TICKET  
      }  
    }else{
      res=NULL
    }
    return(res)
  })
  in_departement_filter=reactive({
    if(!is.null(input$filter_departement)){
      if(input$filter_departement=="all"){
        res=incidents$IN_TICKET
      }else{
        res=incidents[IN_DEPARTMENT_EN_2==input$filter_departement]$IN_TICKET  
      }  
    }else{
      res=NULL
    }
    return(res)
  })
  in_ticket_filter_symptom=reactive({
    if(input$filter_symptom=="all"){
      res=incidents$IN_TICKET
    }else{
      res=incidents[IN_SYMPTOM_EN==input$filter_symptom]$IN_TICKET  
    }  
    return(res)
  })
  in_filter_text=reactive({
    
    list_word=paste0(strsplit(input$text_filter," ")[[1]],collapse = "&")
    pat=paste0("(?=.*",paste0(strsplit(input$text_filter," ")[[1]],collapse = ")(?=.*"),")")
    
    # res=incidents[grep(input$text_filter,IN_DESCRIPTION)]$IN_TICKET
    res=incidents[grepl(pat,IN_DESCRIPTION,perl=T)]$IN_TICKET
    # res=c(res,incidents[grep(input$text_filter,IN_SOLUTION)]$IN_TICKET)
    res=c(res,incidents[grepl(pat,IN_SOLUTION,perl=T)]$IN_TICKET)
    res=c(res,incidents[grepl(pat,IN_TRANSLATED_DESCRIPTION,perl=T)]$IN_TICKET)
  })
  in_ticket_filter_categorie=reactive({
    if(input$filter_categorie=="all"){
      res=incidents$IN_TICKET
    }else{
      res=incidents[IN_CAT_2==input$filter_categorie]$IN_TICKET  
    }  
    return(res)
  })
  
  words_highlight=function(desc_in,desc_pb,color="#FFF9CE"){
    
    # desc_in=" problem with   engine test engine blabla1"
    # desc_pb="  blabla2 engine   blabla3 engine aze aze aze aze qsd"
    
    trim <- function (x) gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", x, perl=TRUE)
    
    corrected_desc_in=trim(desc_in)
    corrected_desc_pb=trim(desc_pb)
    
    which_in=which(strsplit(trimws(corrected_desc_in)," ")[[1]]%chin%strsplit(corrected_desc_pb," ")[[1]])
    which_pb=which(strsplit(trimws(corrected_desc_pb)," ")[[1]]%chin%strsplit(corrected_desc_in," ")[[1]])
    
    words_in=unique(strsplit(corrected_desc_in," ")[[1]][which_in])
    words_pb=unique(strsplit(corrected_desc_pb," ")[[1]][which_pb])
    
    for (i in seq_along(words_in)){
      # corrected_desc_in=gsub(paste0("",words_in[i],""),paste0("<mark>",words_in[i],"</mark>"),corrected_desc_in)
      # corrected_desc_in=gsub(paste0("",words_in[i],""),paste0("<FONT style='BACKGROUND-COLOR: #FFC8C8'>",words_in[i],"</FONT>"),corrected_desc_in)
      corrected_desc_in=gsub(paste0("",words_in[i],""),paste0("<FONT style='BACKGROUND-COLOR: ",color,"'>",words_in[i],"</FONT>"),corrected_desc_in)
    }    
    for (i in seq_along(words_pb)){
      # corrected_desc_pb=gsub(paste0("",words_pb[i],""),paste0("<mark>",words_pb[i],"</mark>"),corrected_desc_pb)
      corrected_desc_pb=gsub(paste0("",words_pb[i],""),paste0("<FONT style='BACKGROUND-COLOR: ",color,"'>",words_pb[i],"</FONT>"),corrected_desc_pb)
    }  
    res=list(corrected_desc_in,corrected_desc_pb)
    return(res)
  }
  
  pb_in_table_comparison <- reactive({
    
    req(input$PR_SELECTION)
    # browser()
    PR_info=problems[PR_TICKET==pr_ticket()]
    
    IN_info=incidents[IN_TICKET%in%selected_ticket()]
    
    PR_info_melt=melt(PR_info,id.vars = "PR_TICKET",measure.vars =cols_PR_table_comparison)
    IN_info_melt=melt(IN_info,id.vars = "IN_TICKET",measure.vars =cols_IN_table_comparison)
    
    PR_info_melt[,merge_var:=gsub("PR_","",variable)]
    IN_info_melt[,merge_var:=gsub("IN_","",variable)]
    PR_info_melt[,merge_var:=gsub("CAT_3","CATEGORIE",merge_var)]
    IN_info_melt[,merge_var:=gsub("CAT_2","CATEGORIE",merge_var)]
    
    PR_info_melt[,order_display_row:=1:.N]
    
    scores=matching_data[PR_TICKET==pr_ticket() & IN_TICKET%in%selected_ticket()]
    scores=melt(scores,id.vars = "PR_TICKET",measure.vars = grep("_note$",names(scores),value=T))
    scores[,merge_var:=gsub("_note","",variable)]
    scores[,merge_var:=gsub("GAMME_","GAMME_EN_",merge_var)]
    scores[,merge_var:=gsub("SYMPTOM","SYMPTOM_EN",merge_var)]
    scores[,merge_var:=gsub("CAT","CATEGORIE",merge_var)]
    
    
    setkey(IN_info_melt,merge_var)
    setkey(PR_info_melt,merge_var)
    setkey(scores,merge_var)
    
    # pb_in_table=PR_info_melt[IN_info_melt,IN_val:=i.value]
    # pb_in_table=PR_info_melt[IN_info_melt,IN_val:=i.value][order(order_display_row),.(Dimension=merge_var,Incident=IN_val,Probleme=value)]
    pb_in_table=PR_info_melt[IN_info_melt,IN_val:=i.value][scores,note:=i.value]
    pb_in_table=pb_in_table[order(order_display_row),.(Dimension=merge_var,Probleme=value,Incident=IN_val,note)]
    
    #renaming the dimension of the labels
    pb_in_table[Dimension=="GAMME_EN_2",Dimension:="PU"]
    pb_in_table[Dimension=="GAMME_EN_3",Dimension:="Gamme machine"]
    pb_in_table[Dimension=="GAMME_EN_4",Dimension:="Motorisation / Sous-gamme"]
    pb_in_table[Dimension=="MODEL",Dimension:="Modèle"]
    pb_in_table[Dimension=="CATEGORIE",Dimension:="Catégorie"]
    pb_in_table[Dimension=="SYMPTOM_EN",Dimension:="Symptôme"]
    pb_in_table[Dimension=="SERIAL_NUMBER",Dimension:="Serial Number"]
    
    return(pb_in_table)
  })
  
  incident_table=reactive({
    
    req(input$PR_SELECTION)
    res=matching_data[PR_TICKET==pr_ticket()][order(get(init_score_selected)),
                                              .(IN_TICKET,
                                                is_match=match_ok,
                                                score_final=round(get(note_selected),2),
                                                score_text=round(get(score_col),2))]
    res=res[,is_match:=ifelse(is_match,1,0)]
    
    res=res[IN_TICKET%in%in_ticket_filter_gamme3()]
    res=res[IN_TICKET%in%in_ticket_filter_gamme4()]
    res=res[IN_TICKET%in%in_departement_filter()]
    res=res[IN_TICKET%in%in_ticket_filter_categorie()]
    res=res[IN_TICKET%in%in_filter_text()]
    res=res[IN_TICKET%in%in_ticket_filter_symptom()]
    
    # browser()
    setkey(incidents,IN_TICKET)
    setkey(res,IN_TICKET)
    res[incidents,PR_TICKET_match:=i.PR_TICKET_match]
    res[,matching_status:="Non matché"]
    res[is_match==1,matching_status:="Matché"]
    res[is_match==0,matching_status:=paste0("Matché à ",PR_TICKET_match)]
    res[is_match==0 & PR_TICKET_match=="",matching_status:="Non matché"]
    res[PR_TICKET_match=="",is_match:=NA]
    # res[,is_match:=NULL]
    res[,PR_TICKET_match:=NULL]
    
    
    
    list_incident_in_date_range=incidents[IN_SUBMIT_DATE>=input$DateRangeSelection[1] & IN_SUBMIT_DATE<=input$DateRangeSelection[2],IN_TICKET]
    res=res[IN_TICKET%in%list_incident_in_date_range][order(-score_final)]
    
    return(res)
    
    
  })
  
  in_desc=reactive({
    req(input$PR_SELECTION)
    req(selected_ticket())
    ln=incidents[IN_TICKET%in%selected_ticket(),MS_LN]
    if(length(ln)==0){
      return(NULL)
    }
    
    if(ln=="en"){
      res=incidents[IN_TICKET%in%selected_ticket(),IN_DESCRIPTION]
    }else{
      res=incidents[IN_TICKET%in%selected_ticket(),IN_TRANSLATED_DESCRIPTION]
      res=paste0("<b>! TRANSLATED ! Original language : ",toupper(ln),"</b><br>",res)
    }
    
    if(input$text_filter!=""){
      words=strsplit(input$text_filter," ")[[1]]
      for (x in words){
        res<-gsub(x,paste0("<FONT style='font-size:20px;'>",x,"</FONT>"),res)
        res<-gsub(x,paste0("<b>",x,"</b>"),res)  
      }
    }
    return(res)
  })
  pb_desc=reactive({
    req(input$PR_SELECTION)
    # req(selected_ticket())
    problems[PR_TICKET==pr_ticket(),PR_DESCRIPTION]
  })
  in_disc=reactive({
    req(input$PR_SELECTION)
    req(selected_ticket())
    res=incidents[IN_TICKET%in%selected_ticket(),IN_SOLUTION]
    if(input$text_filter!=""){
      words=strsplit(input$text_filter," ")[[1]]
      for (x in words){
        res<-gsub(x,paste0("<FONT style='font-size:20px;'>",x,"</FONT>"),res)
        res<-gsub(x,paste0("<b>",x,"</b>"),res)  
      }
    }
    return(res)
  })
  
  in_clean_desc=reactive({
    req(input$PR_SELECTION)
    req(selected_ticket())
    incidents[IN_TICKET%in%selected_ticket(),get(in_desc_col)]
  })
  pb_clean_desc=reactive({
    req(input$PR_SELECTION)
    # req(selected_ticket())
    problems[PR_TICKET==pr_ticket(),get(pb_desc_col)]
  })
  pb_corpus=reactive({
    req(input$PR_SELECTION)
    # req(selected_ticket())
    tickets_corpus=matching_data[PR_TICKET==pr_ticket() & (match_ok),IN_TICKET]
    corpus=incidents[
      IN_TICKET%in%tickets_corpus & !is.na(get(in_desc_col)) & !get(in_desc_col)=="NA",
      .(corp=paste(paste0(IN_TICKET,": ",get(in_desc_col)),collapse = " <br/> ")),
      by=PR_TICKET_match
      ]$corp
    
    if(nrow(data.table(selected_ticket()))!=0){
      corpus=gsub(selected_ticket(),paste0("<b>",selected_ticket(),"</b>"),corpus)
    }
    return(corpus)
  })
  
  output$pb_text=renderText(pb_desc())
  output$in_text=renderText(in_desc())
  output$in_disc=renderText(in_disc())
  
  output$pb_corpus=renderText({
    if(identical(pb_corpus(),character(0))){
      return(NULL)
    }
    if(identical(in_clean_desc(),character(0))){
      return(pb_corpus())
    }
    words_highlight(pb_corpus(),in_clean_desc(),color = "#FFC8C8")[[1]]
  })
  output$pb_text_clean=renderText({
    # browser()
    if(identical(in_clean_desc(),character(0))){
      return(pb_clean_desc())
    }
    res=words_highlight(in_clean_desc(),pb_clean_desc())
    res=res[[2]]
    return(res)
  })
  output$in_text_clean=renderText({
    
    res=in_clean_desc()
    if(identical(in_clean_desc(),character(0))){
      res=NULL
    }else{
      if(!identical(pb_clean_desc(),character(0))){
        res=words_highlight(res,pb_clean_desc())[[1]]
      }
      if(!identical(pb_corpus(),character(0))){
        res=words_highlight(res,pb_corpus(),color = "#FFC8C8")[[1]]
      }
    }
    return(res)
  })
  
  output$incident_table_selection <- renderDataTable({
    datatable(incident_table(),
              selection = "single",
              options=list(pageLength = 15,
                           dom="tp",
                           columnDefs = list(list(visible=FALSE, targets=c(2)))
              )
              #           # options=list(dom="tp",scrollY="200px",scrollCollapse=T,paging=F)
              # )%>% formatStyle('matching_status',target="row",
              #   backgroundColor = styleEqual(c('Matché','Matché à un autre PB'), c('yellow','#9BF59B'))
    )%>% formatStyle('is_match',target="row",
                     backgroundColor = styleEqual(c('1','0'), c('yellow','#9BF59B'))             
    )
  })
  
  output$pb_in_comparison_DT <- DT::renderDataTable({
    datatable(pb_in_table_comparison(),
              options = list(
                dom = 't',
                ordering=F,
                autowidth=T,
                columnDefs = list(list(width = '25%', targets = 0),
                                  list(width = '30%', targets = 1),
                                  list(width = '30%', targets = 2),
                                  list(width = '15%', targets = 3))
                # selection = list(mode="single",target = 'cell')
              ),
              rownames = F,
              class = 'hover',
              selection = list(mode="single",target = 'cell')
    )%>%formatStyle(
      "note",
      background = styleColorBar(0:10, 'lightblue'),
      # backgroundSize = '98% 88%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    )%>%formatStyle( 0, target= 'row', lineHeight='60%'
    )%>%formatStyle('Probleme',target="cell",valueColumns = 'Dimension',color=styleEqual("TICKET",c("blue"))
    )%>%formatStyle('Incident',target="cell",valueColumns = 'Dimension',color=styleEqual("TICKET",c("blue"))
    )
  })
  
  outputOptions(output,"pr_selection_ui",priority=25)
  outputOptions(output,"init_date_ui",priority=20)
  outputOptions(output,"filter_gamme_4_ui",priority=19)
  outputOptions(output,"incident_table_selection",priority=5)
  outputOptions(output,"pb_in_comparison_DT",priority=1)
  outputOptions(output,"pb_text",priority=1)
  outputOptions(output,"in_text",priority=1)
  outputOptions(output,"in_disc",priority=1)
  outputOptions(output,"pb_corpus",priority=1)
  outputOptions(output,"pb_text_clean",priority=1)
  outputOptions(output,"in_text_clean",priority=1)
  
  tab2_dt_selected_in=reactive({
    # browser()
    # if(nchar(input$tr_in_ticket)==11 & nrow(incidents[IN_TICKET==input$tr_in_ticket])==1){
    #   return(incidents[IN_TICKET==input$tr_in_ticket])
    # }else{
    #   return(NULL)
    # }
    return(incidents[IN_TICKET==input$tr_in_ticket])
  })
  output$tab2_ticket_desc=renderText(tab2_dt_selected_in()$IN_DESCRIPTION)
  output$tab2_ticket_translation=renderText(tab2_dt_selected_in()[,ifelse(MS_LN=="en","Text is already in english",IN_TRANSLATED_DESCRIPTION)])
  
  USER <- reactiveValues(Logged = Logged)
  
  observeEvent(input$PR_SELECTION,{
    updateTextInput(session,"text_filter",value = "")
  })
  observeEvent(input$pb_in_comparison_DT_cell_clicked,{
    req(input$PR_SELECTION)
    req(input$pb_in_comparison_DT_cell_clicked)
    clicked=input$pb_in_comparison_DT_cell_clicked$value
    if(length(clicked)==0) return(NULL)
    if(grepl("P\\d{6}-\\d{3}",clicked)){
      link=paste0("https://manitougroup_assist.easyvista.com/autoconnect_mail.php?field1=5C0F051E590F056F1D&field2=&field4=%7BD7406E06-3ACD-4D60-AD2A-9AA5CCADC96B%7D&field5=ViewDialog&field6=",clicked,"&field7=RFC_NUMBER")
    }else if(grepl("I\\d{6}-\\d{3}",clicked)){
      # fo  =paste0("https://manitougroup_assist.easyvista.com/autoconnect_mail.php?field1=5C0F051E590F056F1D&field2=&field4=%7BAF1AE6AD-FF4B-41B0-93B3-99BEF6052B12%7D&field5=ViewDialog&field6=",clicked,"&field7=RFC_NUMBER")
      # hd  =paste0("https://manitougroup_assist.easyvista.com/autoconnect_mail.php?field1=5C0F051E590F056F1D&field2=&field4=%7B07ED9C68-6172-48EA-8A58-90912B0A283E%7D&field5=ViewDialog&field6=",clicked,"&field7=RFC_NUMBER")
      # link=paste0("https://manitougroup_assist.easyvista.com/autoconnect_mail.php?field1=5C0F051E590F056F1D&field2=&field4=%7BAF1AE6AD-FF4B-41B0-93B3-99BEF6052B12%7D&field5=ViewDialog&field6=",clicked,"&field7=RFC_NUMBER")
      link=paste0("https://manitougroup_assist.easyvista.com/autoconnect_mail.php?field1=5C0F051E590F056F1D&field2=&field4=%7B07ED9C68-6172-48EA-8A58-90912B0A283E%7D&field5=ViewDialog&field6=",clicked,"&field7=RFC_NUMBER")
      
    }else{
      return(NULL)
    }
    
    getPage(link)
  })
  
  getPage<-function(link) {
    js$browseURL(link)
    # return((browseURL(link)))
    # return((browseURL(link)))
  }
  
  ############ Authentification
  observe({ 
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <<- isolate(input$textInput_userName)
          Password <- sha256(isolate(input$passwordInput_pw))
          Id.username <- which(logins$id == Username)
          Id.password <- which(logins$hashed_pw == Password)
          if (length(Id.username) > 0 & length(Id.password) > 0) {
            if (Id.username == Id.password) {
              USER$Logged <- TRUE
            } 
          }
        } 
      }
    }    
  })
  
  ############ gestion UI multiple
  observe({
    if (USER$Logged == FALSE){
      output$page <- renderUI({
        div(class="outer",do.call(bootstrapPage,c("",ui_login())))
      })
    }
    if (USER$Logged == TRUE){
      output$page <- renderUI({
        # div(class="outer",do.call(navbarPage,c(inverse=TRUE,id="nav",title = "Problem matching",ui_app())))
        start_time<<-Sys.time()
        ui_app()
      })
      print(ui)
    }
  })
  
  onStop(function(){
    # browser()
    if(file.exists(paste0(path_data,"connection_log.csv"))){
      res=fread(paste0(path_data,"connection_log.csv"))
      dt=data.table(user=Username,start=format(start_time,"%Y-%m-%d %H:%M:%S"),end=format(Sys.time(),"%Y-%m-%d %H:%M:%S"))
      res=rbind(res,dt)
    }else{
      res=data.table(user=Username,start=format(start_time,"%Y-%m-%d %H:%M:%S"),end=format(Sys.time(),"%Y-%m-%d %H:%M:%S"))
    }
    fwrite(res,file=paste0(path_data,"connection_log.csv"))
  })
  
})
