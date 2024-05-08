Logged=FALSE
options(encoding = "UTF-8")
#shiny_load_data_function
# load(paste0(path_root,"pb_in_db.Rdata"))
matching_data=fread(paste0(path_data,"matching_data.csv"),encoding = "UTF-8")
# matching_data=fread(paste0(path_data,"matching_data.csv"),encoding = "UTF-8",nrows = 2000000)
incidents=fread(paste0(path_data,"incidents.csv"),encoding = "UTF-8")
incidents[,IN_DESCRIPTION:=gsub("\"{2,}","\"",IN_DESCRIPTION)]
incidents[,IN_CAT_2:=gsub("IN_ ","IN_",IN_CAT_2)]
problems=fread(paste0(path_data,"problems.csv"),encoding = "UTF-8")
problems[,PR_DESCRIPTION:=gsub("\"{2,}","\"",PR_DESCRIPTION)]

logins=fread(paste0(path_data,"logins.txt"))

problem_table_for_selection=matching_data[(match_ok),.N,by=PR_TICKET]
problem_table_for_selection[,label_pr_ticket:=paste0(PR_TICKET," (",N," incidents)")]
setkey(problem_table_for_selection,PR_TICKET)
setkey(problems,PR_TICKET)
problem_table_for_selection[problems,PR_STATUS_EN:=i.PR_STATUS_EN]
problem_table_for_selection=problem_table_for_selection[order(-PR_TICKET)]

init_score_selected="pr_rank"
note_selected="score"
in_desc_col="IN_CLEAN_DESCRIPTION"
pb_desc_col="PR_CLEAN_DESCRIPTION"
score_col="score_str"

cols_PR_table_comparison=c("PR_TICKET","PR_GAMME_EN_2","PR_GAMME_EN_3","PR_GAMME_EN_4","PR_MODEL","PR_CAT_3","PR_SYMPTOM_EN","PR_SERIAL_NUMBER")
cols_IN_table_comparison=c("IN_TICKET","IN_GAMME_EN_2","IN_GAMME_EN_3","IN_GAMME_EN_4","IN_MODEL","IN_CAT_2","IN_SYMPTOM_EN","IN_SERIAL_NUMBER")

pr_status_chocies=c("all",problems[,.N,by=.(PR_STATUS_EN)][order(-N)]$PR_STATUS_EN)
names(pr_status_chocies)=c("all",problems[,.N,by=.(PR_STATUS_EN)][order(-N)][,paste0(PR_STATUS_EN," (",N,")")])

filter_in_departement_choices=c("all",incidents[,unique(IN_DEPARTMENT_EN_2)])

filter_gamme_3_choices=c("all",incidents[,.N,by=(IN_GAMME_EN_3)][order(-N)]$IN_GAMME_EN_3)
names(filter_gamme_3_choices)=gsub("IN_","",filter_gamme_3_choices)

filter_symptom_choices=c("all",incidents[,unique(IN_SYMPTOM_EN)])
names(filter_symptom_choices)=gsub("IN_","",c("all",incidents[,unique(IN_SYMPTOM_EN)]))

filter_categorie_choices=incidents[,unique(IN_CAT_2)]
filter_categorie_choices=c("all",stringr::str_sort(filter_categorie_choices,numeric = T))
names(filter_categorie_choices)=gsub("^IN_","",filter_categorie_choices)

# sankey_choices=c("","PR_SYMPTOM_EN","IN_SYMPTOM_EN",
#                  "PR_GAMME_EN_1","PR_GAMME_EN_2","PR_GAMME_EN_3","PR_GAMME_EN_4",
#                  "IN_GAMME_EN_1","IN_GAMME_EN_2","IN_GAMME_EN_3","IN_GAMME_EN_4",
#                  "IN_LOCATION_1","IN_LOCATION_2","IN_LOCATION_3","IN_LOCATION_4",
#                  "PR_LOCATION_1","PR_LOCATION_2","PR_LOCATION_3","PR_LOCATION_4",
#                  "IN_CAT_1","IN_CAT_2","IN_CAT_3","PR_CAT_2","PR_CAT_3")


