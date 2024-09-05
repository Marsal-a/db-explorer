nz_con <- connectNzSDSE()
library(dplyr)
library(dbplyr)

fix_smart <- function(text, all = FALSE) {
  if (all) {
    ## to remove all non-ascii symbols use ...
    text <- stringi::stri_trans_general(text, "latin-ascii")
  } else {
    ## based on https://stackoverflow.com/a/1262210/1974918
    ## based on https://stackoverflow.com/a/54467895/1974918
    text <- gsub("\u2022", "*", text) %>%
      gsub("\u2026", "...", .) %>%
      gsub("\u2013", "-", .) %>%
      gsub("\u2019", "'", .) %>%
      gsub("\u2018", "'", .) %>%
      gsub("\u201D", '"', .) %>%
      gsub("\u201C", '"', .)
  }
  gsub("\r\n", "\n", text) %>%
    gsub("\r", "\n", .) %>%
    gsub("\f", "\n", .)
}

fix_filter <- function(filt){
  cmd <- filt %>%
    gsub("\\n", "", .) %>%
    gsub("'", "\\\\'", .) %>%
    gsub("\"", "\'", .) %>%
    fix_smart() %>% stringr::str_trim()
  return(cmd)
}




df<-tbl(nz_con,in_schema("DTL_ADAM_MARSAL","DATA_AMENAGE"))

condition_filtre_1 <- "PER_ID==\"95891955\""
condition_filtre_1_fix <- fix_filter(condition_filtre_1)

df_filtered <- df %>% filter(!!rlang::parse_expr(condition_filtre_1))
df_filtered <- df %>% filter(!!rlang::parse_expr(condition_filtre_1_fix))
df_filtered %>% show_query()


condition_filtre_2="PER_ID==\"95891955\" &\n  DATE_EVT==\"2021-02-15\" &\n  DUREE_FERME==\"120\" &\n  DUREE_SURSIS==\"240\""
list_filter<-fix_filter(stringr::str_split(condition_filtre_2,"&",simplify = T))
filtered_df <- purrr::reduce(list_filter, ~ dplyr::filter(.x, !!rlang::parse_expr(.y)), .init = df)





decompose_filter <- function(raw_filter_text){
  
  splitted_filter <- fix_filter(stringr::str_split(raw_filter_text,"&",simplify = T))
  
  decompose_regex <- "^([a-zA-Z_][a-zA-Z0-9_]*)\\s*(==|!=|<|>|<=|>=|%in%|%like%|%ilike%|&|\\||!)\\s*['\"]([^'\"]*)['\"]$"
  decomposed_filter <- stringr::str_match(splitted_filter,decompose_regex)
  filter_components <- lapply(1:nrow(decomposed_filter), function(i) {
    list(
      column = decomposed_filter[i, 2],
      operator = decomposed_filter[i, 3],
      value = decomposed_filter[i, 4]
    )
  })
  return(filter_components)
  
}

f1="PER_ID==\"95891955\" &\n  DATE_EVT >= \"2021-02-15\" &\n  DUREE_FERME%in%\"120\" &\n  DUREE_SURSIS %ian% \"240\""
decompose_filter(f1)



# Exemple de filtres
filters <- c("PER_ID=='95891955'", 
             "DATE_EVT == '2021-02-15'", 
             "DUREE_FERME == '120'", 
             "DUREE_SURSIS == '240'", 
             "COLUMN >= '100'", 
             "ANOTHER_COLUMN %in% 'value'", 
             "YET_ANOTHER_COLUMN %like% 'pattern'")



## TEST 1
matches <- regmatches(filters, regexec(regex, filters))
for (match in matches) {
  # print(match)
  cat("Colonne:", match[[2]], "\n")
  cat("Opérateur:", match[[3]], "\n")
  cat("Valeur:", match[[4]], "\n\n")
}

## TEST 2
extract_filter_parts <- function(filter) {
  match <- regexpr(regex, filter, perl = TRUE)
  if (match == -1) {
    return(NULL)
  }
  parts <- regmatches(filter, match)
  parts <- unlist(strsplit(parts, " "))
  list(column = parts[1], operator = parts[2], value = parts[3])
}
filter_parts <- lapply(filters, extract_filter_parts)
print(filter_parts)




regexec(regex, filters[1])

filter=filters[1]
regmatches(filter, regexpr(regex, filter))
regmatches(filter, regexec(regex, filter))

stringr::str_match(filter,regex)


ora_con <- connectOraSDSE()
f="INDIC_COLL==110100 & DASAI==sql(\"TO_DATE('2012-06-21', 'YYYY-MM-DD')\")"
f="INDIC_COLL==110100 & DASAI==sql(\"TO_DATE('2012-06-21', 'YYYY-MM-DD')\")"


tb <- tbl(ora_con,in_schema("TGI","AFFAIRES"))
tb %>% filter(!!rlang::parse_expr(f))
