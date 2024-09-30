fixSmartFilter <- function(text, all = FALSE) {
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

preProcessFilter <- function(filer_cmd){
  filer_cmd %>%
    gsub("\\n", "", .) %>%
    gsub("'", "\\\\'", .) %>%
    gsub("\"", "\'", .) %>%
    fixSmartFilter() %>%
    stringr::str_trim("both")
  
}

decomposeFilter <- function(raw_filter_text){
  
  splitted_filter <- stringr::str_split(raw_filter_text,"&",simplify = T)
  splitted_filter <- stringr::str_trim(splitted_filter)
  splitted_filter <- stringr::str_squish(splitted_filter)
                                       
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

