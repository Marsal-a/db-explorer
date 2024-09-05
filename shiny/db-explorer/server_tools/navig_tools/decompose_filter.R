decompose_filter <- function(raw_filter_text){
  
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