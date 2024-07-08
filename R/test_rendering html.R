htmltools::save_html(HTML(readLines(
  rmarkdown::render(
    paste0(getOption("path_db_explorer"),"tools/help/help_filters.md"),
    encoding = "UTF-8",
    envir = new.env()
  )
)),"f1.html")

HTML(
  rmarkdown::render(
    paste0(getOption("path_db_explorer"),"tools/help/help_filters.md"),
    encoding = "UTF-8",
    envir = new.env()
  )
)