myGUI <- function() {
  print(getwd())
  appDir <- "explorations/FiF/"
  appDir <- "../../explorations/FiF/"
  shiny::runApp(appDir = appDir, display.mode = "normal", quiet = TRUE, host = "127.0.0.1", port = 3838)
}
myGUI()

