#' @encoding UTF-8
#' @export
resbiotaRun <- function(launch.browser = T){
  appDir <- system.file("app", package = "resbiota")
  shiny::runApp(appDir, launch.browser = launch.browser)  
}