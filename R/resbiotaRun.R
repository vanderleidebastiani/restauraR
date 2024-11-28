#' @title Launch the resbiota Shiny Application
#' @description Launches a local version of the resbiota application. 
#' @encoding UTF-8
#' @importFrom shiny runApp
#' @param launchBrowser Shiny application launch mode. See runApp function (default launchBrowser = TRUE).
#' @param ... Arguments used in the runApp function.
#' @export
resbiotaRun <- function(launchBrowser = TRUE, ...){
  appDir <- system.file("app", package = "resbiota")
  shiny::runApp(appDir = appDir, launch.browser = launchBrowser, ...)  
}