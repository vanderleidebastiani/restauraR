#' @import shiny
#' @import shinydashboard
#' @import shinydashboardPlus
#' @export
appServer <- shiny::shinyServer(function(input, output, session) {})
  
#' @export
appUI <- shinydashboardPlus::dashboardPage(header = shinydashboardPlus::dashboardHeader(),
                                           sidebar = shinydashboardPlus::dashboardSidebar(),
                                           body = shinydashboard::dashboardBody(),
                                           skin = "black",
                                           md = FALSE,
                                           scrollToTop = TRUE
)
# shinyApp(
#   ui = appUI,
#   server = appServer
# )