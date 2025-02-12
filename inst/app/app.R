shinyApp(
  ui = appUI,
  server = appServer,
  onStart = function() {
    shinyjs::useShinyjs()
    # shinyalert::useShinyalert(force = TRUE)
    }
)