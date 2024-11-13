#' @rdname app

# ui : call this function once somewhere
shinyWidgets::useSweetAlert()

i18n <- shiny.i18n::Translator$new(translation_json_path = system.file("app", "translation.json", package = "resbiota"))
# i18n <- shiny.i18n::Translator$new(translation_json_path = "resbiota/inst/app/translation.json")
i18n$set_translation_language("en")
# i18n$set_translation_language("pt")


## Shiny UI ----
### Header ----
header <- shinydashboardPlus::dashboardHeader(
  title = htmltools::tagList(
    tags$span(
      class = "logo-mini", "res"
    ),
    tags$span(
      class = "logo-lg", "resbiota"
    )
  ),
  controlbarIcon = shiny::icon("gears")
)


### Sidebar ----
sidebar <- shinydashboardPlus::dashboardSidebar(
  collapsed = FALSE,
  # width = 150,
  shinydashboard::sidebarMenu(
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id = "tabs",
    shinydashboard::menuItem("Data input", tabName = "dataInputTab", icon = shiny::icon("database")),
    shinydashboard::menuItem("Simulate", tabName = "simulateTab", icon = shiny::icon("sliders-h")),
    shinydashboard::menuItem("Compute", tabName = "computeTab", icon = shiny::icon("calculator")),
    shinydashboard::menuItem("Select", tabName = "selectTab", icon = shiny::icon("filter")),
    # shinydashboard::menuItem("View", tabName = "viewTab", icon = shiny::icon("newspaper")),
    # shinydashboard::menuItem("Merge", tabName = "mergeTab", icon = shiny::icon("chart-simple")),
    # shinydashboard::menuItem("Merge", tabName = "mergeTab", icon = shiny::icon("code-merge"))
    # shinydashboard::menuItem("Traits data", tabName = "dataTab", icon = shiny::icon("th")),
    shinydashboard::menuItem("Export", tabName = "menuExport", icon = shiny::icon("download"))
  )
)

### Control bar ----
controlbar <- shinydashboardPlus::dashboardControlbar(
  id = "controlbar",
  shinydashboardPlus::controlbarMenu(
    id = "menu",
    shinydashboardPlus::controlbarItem(
      title = "Global options",
      shinyWidgets::prettyRadioButtons(inputId = "fileSep",
                                       label = "Separator character",
                                       choices = c(",", ";"),
                                       selected = ",",
                                       inline = TRUE,
                                       status = "primary"
      ),
      shiny::selectInput(inputId = "selectedLanguage",
                         label = i18n$t("Change language"),
                         choices = setNames(
                           i18n$get_languages(),
                           c("English", "Português") # Set labels for the languages
                         ),
                         selected = i18n$get_key_translation()
                         # selected = "pt"
      )
    )
  )
)

### Body ----
body <- dashboardBody(
  
  # Use shiny.i18n functions
  shiny.i18n::usei18n(i18n),
  
  # Global CSS tags
  tags$head(
    tags$style(shiny::HTML(
      "hr {border-top: 1px solid #000000;}"
    ))
  ),
  # tags$head(tags$style('#box .box-header{ display: none}')),
  # Use shinyjs functions
  shinyjs::useShinyjs(),
  shinydashboard::tabItems(
    #### dataInputTab ----
    # Data input tab
    shinydashboard::tabItem(tabName = "dataInputTab",
                            htmltools::h2("Data input"),
                            # hr(),
                            fluidRow(
                              shiny::column(width = 12,
                                            shiny::tabsetPanel(
                                              shiny::tabPanel("Load files", 
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 8,
                                                                              shiny::fileInput(inputId = "traitsInput",
                                                                                               label = "Traits",
                                                                                               accept = c(".csv"),
                                                                                               buttonLabel = "Browse..."
                                                                              ),
                                                                              shiny::fileInput(inputId = "restCompInput",
                                                                                               label = "Species composition of restoration sites",
                                                                                               accept = c(".csv"),
                                                                                               buttonLabel = "Browse..."
                                                                              ),
                                                                              shiny::fileInput(inputId = "restGroupInput",
                                                                                               label = "Complementary information for restoration sites",
                                                                                               accept = c(".csv"),
                                                                                               buttonLabel = "Browse..."
                                                                              ),
                                                                              shiny::fileInput(inputId = "referenceInput",
                                                                                               label = "Species composition of reference sites",
                                                                                               accept = c(".csv"),
                                                                                               buttonLabel = "Browse..."
                                                                              ),
                                                                              shiny::fileInput(inputId = "supplementaryInput",
                                                                                               label = "Species composition of supplementary sites",
                                                                                               accept = c(".csv"),
                                                                                               buttonLabel = "Browse..."
                                                                              )
                                                                ), # End column
                                                                shiny::column(width = 4, 
                                                                              htmltools::br(),
                                                                              shinyWidgets::actionBttn(inputId = "doClear", 
                                                                                                       label = "Clear all",
                                                                                                       style = "fill",
                                                                                                       size = "md",
                                                                                                       color = "danger")
                                                                              
                                                                ) # End column
                                                              ) # End row
                                              ), # End load files tab
                                              shiny::tabPanel("View traits", 
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 12,
                                                                              conditionalPanel(condition = "output.showTraitsData == true",
                                                                                               htmltools::h5(htmltools::strong("Traits class")),
                                                                                               rhandsontable::rHandsontableOutput("outputTableTraitsClass"),
                                                                                               htmltools::br(),
                                                                                               htmltools::h5(htmltools::strong("Traits data")),
                                                                                               rhandsontable::rHandsontableOutput("outputTableTraitsData")
                                                                              )
                                                                ) # End column
                                                              ) # End row
                                              ), # End view traits tab
                                              shiny::tabPanel("View restoration sites", 
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 12,
                                                                              conditionalPanel(condition = "output.showRestComp == true",
                                                                                               htmltools::h5(htmltools::strong("Species composition")),
                                                                                               rhandsontable::rHandsontableOutput("outputTableRestComp"),
                                                                                               htmltools::br()				 
                                                                              ),
                                                                              conditionalPanel(condition = "output.showRestGroup == true",
                                                                                               htmltools::h5(htmltools::strong("Complementary information")),
                                                                                               rhandsontable::rHandsontableOutput("outputTableRestGroup")				 
                                                                              )
                                                                ) # End column
                                                              ) # End row
                                              ), # End view restoration sites tab
                                              shiny::tabPanel("View reference sites", 
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 12,
                                                                              conditionalPanel(condition = "output.showReference == true",
                                                                                               htmltools::h5(htmltools::strong("Species composition of reference sites")),
                                                                                               rhandsontable::rHandsontableOutput("outputTableRefComp"),
                                                                                               htmltools::br()				 
                                                                              ),
                                                                              conditionalPanel(condition = "output.showSupplementary == true",
                                                                                               htmltools::h5(htmltools::strong("Species composition of supplementary sites")),
                                                                                               rhandsontable::rHandsontableOutput("outputTableSuppleComp")				 
                                                                              )
                                                                ) # End column
                                                              ) # End row
                                              ) # End view reference sites tab
                                            ) # End tabsetPanel
                              ) # End column
                            ) # End row
    ), # End dataInputTab
    #### simulateTab ----
    shinydashboard::tabItem(tabName = "simulateTab",
                            htmltools::h2("Simulated communities"),
                            # htmltools::hr(),
                            shiny::fluidRow(
                              shiny::column(width = 12,
                                            shiny::tabsetPanel(
                                              shiny::tabPanel("Simulate", 
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 8,
                                                                              htmltools::h4("Basic parameters"),
                                                                              htmltools::hr(),
                                                                              shiny::textInput(inputId = "prefixSimInput",
                                                                                               label = "Simulation name",
                                                                                               value = "Sim_1"
                                                                              ),
                                                                              shinyWidgets::prettyRadioButtons(inputId = "goalsSimInput",
                                                                                                               label = "Restoration goals",
                                                                                                               choices = setNames(
                                                                                                                 c("New", "Ongoing"),
                                                                                                                 c("New", "Ongoing") # Set labels
                                                                                                               ),
                                                                                                               selected = "New",
                                                                                                               inline = TRUE,
                                                                                                               status = "primary"
                                                                              ),
                                                                              shinyWidgets::prettyRadioButtons(inputId = "methodSimInput",
                                                                                                               label = "Method",
                                                                                                               choices = setNames(
                                                                                                                 c("Proportions", "Individuals"),
                                                                                                                 c("Proportions", "Individuals") # Set labels
                                                                                                               ),
                                                                                                               selected = "Proportions",
                                                                                                               inline = TRUE,
                                                                                                               status = "primary"
                                                                              ),
                                                                              shiny::conditionalPanel(condition = "(input.methodSimInput == 'Individuals')",
                                                                                                      shiny::numericInput(inputId = "nIndSimInput", 
                                                                                                                          label = "The number of individuals to draw",
                                                                                                                          value = NULL
                                                                                                      )
                                                                              ),
                                                                              shinyWidgets::sliderTextInput(inputId = "richSliderSimInput",
                                                                                                            label = "Range of richness",
                                                                                                            choices = c(1, 1),
                                                                                                            selected = c(1, 1)
                                                                              ),
                                                                              shiny::numericInput(inputId = "itSimInput", 
                                                                                                  label = "Number of iterations",
                                                                                                  value = 1000,
                                                                                                  min = 4),
                                                                              shinyWidgets::pickerInput(inputId = "avaSimInput",
                                                                                                        label = "Species availability",
                                                                                                        choices = NULL,
                                                                                                        multiple = TRUE,
                                                                                                        options = list("max-options" = 1),
                                                                                                        inline = FALSE
                                                                              ),
                                                                              shinyWidgets::pickerInput(inputId = "undSimInput",
                                                                                                        label = "Undesired species",
                                                                                                        choices = NULL,
                                                                                                        multiple = TRUE,
                                                                                                        options = list("max-options" = 1),
                                                                                                        inline = FALSE
                                                                              ),
                                                                              shinyWidgets::pickerInput(inputId = "cwmSimInput",
                                                                                                        label = "Traits to Community Weighted Mean",
                                                                                                        choices = NULL,
                                                                                                        multiple = TRUE,
                                                                                                        options = list(`actions-box` = TRUE),
                                                                                                        inline = FALSE
                                                                              ),
                                                                              shinyWidgets::pickerInput(inputId = "raoSimInput",
                                                                                                        label = "Traits to Rao Quadratic Entropy",
                                                                                                        choices = NULL,
                                                                                                        multiple = TRUE,
                                                                                                        options = list(`actions-box` = TRUE),
                                                                                                        inline = FALSE
                                                                              ),
                                                                              shinyWidgets::prettyRadioButtons(inputId = "speficyGroupsSimInput",
                                                                                                               label = "Specify probabilities for groups of species",
                                                                                                               choices = setNames(
                                                                                                                 c("Yes", "No"),
                                                                                                                 c("Yes", "No") # Set labels
                                                                                                               ),
                                                                                                               selected = "No",
                                                                                                               inline = TRUE,
                                                                                                               status = "primary"
                                                                              ),
                                                                              shiny::conditionalPanel(condition = "(input.speficyGroupsSimInput == 'Yes')",
                                                                                                      shinyWidgets::pickerInput(inputId = "groupSimInput",
                                                                                                                                label = "Species group",
                                                                                                                                choices = NULL,
                                                                                                                                multiple = TRUE,
                                                                                                                                options = list("max-options" = 1),
                                                                                                                                inline = FALSE
                                                                                                      ),
                                                                                                      shinyWidgets::prettyRadioButtons(inputId = "probGroupTypeSimInput",
                                                                                                                                       label = "Probabilities to draw species",
                                                                                                                                       choices = setNames(
                                                                                                                                         c("Abundance", "Richness and abundance"),
                                                                                                                                         c("Abundance", "Richness and abundance") # Set labels
                                                                                                                                       ),
                                                                                                                                       selected = "Abundance",
                                                                                                                                       inline = TRUE,
                                                                                                                                       status = "primary"
                                                                                                      ),
                                                                                                      htmltools::br(),
                                                                                                      shiny::conditionalPanel(condition = "(input.probGroupTypeSimInput == 'Richness and abundance')",
                                                                                                                              shiny::uiOutput("slidersProbRicSim"),
                                                                                                                              htmltools::br(),
                                                                                                                              htmltools::br()
                                                                                                      ),
                                                                                                      shiny::conditionalPanel(condition = "(input.probGroupTypeSimInput == 'Abundance' || input.probGroupTypeSimInput == 'Richness and abundance')",
                                                                                                                              shiny::uiOutput("slidersProbAbuSim"),
                                                                                                                              htmltools::br()
                                                                                                      )
                                                                              ),
                                                                              htmltools::br(),
                                                                              htmltools::br(),
                                                                              htmltools::h4("Advanced parameters"),
                                                                              htmltools::hr(),
                                                                              shiny::conditionalPanel(condition = "(input.methodSimInput == 'Individuals')",
                                                                                                      shinyWidgets::pickerInput(inputId = "probSimInput",
                                                                                                                                label = "Probabilities to draw individuals",
                                                                                                                                choices = NULL,
                                                                                                                                multiple = TRUE,
                                                                                                                                options = list("max-options" = 1),
                                                                                                                                inline = FALSE
                                                                                                      ),
                                                                                                      shiny::numericInput(inputId = "cvAbundSimInput", 
                                                                                                                          label = "Coefficient of variation of the relative abundances",
                                                                                                                          value = 1
                                                                                                      )
                                                                              ),
                                                                              shiny::sliderInput(inputId = "phiSimInput", 
                                                                                                 label = "Weights of either quadratic entropy or entropy",
                                                                                                 value = 1,
                                                                                                 min = 0,
                                                                                                 max = 1)
                                                                ), # End column
                                                                shiny::column(width = 4, 
                                                                              # fixedPanel(width = "100%",
                                                                              htmltools::h4("Run"),
                                                                              htmltools::hr(),
                                                                              shinyWidgets::actionBttn(inputId = "doSimulate",
                                                                                                       label = "Simulate",
                                                                                                       style = "fill",
                                                                                                       size = "lg",
                                                                                                       color = "success"),
                                                                              htmltools::br(),
                                                                              htmltools::br(),
                                                                              shiny::textOutput(outputId = "countScenariosText"),
                                                                              shiny::textOutput(outputId = "countSimulationText"),
                                                                              htmltools::br(),
                                                                              htmltools::br(),
                                                                              htmltools::h4("Merge"),
                                                                              htmltools::hr(),
                                                                              shiny::textInput(inputId = "mergeSimulateNameInput",
                                                                                               label = "Merged simulation name",
                                                                                               value = "Merged_1"),
                                                                              shinyWidgets::pickerInput(inputId = "mergeSimulateInput",
                                                                                                        label = "Choose to merge",
                                                                                                        choices = NULL,
                                                                                                        multiple = TRUE,
                                                                                                        inline = FALSE
                                                                              ),
                                                                              shinyWidgets::actionBttn(inputId = "doSimulateMerge", 
                                                                                                       label = "Merge",
                                                                                                       style = "fill",
                                                                                                       size = "md",
                                                                                                       color = "success"),
                                                                              htmltools::br(),
                                                                              htmltools::br(),
                                                                              htmltools::br(),
                                                                              htmltools::h4("Remove"),
                                                                              htmltools::hr(),
                                                                              shinyWidgets::pickerInput(inputId = "removeSimulateInput",
                                                                                                        label = "Choose to remove",
                                                                                                        choices = NULL,
                                                                                                        multiple = TRUE,
                                                                                                        inline = FALSE
                                                                              ),
                                                                              shinyWidgets::actionBttn(inputId = "doSimulateRemove", 
                                                                                                       label = "Remove",
                                                                                                       style = "fill",
                                                                                                       size = "md",
                                                                                                       color = "danger"
                                                                              )
                                                                              # ) # end fixedPanel
                                                                ) # End column
                                                              ) # End row
                                              ),
                                              shiny::tabPanel("View simulated communities", 
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 12,
                                                                              shinyWidgets::pickerInput(inputId = "scenarioViewInput",
                                                                                                        label = "Select scenario",
                                                                                                        choices = NULL,
                                                                                                        multiple = TRUE,
                                                                                                        options = list("max-options" = 1),
                                                                                                        inline = FALSE
                                                                              ),
                                                                              shiny::htmlOutput(outputId = "TextTESTE")
                                                                )
                                                              ) # End row
                                              ) # End tabPanel
                                            ) # End tabsetPanel
                              ) # End column
                            ) # End row
    ), # End simulateTab
    #### computeTab ----
    shinydashboard::tabItem(tabName = "computeTab",
                            h2("Compute parameters"),
                            # hr(),
                            fluidRow(
                              shiny::column(width = 12,
                                            shiny::tabsetPanel(
                                              shiny::tabPanel("Compute", 
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 8,
                                                                              htmltools::h4("Set basic parameters"),
                                                                              htmltools::hr(),
                                                                              # shiny::textInput(inputId = "prefixComInput",
                                                                              # 				 label = "Simulation name",
                                                                              # 				 value = "SimPar_1"
                                                                              # ),
                                                                              shinyWidgets::pickerInput(inputId = "scenarioComInput",
                                                                                                        label = "Choose scenario",
                                                                                                        choices = NULL,
                                                                                                        multiple = TRUE,
                                                                                                        options = list("max-options" = 1),
                                                                                                        inline = FALSE
                                                                              ),
                                                                              shinyWidgets::pickerInput(inputId = "avaComInput",
                                                                                                        label = "Species availability",
                                                                                                        choices = NULL,
                                                                                                        multiple = TRUE,
                                                                                                        options = list("max-options" = 1),
                                                                                                        inline = FALSE
                                                                              ),
                                                                              shinyWidgets::pickerInput(inputId = "cwmComInput",
                                                                                                        label = "Traits to Community Weighted Mean",
                                                                                                        choices = NULL,
                                                                                                        multiple = TRUE,
                                                                                                        options = list(`actions-box` = TRUE),
                                                                                                        inline = FALSE
                                                                              ),
                                                                              shinyWidgets::pickerInput(inputId = "cwvComInput",
                                                                                                        label = "Traits to Community Weighted Variance",
                                                                                                        choices = NULL,
                                                                                                        multiple = TRUE,
                                                                                                        options = list(`actions-box` = TRUE),
                                                                                                        inline = FALSE
                                                                              ),
                                                                              shinyWidgets::pickerInput(inputId = "raoComInput",
                                                                                                        label = "Traits to Rao Quadratic Entropy",
                                                                                                        choices = NULL,
                                                                                                        multiple = TRUE,
                                                                                                        options = list(`actions-box` = TRUE),
                                                                                                        inline = FALSE
                                                                              ),
                                                                              shinyWidgets::pickerInput(inputId = "costComInput",
                                                                                                        label = "Cost per individual",
                                                                                                        choices = NULL,
                                                                                                        multiple = TRUE,
                                                                                                        options = list("max-options" = 1),
                                                                                                        inline = FALSE
                                                                              ),
                                                                              shinyWidgets::pickerInput(inputId = "densComInput",
                                                                                                        label = "Species planting density",
                                                                                                        choices = NULL,
                                                                                                        multiple = TRUE,
                                                                                                        options = list("max-options" = 1),
                                                                                                        inline = FALSE
                                                                              ),
                                                                              htmltools::br(),
                                                                              htmltools::br(),
                                                                              htmltools::h4("Dissimilarity"),
                                                                              htmltools::hr(),
                                                                              
                                                                              htmltools::br(),
                                                                              htmltools::br(),
                                                                              htmltools::h4("Multifunctionality"),
                                                                              htmltools::hr(),
                                                                              
                                                                              
                                                                              
                                                                ), # End column
                                                                shiny::column(width = 4,
                                                                              htmltools::h4("Compute"),
                                                                              htmltools::hr(),
                                                                              shinyWidgets::actionBttn(inputId = "doCompute", 
                                                                                                       label = "Compute",
                                                                                                       style = "fill",
                                                                                                       size = "lg",
                                                                                                       color = "success")
                                                                ) # End column
                                                              ) # End row
                                              ),
                                              shiny::tabPanel("View simulated communities", 
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 12,
                                                                              shiny::sliderInput("bins3", label = p(i18n$t("Hello Shiny!"), actionButton("titleBtId", label = "", icon = icon("info"), style = 'padding:4px; font-size:60%')), min = 1, max = 50, value = 30),
                                                                              
                                                                              
                                                                              shiny::sliderInput("bins4", label = p(i18n$t("Hello Shiny!"), 
                                                                                                                    shinyBS::bsButton("surf-info",
                                                                                                                                      label = "",
                                                                                                                                      icon = shiny::icon("info"),
                                                                                                                                      style = "default",
                                                                                                                                      size = "extra-small")), 
                                                                                                 min = 1, max = 50, value = 30),
                                                                              # shinyBS::bsPopover(
                                                                              # 	id = "surf-info",
                                                                              # 	# title = "More information",
                                                                              # 	title = NULL,
                                                                              # 	# content = shiny::HTML(paste0(
                                                                              # 	# 	i18n$t("Ribeye steak, grilled jumbo shrimp, butter roasted potato medley grilled asparagus.")
                                                                              # 	# )),
                                                                              # 	# content = shiny::HTML("Test:"),
                                                                              # 	content = shiny::HTML(i18n$t("Test:")),
                                                                              # 	# content = as.character(i18n$t("Test:")),
                                                                              # 	# content = testRV$t1,
                                                                              # 	placement = "right",
                                                                              # 	trigger = "hover"
                                                                              # 	options = list(container = "body")
                                                                              # ),
                                                                              rhandsontable::rHandsontableOutput("traitsData3")	  
                                                                )
                                                                
                                                                # x, trait, stan = NULL, reference = NULL, supplementary = NULL
                                                                # shiny::sliderInput("bins1", label = i18n$t("Hello Shiny!"), min = 1, max = 50, value = 30),
                                                                # shiny::sliderInput("bins1", label = "xy", min = 1, max = 50, value = 30),
                                                                # shiny::sliderInput("bins2", label = i18n$t("Test:"), min = 1, max = 50, value = 30),
                                                                # shinydashboardPlus::box(id = "box", width = NULL, title = NULL, headerBorder = FALSE,
                                                                # 						shiny::sliderInput("bins2", label = i18n$t("Test:"), min = 1, max = 50, value = 30)
                                                                # ),
                                                                # shinydashboardPlus::box(id = "box22",
                                                                # 	# title = p("Title 1", actionButton("titleBtId", "", icon = icon("refresh"), title = "Update")),
                                                                # 	title = "xasxsa",
                                                                # 	shiny::sliderInput("bins12", label = i18n$t("Test:::"), min = 1, max = 50, value = 30),
                                                                # 	width = 4, solidHeader = TRUE #status = "warning"
                                                                # ),
                                                                # shinydashboardPlus::box(id = "box", width = NULL, title = NULL, headerBorder = FALSE,
                                                                # 						footer = i18n$t("Hello Shiny!"),
                                                                # 						shiny::sliderInput("bins2", label = i18n$t("Test:"), min = 1, max = 50, value = 30)
                                                                # ),
                                                                # shinydashboardPlus::box(id = "boxND", width = NULL, title = "xx", headerBorder = FALSE,
                                                                # 						shiny::sliderInput("bins4", label = "Test:", min = 1, max = 50, value = 30),
                                                                # 						sidebar = boxSidebar(
                                                                # 							icon = shiny::icon("info"),
                                                                # 							startOpen = FALSE,
                                                                # 							id = "mycardsidebar",
                                                                # 						)),
                                                                # FUNCIONA
                                                                # shinydashboardPlus::box(id = "box2", width = NULL, title = p(i18n$t("Hello Shiny!"), actionButton("titleBtId", "", icon = icon("refresh"), title = "Update")), headerBorder = FALSE,
                                                                # 						footer = i18n$t("Hello Shiny!!"),
                                                                # 						shinyWidgets::pickerInput(
                                                                # 							inputId = "avaComputeInput2",
                                                                # 							label = "Species availability",
                                                                # 							choices = "---None---",
                                                                # 							inline = FALSE
                                                                # 						)
                                                                # ),
                                                                # actionButton("Button", icon = icon("house-user") , label = i18n$t("This is a button")),
                                                                # actionButton("Button", icon = icon("house-user") , label = i18n$t("Test:")),
                                                                # selectInput(inputId = "Select2",
                                                                # 			# label = "xx",
                                                                # 			label = shiny::HTML("This is the title of the selectInput", #"<font size='2'>",
                                                                # 								as.character(shinyBS::bsButton("surf-info",
                                                                # 															   label = "",
                                                                # 															   icon = shiny::icon("info"),
                                                                # 															   style = "default",
                                                                # 															   size = "extra-small")),
                                                                # 								"</font>"),
                                                                # 			choices = 1:3),
                                                                # shinyBS::bsPopover(
                                                                # 	id = "surf-info",
                                                                # 	# title = "More information",
                                                                # 	title = NULL,
                                                                # 	# content = shiny::HTML(paste0(
                                                                # 	# 	i18n$t("Ribeye steak, grilled jumbo shrimp, butter roasted potato medley grilled asparagus.")
                                                                # 	# )),
                                                                # 	content = shiny::HTML("Test:"),
                                                                # 	placement = "right",
                                                                # 	trigger = "hover",
                                                                # 	options = list(container = "body")
                                                                # ),
                                                                # shinyBS::bsPopover(
                                                                # 	id = "bins1",
                                                                # 	# title = "More information",
                                                                # 	title = NULL,
                                                                # 	# content = i18n$t("Hello Shiny!"),
                                                                # 	content = "xx",
                                                                # 	placement = "right",
                                                                # 	trigger = "hover",
                                                                # 	options = list(container = "body")
                                                                # ),
                                                                
                                                              ) # End row
                                              ), # End tabPanel
                                              shiny::tabPanel("View simulated communities", 
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 12,
                                                                              rhandsontable::rHandsontableOutput("traitsData3")	  
                                                                )
                                                              ) # End row
                                              )
                                            )
                              )
                            )
    ), # End computeTab
    #### selectTab ----
    shinydashboard::tabItem(tabName = "selectTab",
                            h2("Select communities"),
                            fluidRow(
                              shiny::column(width = 12,
                                            shiny::tabsetPanel(
                                              shiny::tabPanel("Select", 
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 8,
                                                                              htmltools::h4("Set basic parameters"),
                                                                              hr(),
                                                                              shinyWidgets::pickerInput(inputId = "scenarioSelInput",
                                                                                                        label = "Choose scenario",
                                                                                                        choices = NULL,
                                                                                                        multiple = TRUE,
                                                                                                        options = list("max-options" = 1),
                                                                                                        inline = FALSE
                                                                              ),
                                                                              shiny::textInput(inputId = "prefixSelInput",
                                                                                               label = "Selection name",
                                                                                               value = "Sel_1"
                                                                              ),
                                                                              shinyWidgets::pickerInput(inputId = "testsDetSelInput",
                                                                                                        label = "Parameters to deterministic selection",
                                                                                                        choices = NULL,
                                                                                                        multiple = TRUE,
                                                                                                        options = list(`actions-box` = TRUE),
                                                                                                        inline = FALSE
                                                                              ),
                                                                              shiny::conditionalPanel(condition = "output.showSlidersTestsDetSel == true",
                                                                                                      shiny::uiOutput("slidersTestsDetSel"),
                                                                                                      htmltools::br(),
                                                                                                      htmltools::br()
                                                                              ),
                                                                              shinyWidgets::pickerInput(inputId = "testsHieSelInput",
                                                                                                        label = "Parameters to hierarchical selection",
                                                                                                        choices = NULL,
                                                                                                        multiple = TRUE,
                                                                                                        options = list(`actions-box` = TRUE),
                                                                                                        inline = FALSE
                                                                              ),
                                                                              shiny::uiOutput(outputId = "outputRankList"),
                                                                              verbatimTextOutput("results_basic"),
                                                                              verbatimTextOutput("results_basic2"),
                                                                              shiny::conditionalPanel(condition = "output.showSlidersTestsHieSel == true",
                                                                                                      shiny::uiOutput("slidersTestsHieSel"),
                                                                                                      htmltools::br(),
                                                                              ),
                                                                              htmltools::br(),
                                                                              shinyWidgets::prettyRadioButtons(inputId = "speficyGroupsSelInput",
                                                                                                               label = "Selection inside sites groups",
                                                                                                               choices = setNames(
                                                                                                                 c("Yes", "No"),
                                                                                                                 c("Yes", "No") # Set labels
                                                                                                               ),
                                                                                                               selected = "No",
                                                                                                               inline = TRUE,
                                                                                                               status = "primary"
                                                                              ),
                                                                              shiny::conditionalPanel(condition = "(input.speficyGroupsSelInput == 'Yes')",
                                                                                                      shinyWidgets::pickerInput(inputId = "groupSelInput",
                                                                                                                                label = "Species groups",
                                                                                                                                choices = NULL,
                                                                                                                                multiple = TRUE,
                                                                                                                                options = list("max-options" = 1),
                                                                                                                                inline = FALSE
                                                                                                      )
                                                                              ),
                                                                              shinyWidgets::prettyRadioButtons(inputId = "singleSelectionInput",
                                                                                                               label = "Selection method",
                                                                                                               choices = setNames(
                                                                                                                 c(TRUE, FALSE),
                                                                                                                 c("Single", "Multiple") # Set labels
                                                                                                               ),
                                                                                                               selected = TRUE,
                                                                                                               inline = TRUE,
                                                                                                               status = "primary"
                                                                              ),
                                                                ),
                                                                shiny::column(width = 4,
                                                                              htmltools::h4("Select"),
                                                                              hr(),
                                                                              shinyWidgets::actionBttn(inputId = "doSelect", 
                                                                                                       label = "Select",
                                                                                                       style = "fill",
                                                                                                       size = "lg",
                                                                                                       color = "success"),htmltools::br(),
                                                                              htmltools::br(),
                                                                              htmltools::h4("Merge"),
                                                                              htmltools::hr(),
                                                                              shiny::textInput(inputId = "mergeSelectNameInput",
                                                                                               label = "Merged selection name",
                                                                                               value = "Merged_1"),
                                                                              shinyWidgets::pickerInput(inputId = "mergeSelectInput",
                                                                                                        label = "Choose to merge",
                                                                                                        choices = NULL,
                                                                                                        multiple = TRUE,
                                                                                                        inline = FALSE
                                                                              ),
                                                                              shinyWidgets::actionBttn(inputId = "doSelectMerge", 
                                                                                                       label = "Merge",
                                                                                                       style = "fill",
                                                                                                       size = "md",
                                                                                                       color = "success"),
                                                                              htmltools::br(),
                                                                              htmltools::br(),
                                                                              htmltools::br(),
                                                                              htmltools::h4("Remove"),
                                                                              htmltools::hr(),
                                                                              shinyWidgets::pickerInput(inputId = "removeSelectInput",
                                                                                                        label = "Choose to remove",
                                                                                                        choices = NULL,
                                                                                                        multiple = TRUE,
                                                                                                        inline = FALSE
                                                                              ),
                                                                              shinyWidgets::actionBttn(inputId = "doSelectRemove", 
                                                                                                       label = "Remove",
                                                                                                       style = "fill",
                                                                                                       size = "md",
                                                                                                       color = "danger"
                                                                              )
                                                                ) # End column
                                                              ) # End row
                                              ) # End tabPanel
                                            ) # End tabsetPanel
                              ) # End column
                            ) # End row
    ) # End selectTab
  ) # End tabItems
)


## Page UI ----
#' @export
appUI <- shinydashboardPlus::dashboardPage(header = header,
                                           sidebar = sidebar,
                                           body = body,
                                           controlbar = controlbar,
                                           skin = "green",
                                           md = FALSE,
                                           scrollToTop = TRUE
)
