#' @rdname app
#' 
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
  # dropdownBlock(
  # 	id = "dropdown",
  # 	title = "",
  # 	icon = icon("globe"),
  # 	badgeStatus = NULL,
  # 	shiny::selectInput(inputId = "selectedLanguage",
  # 					   label = i18n$t("Change language"),
  # 					   choices = setNames(
  # 					   	i18n$get_languages(),
  # 					   	c("English", "Português") # Set labels for the languages
  # 					   ),
  # 					   selected = i18n$get_key_translation()
  # 	)
  # )
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
    shinydashboard::menuItem("View", tabName = "viewTab", icon = shiny::icon("newspaper")),
    # shinydashboard::menuItem("Merge", tabName = "mergeTab", icon = shiny::icon("chart-simple")),
    # shinydashboard::menuItem("Merge", tabName = "mergeTab", icon = shiny::icon("code-merge"))
    # shinydashboard::menuItem("Traits data", tabName = "dataTab", icon = shiny::icon("th")),
    shinydashboard::menuItem("Export", tabName = "exportTab", icon = shiny::icon("download"))
  )
)

### Control bar ----
controlbar <- shinydashboardPlus::dashboardControlbar(
  id = "controlbar",
  shinydashboardPlus::controlbarMenu(
    id = "menu",
    shinydashboardPlus::controlbarItem(
      title = "Global options",
      shiny::textInput("projectName", label = "Project name", value = "Projet"),
      shinyWidgets::prettyRadioButtons(inputId = "fileSep",
                                       label = "Separator character",
                                       choices = c(",", ";"),
                                       selected = ",",
                                       inline = TRUE,
                                       status = "primary"
      ),
      uiOutput("decimalPlaces"),
      # shiny::numericInput(inputId = "decimalPlaces",
      # 					label = "Decimal places",
      # 					value = 5,
      # 					min = 3,
      # 					step = 1),
      # radioGroupButtons(inputId = "selectedLanguage",
      # 					 label = i18n$t("Change language"),
      # 					 choices = setNames(
      # 					 	i18n$get_languages(),
      # 					 	c("English", "Português") # Set labels for the languages
      # 					 ),
      # 					 selected = i18n$get_key_translation(),
      # 					 justified = TRUE
      # )
      shiny::selectInput(inputId = "selectedLanguage",
                         label = i18n$t("Change language"),
                         choices = setNames(
                           i18n$get_languages(),
                           c("English", "Português") # Set labels for the languages
                         ),
                         selected = i18n$get_key_translation()
      )
    ),
    shinydashboardPlus::controlbarItem(
      title = "Plot options",
      shiny::numericInput(inputId = "saveWidth", label = "Width (mm)", value = 160),
      shiny::numericInput(inputId = "saveHeight", label = "Height (mm)", value = 120),
      shiny::numericInput(inputId = "saveDPI", label = "DPI", value = 300)
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
                                                                              # htmltools::h4("Load files"),
                                                                              # htmltools::hr(),
                                                                              splitLayout(cellWidths = c("80%", "20%"),
                                                                                          shiny::fileInput(inputId = "traitsInput",
                                                                                                           label = "Traits",
                                                                                                           accept = c(".csv"),
                                                                                                           buttonLabel = "Browse..."
                                                                                          ),
                                                                                          div(
                                                                                            shinyWidgets::actionBttn(inputId = "doClearTraits", 
                                                                                                                     label = "Clear",
                                                                                                                     style = "fill",
                                                                                                                     size = "sm",
                                                                                                                     color = "default"),
                                                                                            style = 'margin-top:25px'
                                                                                          )
                                                                              ),
                                                                              splitLayout(cellWidths = c("80%", "20%"),
                                                                                          shiny::fileInput(inputId = "restCompInput",
                                                                                                           label = "Species composition of restoration sites",
                                                                                                           accept = c(".csv"),
                                                                                                           buttonLabel = "Browse..."
                                                                                          ),
                                                                                          div(
                                                                                            shinyWidgets::actionBttn(inputId = "doClearRestComp", 
                                                                                                                     label = "Clear",
                                                                                                                     style = "fill",
                                                                                                                     size = "sm",
                                                                                                                     color = "default"),
                                                                                            style = 'margin-top:25px'
                                                                                          )
                                                                              ),
                                                                              splitLayout(cellWidths = c("80%", "20%"),
                                                                                          shiny::fileInput(inputId = "restGroupInput",
                                                                                                           label = "Complementary information for restoration sites",
                                                                                                           accept = c(".csv"),
                                                                                                           buttonLabel = "Browse..."
                                                                                          ),
                                                                                          div(
                                                                                            shinyWidgets::actionBttn(inputId = "doClearRestGroup", 
                                                                                                                     label = "Clear",
                                                                                                                     style = "fill",
                                                                                                                     size = "sm",
                                                                                                                     color = "default"),
                                                                                            style = 'margin-top:25px'
                                                                                          )
                                                                              ),
                                                                              splitLayout(cellWidths = c("80%", "20%"),
                                                                                          shiny::fileInput(inputId = "referenceInput",
                                                                                                           label = "Species composition of reference sites",
                                                                                                           accept = c(".csv"),
                                                                                                           buttonLabel = "Browse..."
                                                                                          ),
                                                                                          div(
                                                                                            shinyWidgets::actionBttn(inputId = "doClearReference", 
                                                                                                                     label = "Clear",
                                                                                                                     style = "fill",
                                                                                                                     size = "sm",
                                                                                                                     color = "default"),
                                                                                            style = 'margin-top:25px'
                                                                                          )
                                                                              ),
                                                                              splitLayout(cellWidths = c("80%", "20%"),
                                                                                          shiny::fileInput(inputId = "supplementaryInput",
                                                                                                           label = "Species composition of supplementary sites",
                                                                                                           accept = c(".csv"),
                                                                                                           buttonLabel = "Browse..."
                                                                                          ),
                                                                                          div(
                                                                                            shinyWidgets::actionBttn(inputId = "doClearSupplementary", 
                                                                                                                     label = "Clear",
                                                                                                                     style = "fill",
                                                                                                                     size = "sm",
                                                                                                                     color = "default"),
                                                                                            style = 'margin-top:25px'
                                                                                          )
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
                                                                              # htmltools::h4("Basic parameters"),
                                                                              # htmltools::hr(),
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
                                                                              # htmltools::br(),
                                                                              # htmltools::br(),
                                                                              # htmltools::h4("Advanced parameters"),
                                                                              # htmltools::hr(),
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
                                                                              # htmltools::h4("Run"),
                                                                              # htmltools::hr(),
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
                                                                                               value = "Sim_Merged_1"),
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
                                              shiny::tabPanel("Simulation Summary", 
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 12,
                                                                              shinyWidgets::pickerInput(inputId = "scenarioSimulateSummaryInput",
                                                                                                        label = "Select scenario",
                                                                                                        choices = NULL,
                                                                                                        multiple = TRUE,
                                                                                                        options = list("max-options" = 1),
                                                                                                        inline = FALSE
                                                                              ),
                                                                              shiny::htmlOutput(outputId = "outputSimulateSummaryText")
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
                                              shiny::tabPanel("Basic parameters", 
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 8,
                                                                              # htmltools::h4("Set basic parameters"),
                                                                              # htmltools::hr(),
                                                                              # shiny::textInput(inputId = "prefixComInput",
                                                                              # 				 label = "Simulation name",
                                                                              # 				 value = "SimPar_1"
                                                                              # ),
                                                                              shinyWidgets::pickerInput(inputId = "scenarioComParInput",
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
                                                                              shinyWidgets::pickerInput(inputId = "disComInput",
                                                                                                        label = "Traits to dissimilarity between reference sites",
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
                                                                              )
                                                                ), # End column
                                                                shiny::column(width = 4,
                                                                              # htmltools::h4("Compute"),
                                                                              # htmltools::hr(),
                                                                              shinyWidgets::actionBttn(inputId = "doCompute", 
                                                                                                       label = "Compute",
                                                                                                       style = "fill",
                                                                                                       size = "lg",
                                                                                                       color = "success")
                                                                ) # End column
                                                              ) # End row
                                              ),
                                              shiny::tabPanel("Standardize parameters", 
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 8,
                                                                              shinyWidgets::pickerInput(inputId = "scenarioComStandParInput",
                                                                                                        label = "Choose scenario",
                                                                                                        choices = NULL,
                                                                                                        multiple = TRUE,
                                                                                                        options = list("max-options" = 1),
                                                                                                        inline = FALSE
                                                                              ),
                                                                              shinyWidgets::pickerInput(inputId = "stanComParInput",
                                                                                                        label = "Parameters to standardized",
                                                                                                        choices = NULL,
                                                                                                        multiple = TRUE,
                                                                                                        options = list(`actions-box` = TRUE),
                                                                                                        inline = FALSE
                                                                              ),
                                                                              shinyWidgets::prettyRadioButtons(inputId = "speficyMethodStanInput",
                                                                                                               label = "Standardization method",
                                                                                                               choices = setNames(
                                                                                                                 c("max", "standardize"),
                                                                                                                 c("Maximum", "Standardize") # Set labels
                                                                                                               ),
                                                                                                               selected = "max",
                                                                                                               inline = TRUE,
                                                                                                               status = "primary"
                                                                              )
                                                                ),
                                                                shiny::column(width = 4,
                                                                              shinyWidgets::actionBttn(inputId = "doStandardize", 
                                                                                                       label = "Standardize",
                                                                                                       style = "fill",
                                                                                                       size = "lg",
                                                                                                       color = "success")
                                                                )
                                                              ) # End row
                                              ), # End tabPanel
                                              shiny::tabPanel("Multifunctionality", 
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 8,
                                                                              # htmltools::h4("Set basic parameters"),
                                                                              # htmltools::hr(),
                                                                              shinyWidgets::pickerInput(inputId = "scenarioComMultiInput",
                                                                                                        label = "Choose scenario",
                                                                                                        choices = NULL,
                                                                                                        multiple = TRUE,
                                                                                                        options = list("max-options" = 1),
                                                                                                        inline = FALSE
                                                                              ),
                                                                              shinyWidgets::pickerInput(inputId = "testsMultiInput",
                                                                                                        label = "Parameters to multifunctionality",
                                                                                                        choices = NULL,
                                                                                                        multiple = TRUE,
                                                                                                        options = list(`actions-box` = TRUE),
                                                                                                        inline = FALSE
                                                                              ),
                                                                              # shiny::uiOutput(outputId = "outputMultiList"),
                                                                              shiny::conditionalPanel(condition = "output.showSlidersMulti == true",
                                                                                                      shiny::uiOutput("slidersMulti"),
                                                                                                      htmltools::br(),
                                                                              ),
                                                                ), # End column
                                                                shiny::column(width = 4,
                                                                              # htmltools::h4("Compute"),
                                                                              # htmltools::hr(),
                                                                              shinyWidgets::actionBttn(inputId = "doMultiCompute", 
                                                                                                       label = "Compute",
                                                                                                       style = "fill",
                                                                                                       size = "lg",
                                                                                                       color = "success")
                                                                ) # End column
                                                              ) # End row
                                              ),
                                              shiny::tabPanel("Simulation Summary", 
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 12,
                                                                              shinyWidgets::pickerInput(inputId = "scenarioComputeSummaryInput",
                                                                                                        label = "Select scenario",
                                                                                                        choices = NULL,
                                                                                                        multiple = TRUE,
                                                                                                        options = list("max-options" = 1),
                                                                                                        inline = FALSE
                                                                              ),
                                                                              shiny::htmlOutput(outputId = "outputComputeSummaryText")
                                                                )
                                                              ) # End row
                                              ) # End tabPanel
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
                                                                              # htmltools::h4("Set basic parameters"),
                                                                              # hr(),
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
                                                                              # htmltools::h4("Select"),
                                                                              # hr(),
                                                                              shinyWidgets::actionBttn(inputId = "doSelect", 
                                                                                                       label = "Select",
                                                                                                       style = "fill",
                                                                                                       size = "lg",
                                                                                                       color = "success"),
                                                                              htmltools::br(),
                                                                              htmltools::br(),
                                                                              shiny::textOutput(outputId = "countSelectText"),
                                                                              shiny::textOutput(outputId = "countSimulationSelText"),
                                                                              htmltools::br(),
                                                                              htmltools::br(),
                                                                              htmltools::h4("Merge"),
                                                                              htmltools::hr(),
                                                                              shiny::textInput(inputId = "mergeSelectNameInput",
                                                                                               label = "Merged selection name",
                                                                                               value = "Sel_Merged_1"),
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
                                              ), # End tabPanel
                                              shiny::tabPanel("Select Summary", 
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 12,
                                                                              shinyWidgets::pickerInput(inputId = "scenarioSelectSummaryInput",
                                                                                                        label = "Select scenario",
                                                                                                        choices = NULL,
                                                                                                        multiple = TRUE,
                                                                                                        options = list("max-options" = 1),
                                                                                                        inline = FALSE
                                                                              ),
                                                                              shiny::htmlOutput(outputId = "outputSelectSummaryText")
                                                                )
                                                              ) # End row
                                              ), # End tabPanel
                                            ) # End tabsetPanel
                              ) # End column
                            ) # End row
    ), # End selectTab
    #### viewTab ----
    shinydashboard::tabItem(tabName = "viewTab",
                            h2("View results"),
                            fluidRow(
                              shiny::column(width = 12,
                                            shiny::tabsetPanel(
                                              shiny::tabPanel("Parameters", 
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 4,
                                                                              shinyWidgets::pickerInput(inputId = "scenarioViewParInput",
                                                                                                        label = "Choose scenario",
                                                                                                        choices = NULL,
                                                                                                        multiple = TRUE,
                                                                                                        options = list("max-options" = 1),
                                                                                                        inline = FALSE
                                                                              ),
                                                                              shinyWidgets::pickerInput(inputId = "xvarViewInput",
                                                                                                        label = "Variable to x-axis",
                                                                                                        choices = NULL,
                                                                                                        multiple = TRUE,
                                                                                                        options = list("max-options" = 1),
                                                                                                        inline = FALSE
                                                                              ),
                                                                              shinyWidgets::pickerInput(inputId = "yvarViewInput",
                                                                                                        label = "Variable to y-axis",
                                                                                                        choices = NULL,
                                                                                                        multiple = TRUE,
                                                                                                        options = list("max-options" = 1),
                                                                                                        inline = FALSE
                                                                              ),
                                                                              shinyWidgets::prettyRadioButtons(inputId = "hideRefViewParInput",
                                                                                                               label = "Hide reference sites",
                                                                                                               choices = setNames(
                                                                                                                 c(TRUE, FALSE),
                                                                                                                 c("Yes", "No") # Set labels
                                                                                                               ),
                                                                                                               selected = FALSE,
                                                                                                               inline = TRUE,
                                                                                                               status = "primary"
                                                                              ),
                                                                              # hr(),
                                                                              shiny::textInput(inputId = "xvarLab", 
                                                                                               label = "Label to x-axis", 
                                                                                               value = ""),
                                                                              shiny::textInput(inputId = "yvarLab", 
                                                                                               label = "Label to y-axis", 
                                                                                               value = ""),
                                                                              shinyWidgets::actionBttn(inputId = "doPlotPar", 
                                                                                                       label = "Plot",
                                                                                                       style = "fill",
                                                                                                       size = "md",
                                                                                                       color = "success"),
                                                                              shinyWidgets::actionBttn(inputId = "doPlotParClear", 
                                                                                                       label = "Clear",
                                                                                                       style = "fill",
                                                                                                       size = "md",
                                                                                                       color = "default")
                                                                ), # End column
                                                                shiny::column(width = 8,
                                                                              shiny::plotOutput("plotParOutput"),
                                                                              br(),
                                                                              downloadBttn(outputId = "doDownloadParPlot",
                                                                                           label = "Download",
                                                                                           style = "fill",
                                                                                           icon = NULL,
                                                                                           size = "md",
                                                                                           color = "success")
                                                                ) # End column
                                                              ) # End row
                                              ), # End tabPanel
                                              shiny::tabPanel("Multifunctionality", 
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 4,
                                                                              shinyWidgets::pickerInput(inputId = "scenarioViewMultiInput",
                                                                                                        label = "Choose scenario",
                                                                                                        choices = NULL,
                                                                                                        multiple = TRUE,
                                                                                                        options = list("max-options" = 1),
                                                                                                        inline = FALSE
                                                                              ),
                                                                              shinyWidgets::prettyRadioButtons(inputId = "hideRefViewMultiInput",
                                                                                                               label = "Hide reference sites",
                                                                                                               choices = setNames(
                                                                                                                 c(TRUE, FALSE),
                                                                                                                 c("Yes", "No") # Set labels
                                                                                                               ),
                                                                                                               selected = FALSE,
                                                                                                               inline = TRUE,
                                                                                                               status = "primary"
                                                                              ),
                                                                              shinyWidgets::actionBttn(inputId = "doPlotMulti", 
                                                                                                       label = "Plot",
                                                                                                       style = "fill",
                                                                                                       size = "md",
                                                                                                       color = "success"),
                                                                              shinyWidgets::actionBttn(inputId = "doPlotMultiClear", 
                                                                                                       label = "Clear",
                                                                                                       style = "fill",
                                                                                                       size = "md",
                                                                                                       color = "default")
                                                                ), # End column
                                                                shiny::column(width = 8,
                                                                              shiny::plotOutput("plotMultiOutput"),
                                                                              br(),
                                                                              downloadBttn(outputId = "doDownloadMultiPlot",
                                                                                           label = "Download",
                                                                                           style = "fill",
                                                                                           icon = NULL,
                                                                                           size = "md",
                                                                                           color = "success")
                                                                ) # End column
                                                              ) # End row
                                              ) # End tabPanel
                                            ) # End tabsetPanel
                              ) # End column
                            ) # End row
    ), # End viewTab
    #### exportTab ----
    shinydashboard::tabItem(tabName = "exportTab",
                            h2("Export results"),
                            fluidRow(
                              shiny::column(width = 12,
                                            shiny::tabsetPanel(
                                              shiny::tabPanel("Raw", 
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 4,
                                                                              # htmltools::h4("Set basic parameters"),
                                                                              # hr(),
                                                                              shinyWidgets::pickerInput(inputId = "scenarioExportInput",
                                                                                                        label = "Choose scenario",
                                                                                                        choices = NULL,
                                                                                                        multiple = TRUE,
                                                                                                        options = list("max-options" = 1),
                                                                                                        inline = FALSE
                                                                              ),
                                                                              shinyWidgets::actionBttn(inputId = "doExport", 
                                                                                                       label = "Export",
                                                                                                       style = "fill",
                                                                                                       size = "md",
                                                                                                       color = "success"),
                                                                              shiny::sliderInput("bins3", 
                                                                                                 label = htmltools::p(i18n$t("Hello Shiny!"), 
                                                                                                                      shiny::actionButton("titleBtId", 
                                                                                                                                          label = "", 
                                                                                                                                          icon = shiny::icon("info"), 
                                                                                                                                          style = 'padding:4px; font-size:60%')), 
                                                                                                 min = 1, max = 50, value = 30)
                                                                ), # End column
                                                                shiny::column(width = 8,
                                                                              # htmltools::h4("Select"),
                                                                              # hr()
                                                                ) # End column
                                                              ) # End row
                                              ) # End tabPanel
                                            ) # End tabsetPanel
                              ) # End column
                            ) # End row
    ) # End exportTab
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
