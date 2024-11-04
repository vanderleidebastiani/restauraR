#' @rdname app
# ui : call this function once somewhere
shinyWidgets::useSweetAlert()

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
  controlbarIcon = shiny::icon("gear")
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
    shinydashboard::menuItem("Select", tabName = "selectTab", icon = shiny::icon("filter"))
    # shinydashboard::menuItem("Merge", tabName = "mergeTab", icon = shiny::icon("code-merge"))
    # shinydashboard::menuItem("Traits data", tabName = "dataTab", icon = shiny::icon("th")),
    # shinydashboard::menuItem("Themes", tabName = "menuTheme", icon = shiny::icon("newspaper")),
    # shinydashboard::menuItem("Export", tabName = "menuExport", icon = shiny::icon("download"))
  )
)

### Control bar ----
controlbar <- shinydashboardPlus::dashboardControlbar(
  id = "controlbar",
  shinydashboardPlus::controlbarMenu(
    id = "menu",
    controlbarItem(
      title = "Global options",
      shinyWidgets::prettyRadioButtons(inputId = "fileSep",
                                       label = "Separator character",
                                       choices = c(",", ";"),
                                       selected = ",",
                                       inline = TRUE,
                                       status = "primary"
      )
    )
  )
)


### Body ----
body <- dashboardBody(
  # Global CSS tags
  tags$head(
    tags$style(shiny::HTML(
      "hr {border-top: 1px solid #000000;}"
    ))
  ),
  # Use shinyjs functions
  useShinyjs(),
  tabItems(
    #### dataInputTab ----
    # Data input tab
    tabItem(tabName = "dataInputTab",
            h2("Data input"),
            hr(),
            fluidRow(
              shiny::column(width = 12,
                            tabsetPanel(
                              tabPanel("Load files", 
                                       fluidRow(
                                         br(),
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
                                                       br(),
                                                       shinyWidgets::actionBttn(inputId = "doClear", 
                                                                                label = "Clear all",
                                                                                style = "fill",
                                                                                size = "md",
                                                                                color = "danger")
                                                       
                                         ) # End column
                                       ) # End row
                              ), # End load files tab
                              tabPanel("View traits", 
                                       fluidRow(
                                         br(),
                                         shiny::column(width = 12,
                                                       rhandsontable::rHandsontableOutput("traitsData")	  
                                         )
                                       ) # End row
                              ),
                              tabPanel("View restoration sites", 
                                       fluidRow(
                                         br(),
                                         shiny::column(width = 12,
                                                       rhandsontable::rHandsontableOutput("traitsData2")	  
                                         )
                                       ) # End row
                              ),
                              tabPanel("View reference sites", 
                                       fluidRow(
                                         br(),
                                         shiny::column(width = 12,
                                                       rhandsontable::rHandsontableOutput("traitsData3")	  
                                         )
                                       ) # End row
                              )
                            )
              ) # End row
            )
    ), # End dataInputTab
    
    # tabItem(tabName = "dataInputTab",
    # 		h2("Data input"),
    # 		hr(),
    # 		fluidRow(
    # 			shiny::column(width = 12,
    # 						  shiny::fileInput(inputId = "traitsInput",
    # 						  				 label = "Traits",
    # 						  				 accept = c(".csv"),
    # 						  				 buttonLabel = "Browse..."
    # 						  ),
    # 						  # rhandsontable::rHandsontableOutput("traitsData"),
    # 						  # br(),
    # 						  shiny::fileInput(inputId = "restCompInput",
    # 						  				 label = "Species composition of restoration sites",
    # 						  				 accept = c(".csv"),
    # 						  				 buttonLabel = "Browse..."
    # 						  ),
    # 						  shiny::fileInput(inputId = "restGroupInput",
    # 						  				 label = "Complementary information for restoration sites",
    # 						  				 accept = c(".csv"),
    # 						  				 buttonLabel = "Browse..."
    # 						  ),
    # 						  shiny::fileInput(inputId = "referenceInput",
    # 						  				 label = "Species composition of reference sites",
    # 						  				 accept = c(".csv"),
    # 						  				 buttonLabel = "Browse..."
    # 						  ),
    # 						  shiny::fileInput(inputId = "supplementaryInput",
    # 						  				 label = "Species composition of supplementary sites",
    # 						  				 accept = c(".csv"),
    # 						  				 buttonLabel = "Browse..."
    # 						  )
    # 			) # End column
    # 		) # End row
    # ), # End dataInputTab
    #### simulateTab ----
    tabItem(tabName = "simulateTab",
            h2("Simulated communities"),
            hr(),
            shiny::fluidRow(
              shiny::column(width = 8,
                            h4("Basic parameters"),
                            hr(),
                            textInput(inputId = "prefixSimInput",
                                      label = "Simulation name",
                                      value = "Sim_1"
                            ),
                            shinyWidgets::prettyRadioButtons(inputId = "goalsSimInput",
                                                             label = "Restoration goals",
                                                             choices = c("New", "Ongoing"),
                                                             selected = "New",
                                                             inline = TRUE,
                                                             status = "primary"
                            ),
                            shinyWidgets::prettyRadioButtons(inputId = "methodSimInput",
                                                             label = "Method",
                                                             choices = c("Proportions", "Individuals"),
                                                             selected = "Proportions",
                                                             inline = TRUE,
                                                             status = "primary"
                            ),
                            conditionalPanel(condition = "(input.methodSimInput == 'Individuals')",
                                             shiny::numericInput(inputId = "nIndSimInput", 
                                                                 label = "The number of individuals to draw",
                                                                 value = NULL
                                             )
                            ),
                            sliderTextInput(
                              inputId = "richSliderSimInput",
                              label = "Range of richness",
                              choices = c(1, 1),
                              selected = c(1, 1)
                            ),
                            shiny::numericInput(inputId = "itSimInput", 
                                                label = "Number of iterations",
                                                value = 1000,
                                                min = 4),
                            shinyWidgets::pickerInput(
                              inputId = "avaSimInput",
                              label = "Species availability",
                              choices = NULL,
                              multiple = TRUE,
                              options = list("max-options" = 1),
                              inline = FALSE
                            ),
                            shinyWidgets::pickerInput(
                              inputId = "undSimInput",
                              label = "Undesired species",
                              choices = NULL,
                              multiple = TRUE,
                              options = list("max-options" = 1),
                              inline = FALSE
                            ),
                            shinyWidgets::pickerInput(
                              inputId = "cwmSimInput",
                              label = "Traits to Community Weighted Mean",
                              choices = NULL,
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE),
                              inline = FALSE
                            ),
                            shinyWidgets::pickerInput(
                              inputId = "raoSimInput",
                              label = "Traits to Rao Quadratic Entropy",
                              choices = NULL,
                              multiple = TRUE,
                              options = list(`actions-box` = TRUE),
                              inline = FALSE
                            ),
                            shinyWidgets::prettyRadioButtons(inputId = "speficyGroupsSimInput",
                                                             label = "Specify probabilities for groups of species",
                                                             choices = c("Yes", "No"),
                                                             selected = "No",
                                                             inline = TRUE,
                                                             status = "primary"
                            ),
                            conditionalPanel(condition = "(input.speficyGroupsSimInput == 'Yes')",
                                             shinyWidgets::pickerInput(
                                               inputId = "groupSimInput",
                                               label = "Species group",
                                               choices = NULL,
                                               multiple = TRUE,
                                               options = list("max-options" = 1),
                                               inline = FALSE
                                             ),
                                             shinyWidgets::prettyRadioButtons(inputId = "probGroupTypeSimInput",
                                                                              label = "Probabilities to draw species",
                                                                              choices = c("Abundance", "Richness and abundance"),
                                                                              selected = "Abundance",
                                                                              inline = TRUE,
                                                                              status = "primary"
                                             ),
                                             br(),
                                             conditionalPanel(condition = "(input.probGroupTypeSimInput == 'Richness and abundance')",
                                                              uiOutput("slidersProbRicSim"),
                                                              br(),
                                                              br()
                                             ),
                                             conditionalPanel(condition = "(input.probGroupTypeSimInput == 'Abundance' || input.probGroupTypeSimInput == 'Richness and abundance')",
                                                              uiOutput("slidersProbAbuSim"),
                                                              br()
                                             )
                            ),
                            br(),
                            br(),
                            h4("Advanced parameters"),
                            hr(),
                            conditionalPanel(condition = "(input.methodSimInput == 'Individuals')",
                                             shinyWidgets::pickerInput(
                                               inputId = "probSimInput",
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
                            sliderInput(inputId = "phiSimInput", 
                                        label = "Weights of either quadratic entropy or entropy",
                                        value = 1,
                                        min = 0,
                                        max = 1)
              ), # End column
              shiny::column(width = 4, 
                            h4("Run"),
                            hr(),
                            shinyWidgets::actionBttn(inputId = "doSimulate", 
                                                     label = "Simulate",
                                                     style = "fill",
                                                     size = "lg",
                                                     color = "success"),
                            br(),
                            br(),
                            shiny::textOutput(outputId = "countScenariosText"),
                            shiny::textOutput(outputId = "countSimulationText"),
                            br(),
                            br(),
                            h4("Merge"),
                            hr(),
                            textInput(inputId = "mergeSimulateNameInput",
                                      label = "Merged simulation name",
                                      value = "Merged_1"),
                            shinyWidgets::pickerInput(
                              inputId = "mergeSimulateInput",
                              label = "Select to merge",
                              choices = NULL,
                              multiple = TRUE,
                              inline = FALSE
                            ),
                            shinyWidgets::actionBttn(inputId = "doSimulateMerge", 
                                                     label = "Merge",
                                                     style = "fill",
                                                     size = "md",
                                                     color = "success"),
                            br(),
                            br(),
                            br(),
                            h4("Remove"),
                            hr(),
                            shinyWidgets::pickerInput(
                              inputId = "removeSimulateInput",
                              label = "Select to remove",
                              choices = NULL,
                              multiple = TRUE,
                              inline = FALSE
                            ),
                            shinyWidgets::actionBttn(inputId = "doRemove", 
                                                     label = "Remove",
                                                     style = "fill",
                                                     size = "md",
                                                     color = "danger")
              ) # End column
            ) # End row
    ), # End simulateTab
    
    #### computeTab ----
    tabItem(tabName = "computeTab",
            h2("Compute parameters"),
            hr(),
            fluidRow(
              shiny::column(width = 8,
                            # x, trait, stan = NULL, reference = NULL, supplementary = NULL
                            
                            shinyWidgets::pickerInput(
                              inputId = "avaComputeInput",
                              label = "Species availability",
                              choices = "---None---",
                              inline = FALSE
                            ),
                            shinyWidgets::pickerInput(
                              inputId = "cwmComputeInput",
                              label = "Traits to Community Weighted Mean",
                              choices = "---None---",
                              multiple = TRUE,
                              inline = FALSE
                            ),
                            shinyWidgets::pickerInput(
                              inputId = "cwvComputeInput",
                              label = "Traits to Community Weighted Variance ",
                              choices = "---None---",
                              multiple = TRUE,
                              inline = FALSE
                            ),
                            shinyWidgets::pickerInput(
                              inputId = "raoComputeInput",
                              label = "Traits to Rao Quadratic Entropy",
                              choices = "---None---",
                              multiple = TRUE,
                              inline = FALSE
                            ),
                            shinyWidgets::pickerInput(
                              inputId = "costComputeInput",
                              label = "Cost per individual",
                              choices = "---None---",
                              inline = FALSE
                            ),
                            shinyWidgets::pickerInput(
                              inputId = "densComputeInput",
                              label = "Species planting density",
                              choices = "---None---",
                              inline = FALSE
                            ),
                            
                            
                            
              ), # End column
              shiny::column(width = 4,
                            shinyWidgets::actionBttn(inputId = "doCompute", 
                                                     label = "Compute",
                                                     style = "fill",
                                                     size = "lg",
                                                     color = "success")
              ) # End column
            ) # End row
    ), # End computeTab
    #### selectTab ----
    tabItem(tabName = "selectTab",
            h2("Select communities"),
            hr(),
            fluidRow(
              shiny::column(width = 8,
                            # x,
                            textInput(inputId = "testsDetInput",
                                      label = "Deterministic selection"),
                            textInput(inputId = "testsHieInput",
                                      label = "Hierarchical selection"),
                            shinyWidgets::pickerInput(
                              inputId = "groupSelectInput",
                              label = "Species groups",
                              choices = "---None---",
                              inline = FALSE
                            ),
                            shinyWidgets::prettyRadioButtons(inputId = "singleSelectionInput",
                                                             label = "Selection method",
                                                             choices = c("Single", "Multiple"),
                                                             selected = "Single",
                                                             inline = TRUE,
                                                             status = "primary"
                            ),
              ),
              shiny::column(width = 4,
                            shinyWidgets::actionBttn(inputId = "doSelect", 
                                                     label = "Select",
                                                     style = "fill",
                                                     size = "lg",
                                                     color = "success")
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
