#' @rdname app
# UI ----

# ui : call this function once somewhere
shinyWidgets::useSweetAlert()

i18n <- shiny.i18n::Translator$new(translation_json_path = system.file("app", "translation.json", package = "resbiota"))
# i18n <- shiny.i18n::Translator$new(translation_json_path = "resbiota/inst/app/translation.json")
i18n$set_translation_language("en")
i18n$use_js()
# i18n$set_translation_language("pt")


## Shiny UI ----
### Header ----
header <- shinydashboardPlus::dashboardHeader(
  title = htmltools::tagList(
    # Use shiny.i18n functions
    # stackoverflow.com/questions/73509710
    shiny.i18n::usei18n(i18n),
    # Use shinyjs functions
    shinyjs::useShinyjs(),
    tags$span(
      class = "logo-mini", "res"
    ),
    tags$span(
      class = "logo-lg", "resbiota"
    )
  ),
  titleWidth = 175,
  controlbarIcon = shiny::icon("gears")
)


### Sidebar ----
sidebar <- shinydashboardPlus::dashboardSidebar(
  collapsed = TRUE,
  width = 175,
  shinydashboard::sidebarMenu(
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id = "tabs",
    shinydashboard::menuItem(text = i18n$t("Home"), tabName = "startTab", icon = shiny::icon("seedling")),
    shinydashboard::menuItem(text = i18n$t("Data input"), tabName = "dataInputTab", icon = shiny::icon("database")),
    shinydashboard::menuItem(text = i18n$t("Simulate"), tabName = "simulateTab", icon = shiny::icon("sliders-h")),
    shinydashboard::menuItem(text = i18n$t("Compute"), tabName = "computeTab", icon = shiny::icon("calculator")),
    shinydashboard::menuItem(text = i18n$t("Select"), tabName = "selectTab", icon = shiny::icon("filter")),
    shinydashboard::menuItem(text = i18n$t("View"), tabName = "viewTab", icon = shiny::icon("newspaper"))
  )
)

### Control bar ----
controlbar <- shinydashboardPlus::dashboardControlbar(
  id = "controlbar",
  shinydashboardPlus::controlbarMenu(
    id = "menu",
    shinydashboardPlus::controlbarItem(
      title = i18n$t("Global options"),
      shiny::textInput("projectName", label = i18n$t("Project name"), value = "Projet"),
      shinyWidgets::prettyRadioButtons(inputId = "fileSep",
                                       label = i18n$t("Separator character"),
                                       choices = c(",", ";"),
                                       selected = ",",
                                       inline = TRUE,
                                       status = "primary"
      ),
      shiny::uiOutput("decimalPlaces"),
      # shiny::numericInput(inputId = "decimalPlaces",
      # 					label = "Decimal places",
      # 					value = 5,
      # 					min = 3,
      # 					step = 1),
      # radioGroupButtons(inputId = "selectedLanguage",
      # 					 label = i18n$t("Change language"),
      # 					 choices = stats::setNames(
      # 					 	i18n$get_languages(),
      # 					 	c("English", paste0("Portugu", "\u00ea","s")) # Set labels for the languages
      # 					 ),
      # 					 selected = i18n$get_key_translation(),
      # 					 justified = TRUE
      # )
      shiny::selectInput(inputId = "selectedLanguage",
                         label = i18n$t("Change language"),
                         choices = stats::setNames(
                           i18n$get_languages(),
                           c("English", paste0("Portugu", "\u00ea","s")) # Set labels for the languages
                         ),
                         selected = i18n$get_key_translation()
      )
    ),
    shinydashboardPlus::controlbarItem(
      title = "Plot options",
      shiny::numericInput(inputId = "saveWidth", label = i18n$t("Width (mm)"), value = 160),
      shiny::numericInput(inputId = "saveHeight", label = i18n$t("Height (mm)"), value = 120),
      shiny::numericInput(inputId = "saveDPI", label = i18n$t("DPI"), value = 300)
    )
  )
)

### Body ----
body <- shinydashboard::dashboardBody(
  # Use shiny.i18n functions
  # shiny.i18n::usei18n(i18n),
  # Global CSS tags
  tags$head(
    tags$style(shiny::HTML(
      "hr {border-top: 1px solid #000000;}",
      "#boxNH .box-header{display: none}",
      ".alertStartup {background-color:transparent !important;}",
      ".box-header .box-title {font-size: 16px; font-weight: normal;}"
    ))
  ),
  # Use shinyjs functions
  shinyjs::useShinyjs(),
  shinydashboard::tabItems(
    #### startTab -----
    # Getting Started tab
    shinydashboard::tabItem(tabName = "startTab",
                            htmltools::h2(i18n$t("Getting Started")),
                            shiny::fluidRow(
                              shiny::column(width = 12,
                                            shiny::tabsetPanel(
                                              shiny::tabPanel(i18n$t("Overview"),
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 12,
                                                                              htmltools::h4(htmltools::strong("resbiota - Restore Ecosystem Services and Biodiversity Through Trait-based Approaches")),
                                                                              htmltools::br(),
                                                                              htmltools::p(htmltools::strong("Description")),
                                                                              htmltools::p("Create restoration solutions to assemble communities from a regional species pool to achieve multiple functional targets based on simulation."),
                                                                              htmltools::br(), 
                                                                              htmltools::p(htmltools::strong("Steps")),
                                                                              DiagrammeR::grVizOutput("diagramOutput", height = "550px")
                                                                ) # End column
                                                              ) # End row
                                              ), # End tabPanel
                                              shiny::tabPanel(i18n$t("Details"),
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 12,
                                                                              htmltools::h4(htmltools::strong("resbiota - Restore Ecosystem Services and Biodiversity Through Trait-based Approaches")),
                                                                              htmltools::br(),
                                                                              htmltools::p(htmltools::strong(i18n$t("The framework"))),
                                                                              htmltools::p(i18n$t("The framework generates communities with distinct species compositions drawn at random from a species pool. The algorithm takes as input a data set with trait information for the species in the regional pool, that is, a list of species from which trait data are available that could be used to restore a particular ecosystem. Using species from the regional pool, the algorithm produces a set of N-simulated communities with species richness that ranges within a user-defined interval.")),
                                                                              htmltools::p(htmltools::strong(i18n$t("Traits and additional species information"))),
                                                                              htmltools::p(i18n$t("Each species in the pool can be described by functional traits that represent the ecological functions of communities, ecosystem services, or desirable characteristics for novel or restored communities. Those species can be classified into species that are available on the market and those that are not. Furthermore, the cost per individual, observed frequencies occurring in reference sites, and any additional information can be used to select species composition.")),
                                                                              htmltools::p(htmltools::strong(i18n$t("Simulations"))),
                                                                              htmltools::p(i18n$t("The species assemblages are generated using the function in the simulate tab. The species richness within simulated communities varies within a range defined by the user. Thus, it could be specified using the observed range in reference communities or desired restoration targets.")),
                                                                              htmltools::p(i18n$t("The sampling of species can be performed completely at random or selected species based on functional traits or a desired trait profile. If the cwm argument is set the species selections are constrained to specific community-weighted mean trait values, using the entire range of each trait. If the rao argument is set, the species selections are performed in order to optimise the functional diversity of a specific set of traits.")),
                                                                              htmltools::p(i18n$t("To obtain a high number of restoration solutions, all runs return a set of simulated communities with random species distribution and composition based on rules set with cwm and/or rao arguments. Furthermore, if the species available is informed, the sample of species compositions is also performed with only available species on the market. Thus, the function returns a wide range of variability in composition to posterior selection, including compositions with available and not available species in the market.")),
                                                                              htmltools::p(i18n$t("The framework returns a community matrix indicating the proportion of individuals that need to be added for each species. If no established communities are informed, the simulated communities are set as empty communities (sites to restore start with no species, and all species must be planted for restoration). Alternatively, if established communities are set, the new species and individuals are introduced into the established communities (sites to restore can start with pre-existing species). Thus, it is possible to increase the number of ecosystem functions in ongoing restoration sites and follow an approach of adaptive management.")),
                                                                              htmltools::p(htmltools::strong(i18n$t("Communities parameters"))),
                                                                              htmltools::p(i18n$t("After the initial step of species composition simulation, several function parameters can be calculated for each community. This step is mandatory, given that those parameters are used to select a subset of simulated communities that fit the restoration target. Parameters such as species richness, Functional Diversity (FD), Community Weighted Means (CWM), the average functional dissimilarity between each simulated community, and restoration cost can be calculated. In general, those function parameters are used as a proxy for ecosystem functions relevant to the restoration under evaluation.")),
                                                                              htmltools::p(i18n$t("The function in the compute tab is mandatory and allows compute the basic parameters: richness, Community Weighted Mean, Community Weighted Variance, Rao Quadratic Entropy and functional dissimilarity. The user must define which traits will be used to calculate each metric. Additionally, if are provided the species cost per individual and information about planting density for each species, the restoration costs are estimated. When the availability of species is specified, the function counts species unavailable in each community. Furthermore, it is optional to compute functional dissimilarity between selected communities and reference sites. Functional dissimilarity is a pair-wise metric between each selected community and each reference site. If you have more than one reference site, functional dissimilarity will be the average dissimilarity between each selected community and the reference sites.")),
                                                                              htmltools::p(i18n$t("The function in compute tab, subtab standardise parameters, allows the standardisation of the calculated parameters. Two methods are available: 'max', which divides the values by the maximum, and 'standardise', which scales the calculated parameters to zero mean and unit variance. For example, the 'max' method is useful to scale dissimilarity results by scaling the values in ranges from 0 (no dissimilarity) to 1 (maximum dissimilarity).")),
                                                                              htmltools::p(i18n$t("The function in the compute tab, in subtab multifunctionality, is optional and computes the matrix of multifunctionality and alpha multifunctionality. See the multifunctionality topic below.")),
                                                                              htmltools::p(htmltools::strong(i18n$t("Select communities"))),
                                                                              htmltools::p(i18n$t("The selections of simulated communities are performed using the function in the select tab. Selections are based on simple logical tests based on thresholds. The user must define parameter thresholds, that indicate the level of functional composition above (or below) which the function is considered to be restored in each site. The thresholds can be set for one or more calculated parameters in the previous step. This is an essential step that must be based on previous studies that indicate the relationship between species traits and ecosystem functions related to the desired restoration targets. Knowledge of the regional species pool, species availability and reference sites is important to find solutions that adequately optimise restoration goals.")),
                                                                              htmltools::p(i18n$t("The logical tests are specified using the sliders for each parameter which specify the threshold values. All tests are inclusive, testing values greater than or equal to the defined minimum and less than or equal to the defined maximum.")),
                                                                              htmltools::p(i18n$t("Two selection options can be used, individually or alternatively, based on filter and priority selection. In the filter selection, all simulations that are true in the input test are returned. In this case, all parameters evaluated must be true to a community to be selected. In the priority selection, the tests are evaluated hierarchically and only one simulation is selected. When all simulations fail in the first test, the function tries the next test. In the end, the function samples only one simulation from among those that passed all the tests. In this case, an additional argument defines if the selection is performed inside a specific group. This allows, for example, the selection of one simulation for each site that will be restored.")),
                                                                              htmltools::p(htmltools::strong(i18n$t("The reference sites"))),
                                                                              htmltools::p(i18n$t("Reference sites can be included as an input data set. Thus, all calculated parameters also are calculated to reference sites and can be used in posterior analysis or auxiliary to the selection procedure by identifying the natural range and values of these parameters in the reference ecosystem.")),
                                                                              htmltools::p(htmltools::strong(i18n$t("The outputs"))),
                                                                              htmltools::p(i18n$t("The main outputs are a community matrix with species relative abundances and a data frame with the functional parameters calculated for each community. These outputs allow the user to investigate the relationship between these parameters, select communities that meet the restoration goals, and identify functionally important species that are not available on the market.")),
                                                                              htmltools::p(htmltools::strong(i18n$t("Multifunctionality"))),
                                                                              htmltools::p(i18n$t("The restoration projects can target restoring multiple ecosystem services, called multifunctionality. Thus, different restoration objectives should be assigned to distinct sites within a restoration landscape. However, when dealing with multiple functions, trade-offs are likely to arise, whereby the pursuit of one function may prevent the achievement of another. The matrix of multifunctionality can be calculated using simple logical tests in each available functional parameter. Thus, the multifunctionality of each restoration site is defined as the number of functions above (or below) a given threshold. The sum of individual tests is defined as the alpha multifunctionality metric.")),
                                                                              htmltools::p(i18n$t("The selection of simulated species composition can be performed using the alpha multifunctionality metric to maximise multifunctionality between restored sites. The alpha multifunctionality also allows the selection of a simulated community when no solution satisfies all initial criteria, thus, users can use a less restricted solution.")),
                                                                              htmltools::p(i18n$t("High values of the alpha multifunctionality can be a preferable criterion for selection, meaning that, for each restoration site, the selection is performed in order to return a solution with the highest number of ecosystem functions recovered.")),
                                                                              htmltools::p(htmltools::strong(i18n$t("Visualisation"))),
                                                                              htmltools::p(i18n$t("The results visualisation functions are auxiliary to checking parameters in reference sites, analysing the distribution of functional parameters in simulated communities and showing final solutions of selected species composition.")),
                                                                              htmltools::p(i18n$t("The function in the view tab allows the visualisation of basic results using scatter plots to show the trade-offs between several parameters, restoration costs, species richness and any calculated functional parameter. This visualisation includes all simulated communities and also allows the inclusion of observed relations in reference sites.")),
                                                                              htmltools::p(i18n$t("The function in the view multifunctionality tab allows visualise the multifunctionality sets creating a graphical representation of the number of sites where each function has been restored, and the number of sites where combinations of functions were restored.")),
                                                                              htmltools::p(htmltools::strong(i18n$t("Merge functions"))),
                                                                              htmltools::p(i18n$t("The package includes functions to merge results set in different operations.")),
                                                                              htmltools::p(i18n$t("The function in the simulate tab concatenates simulated communities generated under different scenarios. For example, part of restoration sites can be generated from empty communities, and in another scenario, the species composition can be generated based on established communities. Thus, both scenarios can be concatenated to subsequent steps as parameter calculations.")),
                                                                              htmltools::p(i18n$t("The function in the selection tab concatenates different selection procedures. For example, if restoration sites have diverse desired restoration targets, the selection can be performed in distinct steps and then concatenated."))
                                                                ) # End column
                                                              ) # End row
                                              ), # End tabPanel
                                              shiny::tabPanel(i18n$t("About"),
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 12,
                                                                              htmltools::h4(htmltools::strong("resbiota - v0.0.3")),
                                                                              htmltools::br(),
                                                                              htmltools::p(htmltools::strong(i18n$t("Authors"))),
                                                                              htmltools::p(paste0("Vanderlei J. Debastiani, Andr", "\u00e9", " G. Coutinho, Marcos B. Carlucci, Marcus V. Cianciaruso")),
                                                                              htmltools::br(), 
                                                                              htmltools::p(htmltools::strong(i18n$t("References"))),
                                                                              htmltools::p("Coutinho, A. G., Carlucci, M. B., & Cianciaruso, M. V. (2023). A framework to apply trait-based ecological restoration at large scales. Journal of Applied Ecology, 60, 1562-1571. https://doi.org/10.1111/1365-2664.14439"),
                                                                              htmltools::p("Coutinho, A. G., Nunes, A., Branquinho, C., Carlucci, M. B., & Cianciaruso, M. V. (2024). Natural regeneration enhances ecosystem multifunctionality but species addition can increase it during restoration monitoring. Manuscript in preparation.")
                                                                ) # End column
                                                              ) # End row
                                              ) # End tabPanel
                                            ) # End tabsetPanel
                              ) # End column
                            ) # End row
    ), # End startTab
    #### dataInputTab ----
    # Data input tab
    shinydashboard::tabItem(tabName = "dataInputTab",
                            htmltools::h2(i18n$t("Data input")),
                            # hr(),
                            shiny::fluidRow(
                              shiny::column(width = 12,
                                            shiny::tabsetPanel(
                                              shiny::tabPanel(i18n$t("Load files"), 
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 8, 
                                                                              shinydashboardPlus::box(id = "boxNH", width = 12, title = NULL, headerBorder = FALSE,
                                                                                                      shiny::fileInput(inputId = "traitsInput",
                                                                                                                       # label = "Traits",
                                                                                                                       label = htmltools::p(i18n$t("Traits"), 
                                                                                                                                            shiny::actionButton("traitsInputInfo",
                                                                                                                                                                label = "",
                                                                                                                                                                icon = shiny::icon("info"),
                                                                                                                                                                style = "padding:3px; font-size:60%")),
                                                                                                                       accept = c(".csv"),
                                                                                                                       buttonLabel = "Browse..."
                                                                                                      ),
                                                                                                      htmltools::div(
                                                                                                        shinyWidgets::actionBttn(inputId = "doClearTraits", 
                                                                                                                                 label = i18n$t("Clear"),
                                                                                                                                 style = "fill",
                                                                                                                                 size = "sm",
                                                                                                                                 color = "default"),
                                                                                                        style = "margin-top:0px"
                                                                                                      )
                                                                              ),
                                                                              shinydashboardPlus::box(id = "boxNH", width = 12, title = NULL, headerBorder = FALSE,
                                                                                                      shiny::fileInput(inputId = "restCompInput",
                                                                                                                       # label = "Species composition of restoration sites",
                                                                                                                       label = htmltools::p(i18n$t("Species composition of restoration sites"), 
                                                                                                                                            shiny::actionButton("restCompInputInfo",
                                                                                                                                                                label = "",
                                                                                                                                                                icon = shiny::icon("info"),
                                                                                                                                                                style = "padding:3px; font-size:60%")),
                                                                                                                       accept = c(".csv"),
                                                                                                                       buttonLabel = i18n$t("Browse...")
                                                                                                      ),
                                                                                                      htmltools::div(
                                                                                                        shinyWidgets::actionBttn(inputId = "doClearRestComp", 
                                                                                                                                 label = i18n$t("Clear"),
                                                                                                                                 style = "fill",
                                                                                                                                 size = "sm",
                                                                                                                                 color = "default"),
                                                                                                        style = "margin-top:0px"
                                                                                                      )
                                                                              ),
                                                                              shinydashboardPlus::box(id = "boxNH", width = 12, title = NULL, headerBorder = FALSE,
                                                                                                      shiny::fileInput(inputId = "restGroupInput",
                                                                                                                       # label = "Complementary information for restoration sites",
                                                                                                                       label = htmltools::p(i18n$t("Complementary information for restoration sites"), 
                                                                                                                                            shiny::actionButton("restGroupInputInfo",
                                                                                                                                                                label = "",
                                                                                                                                                                icon = shiny::icon("info"),
                                                                                                                                                                style = "padding:3px; font-size:60%")),
                                                                                                                       accept = c(".csv"),
                                                                                                                       buttonLabel = i18n$t("Browse...")
                                                                                                      ),
                                                                                                      htmltools::div(
                                                                                                        shinyWidgets::actionBttn(inputId = "doClearRestGroup", 
                                                                                                                                 label = i18n$t("Clear"),
                                                                                                                                 style = "fill",
                                                                                                                                 size = "sm",
                                                                                                                                 color = "default"),
                                                                                                        style = "margin-top:0px"
                                                                                                      )
                                                                              ),
                                                                              shinydashboardPlus::box(id = "boxNH", width = 12, title = NULL, headerBorder = FALSE,
                                                                                                      shiny::fileInput(inputId = "referenceInput",
                                                                                                                       # label = "Species composition of reference sites",
                                                                                                                       label = htmltools::p(i18n$t("Species composition of reference sites"), 
                                                                                                                                            shiny::actionButton("referenceInputInfo",
                                                                                                                                                                label = "",
                                                                                                                                                                icon = shiny::icon("info"),
                                                                                                                                                                style = "padding:3px; font-size:60%")),
                                                                                                                       accept = c(".csv"),
                                                                                                                       buttonLabel = i18n$t("Browse...")
                                                                                                      ),
                                                                                                      htmltools::div(
                                                                                                        shinyWidgets::actionBttn(inputId = "doClearReference", 
                                                                                                                                 label = i18n$t("Clear"),
                                                                                                                                 style = "fill",
                                                                                                                                 size = "sm",
                                                                                                                                 color = "default"),
                                                                                                        style = "margin-top:0px"
                                                                                                      )
                                                                              ),
                                                                              shinydashboardPlus::box(id = "boxNH", width = 12, title = NULL, headerBorder = FALSE,
                                                                                                      shiny::fileInput(inputId = "supplementaryInput",
                                                                                                                       # label = "Species composition of supplementary sites",
                                                                                                                       label = htmltools::p(i18n$t("Species composition of supplementary sites"), 
                                                                                                                                            shiny::actionButton("supplementaryInputInfo",
                                                                                                                                                                label = "",
                                                                                                                                                                icon = shiny::icon("info"),
                                                                                                                                                                style = "padding:3px; font-size:60%")),
                                                                                                                       accept = c(".csv"),
                                                                                                                       buttonLabel = i18n$t("Browse...")
                                                                                                      ),
                                                                                                      htmltools::div(
                                                                                                        shinyWidgets::actionBttn(inputId = "doClearSupplementary", 
                                                                                                                                 label = i18n$t("Clear"),
                                                                                                                                 style = "fill",
                                                                                                                                 size = "sm",
                                                                                                                                 color = "default"),
                                                                                                        style = "margin-top:0px"
                                                                                                      )
                                                                              )
                                                                ), # End column
                                                                shiny::column(width = 4, 
                                                                              htmltools::br(),
                                                                              shinyWidgets::actionBttn(inputId = "doCheck",
                                                                                                       label = i18n$t("Check data"),
                                                                                                       style = "fill",
                                                                                                       size = "md",
                                                                                                       color = "success"),
                                                                              shinyWidgets::actionBttn(inputId = "doClear", 
                                                                                                       label = i18n$t("Clear all"),
                                                                                                       style = "fill",
                                                                                                       size = "md",
                                                                                                       color = "danger")
                                                                              
                                                                ) # End column
                                                              ) # End row
                                              ), # End load files tab
                                              shiny::tabPanel(i18n$t("View traits"), 
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 12,
                                                                              shiny::conditionalPanel(condition = "output.showTraitsData == true",
                                                                                                      htmltools::h5(htmltools::strong(i18n$t("Traits class"))),
                                                                                                      rhandsontable::rHandsontableOutput("outputTableTraitsClass"),
                                                                                                      htmltools::br(),
                                                                                                      htmltools::h5(htmltools::strong(i18n$t("Traits data"))),
                                                                                                      rhandsontable::rHandsontableOutput("outputTableTraitsData")
                                                                              )
                                                                ) # End column
                                                              ) # End row
                                              ), # End view traits tab
                                              shiny::tabPanel(i18n$t("View restoration sites"), 
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 12,
                                                                              shiny::conditionalPanel(condition = "output.showRestComp == true",
                                                                                                      htmltools::h5(htmltools::strong(i18n$t("Species composition"))),
                                                                                                      rhandsontable::rHandsontableOutput("outputTableRestComp"),
                                                                                                      htmltools::br()				 
                                                                              ),
                                                                              shiny::conditionalPanel(condition = "output.showRestGroup == true",
                                                                                                      htmltools::h5(htmltools::strong(i18n$t("Complementary information"))),
                                                                                                      rhandsontable::rHandsontableOutput("outputTableRestGroup")				 
                                                                              )
                                                                ) # End column
                                                              ) # End row
                                              ), # End view restoration sites tab
                                              shiny::tabPanel(i18n$t("View reference sites"), 
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 12,
                                                                              shiny::conditionalPanel(condition = "output.showReference == true",
                                                                                                      htmltools::h5(htmltools::strong(i18n$t("Species composition of reference sites"))),
                                                                                                      rhandsontable::rHandsontableOutput("outputTableRefComp"),
                                                                                                      htmltools::br()				 
                                                                              ),
                                                                              shiny::conditionalPanel(condition = "output.showSupplementary == true",
                                                                                                      htmltools::h5(htmltools::strong(i18n$t("Species composition of supplementary sites"))),
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
                            htmltools::h2(i18n$t("Simulated communities")),
                            # htmltools::hr(),
                            shiny::fluidRow(
                              shiny::column(width = 12,
                                            shiny::tabsetPanel(
                                              shiny::tabPanel(i18n$t("Simulate"), 
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 8,
                                                                              
                                                                              shinydashboardPlus::box(id = "box", width = 12, headerBorder = FALSE, 
                                                                                                      title = i18n$t("Scenario"), 
                                                                                                      collapsible = TRUE,
                                                                                                      collapsed = FALSE,
                                                                                                      shiny::textInput(inputId = "prefixSimInput",
                                                                                                                       label = i18n$t("Simulation name"),
                                                                                                                       value = "Sim_1"
                                                                                                      )
                                                                              ),
                                                                              shinydashboardPlus::box(id = "box", width = 12, headerBorder = FALSE, 
                                                                                                      title = i18n$t("Basic parameters"), 
                                                                                                      collapsible = TRUE,
                                                                                                      collapsed = FALSE,
                                                                                                      shiny::uiOutput("radioGoalsSimOutput"),
                                                                                                      # shinyWidgets::prettyRadioButtons(inputId = "goalsSimInput",
                                                                                                      # 								 # label = "Restoration goals",
                                                                                                      # 								 label = htmltools::p(i18n$t("Restoration goals"), 
                                                                                                      # 								 					 shiny::actionButton("goalsSimInputInfo",
                                                                                                      # 								 					 					label = "",
                                                                                                      # 								 					 					icon = shiny::icon("info"),
                                                                                                      # 								 					 					style = "padding:3px; font-size:60%")),
                                                                                                      # 								 choices = stats::setNames(
                                                                                                      # 								 	c("New", "Ongoing"),
                                                                                                      # 								 	c("New", "Ongoing") # Set labels
                                                                                                      # 								 ),
                                                                                                      # 								 selected = "New",
                                                                                                      # 								 inline = TRUE,
                                                                                                      # 								 status = "primary"
                                                                                                      # ),
                                                                                                      shiny::uiOutput("radioMethodSimOutput"),
                                                                                                      # shinyWidgets::prettyRadioButtons(inputId = "methodSimInput",
                                                                                                      # 								 # label = "Method",
                                                                                                      # 								 label = htmltools::p(i18n$t("Method"), 
                                                                                                      # 								 					 shiny::actionButton("methodSimInputInfo",
                                                                                                      # 								 					 					label = "",
                                                                                                      # 								 					 					icon = shiny::icon("info"),
                                                                                                      # 								 					 					style = "padding:3px; font-size:60%")),
                                                                                                      # 								 choices = stats::setNames(
                                                                                                      # 								 	c("Proportions", "Individuals"),
                                                                                                      # 								 	c("Proportions", "Individuals") # Set labels
                                                                                                      # 								 ),
                                                                                                      # 								 selected = "Proportions",
                                                                                                      # 								 inline = TRUE,
                                                                                                      # 								 status = "primary"
                                                                                                      # ),
                                                                                                      shiny::conditionalPanel(condition = "(input.methodSimInput == 'Individuals')",
                                                                                                                              shiny::numericInput(inputId = "nIndSimInput", 
                                                                                                                                                  # label = "The number of individuals to draw",
                                                                                                                                                  label = htmltools::p(i18n$t("The number of individuals to draw"), 
                                                                                                                                                                       shiny::actionButton("nIndSimInputInfo",
                                                                                                                                                                                           label = "",
                                                                                                                                                                                           icon = shiny::icon("info"),
                                                                                                                                                                                           style = "padding:3px; font-size:60%")),
                                                                                                                                                  value = NULL
                                                                                                                              )
                                                                                                      ),
                                                                                                      shinyWidgets::sliderTextInput(inputId = "richSliderSimInput",
                                                                                                                                    # label = "Range of richness",
                                                                                                                                    label = htmltools::p(i18n$t("Range of richness"), 
                                                                                                                                                         shiny::actionButton("richSliderSimInputInfo",
                                                                                                                                                                             label = "",
                                                                                                                                                                             icon = shiny::icon("info"),
                                                                                                                                                                             style = "padding:3px; font-size:60%")),
                                                                                                                                    choices = c(1, 1),
                                                                                                                                    selected = c(1, 1)
                                                                                                      ),
                                                                                                      shiny::numericInput(inputId = "itSimInput", 
                                                                                                                          # label = "Number of iterations",
                                                                                                                          label = htmltools::p(i18n$t("Number of iterations"), 
                                                                                                                                               shiny::actionButton("itSimInputInfo",
                                                                                                                                                                   label = "",
                                                                                                                                                                   icon = shiny::icon("info"),
                                                                                                                                                                   style = "padding:3px; font-size:60%")),
                                                                                                                          value = 1000,
                                                                                                                          min = 4)
                                                                              ),
                                                                              shinydashboardPlus::box(id = "box", width = 12, headerBorder = FALSE, 
                                                                                                      title = i18n$t("Species information"), 
                                                                                                      collapsible = TRUE,
                                                                                                      collapsed = TRUE,
                                                                                                      shinyWidgets::pickerInput(inputId = "avaSimInput",
                                                                                                                                # label = "Species availability",
                                                                                                                                label = htmltools::p(i18n$t("Species availability"), 
                                                                                                                                                     shiny::actionButton("avaSimInputInfo",
                                                                                                                                                                         label = "",
                                                                                                                                                                         icon = shiny::icon("info"),
                                                                                                                                                                         style = "padding:3px; font-size:60%")),
                                                                                                                                choices = NULL,
                                                                                                                                multiple = TRUE,
                                                                                                                                options = list("max-options" = 1),
                                                                                                                                inline = FALSE
                                                                                                      ),
                                                                                                      shinyWidgets::pickerInput(inputId = "undSimInput",
                                                                                                                                # label = "Undesired species",
                                                                                                                                label = htmltools::p(i18n$t("Undesired species"), 
                                                                                                                                                     shiny::actionButton("undSimInputInfo",
                                                                                                                                                                         label = "",
                                                                                                                                                                         icon = shiny::icon("info"),
                                                                                                                                                                         style = "padding:3px; font-size:60%")),
                                                                                                                                choices = NULL,
                                                                                                                                multiple = TRUE,
                                                                                                                                options = list("max-options" = 1),
                                                                                                                                inline = FALSE
                                                                                                      )
                                                                              ),
                                                                              shinydashboardPlus::box(id = "box", width = 12, headerBorder = FALSE,
                                                                                                      title = i18n$t("Functional traits constraints"), 
                                                                                                      collapsible = TRUE,
                                                                                                      collapsed = FALSE,
                                                                                                      shinyWidgets::pickerInput(inputId = "constCWMSimInput",
                                                                                                                                # label = "Traits to Community Weighted Mean",
                                                                                                                                label = htmltools::p(i18n$t("Traits to Community Weighted Mean"), 
                                                                                                                                                     shiny::actionButton("constCWMSimInputInfo",
                                                                                                                                                                         label = "",
                                                                                                                                                                         icon = shiny::icon("info"),
                                                                                                                                                                         style = "padding:3px; font-size:60%")),
                                                                                                                                choices = NULL,
                                                                                                                                multiple = TRUE,
                                                                                                                                options = list(`actions-box` = TRUE),
                                                                                                                                inline = FALSE
                                                                                                      ),
                                                                                                      shinyWidgets::pickerInput(inputId = "maxDiverSimInput",
                                                                                                                                # label = "Traits to Rao Quadratic Entropy",
                                                                                                                                label = htmltools::p(i18n$t("Traits to Rao Quadratic Entropy"), 
                                                                                                                                                     shiny::actionButton("maxDiverSimInputInfo",
                                                                                                                                                                         label = "",
                                                                                                                                                                         icon = shiny::icon("info"),
                                                                                                                                                                         style = "padding:3px; font-size:60%")),
                                                                                                                                choices = NULL,
                                                                                                                                multiple = TRUE,
                                                                                                                                options = list(`actions-box` = TRUE),
                                                                                                                                inline = FALSE
                                                                                                      )
                                                                              ),
                                                                              shinydashboardPlus::box(id = "box", width = 12, headerBorder = FALSE, 
                                                                                                      title = i18n$t("Constraints by species groups"), 
                                                                                                      collapsible = TRUE,
                                                                                                      collapsed = TRUE,
                                                                                                      shiny::uiOutput("radioSpeficyGroupsSimOutput"),
                                                                                                      # shinyWidgets::prettyRadioButtons(inputId = "speficyGroupsSimInput",
                                                                                                      # 								 # label = "Specify probabilities for groups of species",
                                                                                                      # 								 label = htmltools::p(i18n$t("Specify probabilities for groups of species"), 
                                                                                                      # 								 					 shiny::actionButton("speficyGroupsSimInputInfo",
                                                                                                      # 								 					 					label = "",
                                                                                                      # 								 					 					icon = shiny::icon("info"),
                                                                                                      # 								 					 					style = "padding:3px; font-size:60%")),
                                                                                                      # 								 choices = stats::setNames(
                                                                                                      # 								 	c("Yes", "No"),
                                                                                                      # 								 	c("Yes", "No") # Set labels
                                                                                                      # 								 ),
                                                                                                      # 								 selected = "No",
                                                                                                      # 								 inline = TRUE,
                                                                                                      # 								 status = "primary"
                                                                                                      # ),
                                                                                                      shiny::conditionalPanel(condition = "(input.speficyGroupsSimInput == 'Yes')",
                                                                                                                              shinyWidgets::pickerInput(inputId = "groupSimInput",
                                                                                                                                                        label = i18n$t("Species group"),
                                                                                                                                                        choices = NULL,
                                                                                                                                                        multiple = TRUE,
                                                                                                                                                        options = list("max-options" = 1),
                                                                                                                                                        inline = FALSE
                                                                                                                              ),
                                                                                                                              shiny::uiOutput("radioProbGroupTypeSimOutput"),
                                                                                                                              # shinyWidgets::prettyRadioButtons(inputId = "probGroupTypeSimInput",
                                                                                                                              # 								 # label = "Probabilities to draw species",
                                                                                                                              # 								 label = htmltools::p(i18n$t("Probabilities to draw species"), 
                                                                                                                              # 								 					 shiny::actionButton("probGroupTypeSimInputInfo",
                                                                                                                              # 								 					 					label = "",
                                                                                                                              # 								 					 					icon = shiny::icon("info"),
                                                                                                                              # 								 					 					style = "padding:3px; font-size:60%")),
                                                                                                                              # 								 choices = stats::setNames(
                                                                                                                              # 								 	c("Abundance", "Richness and abundance"),
                                                                                                                              # 								 	c("Abundance", "Richness and abundance") # Set labels
                                                                                                                              # 								 ),
                                                                                                                              # 								 selected = "Abundance",
                                                                                                                              # 								 inline = TRUE,
                                                                                                                              # 								 status = "primary"
                                                                                                                              # ),
                                                                                                                              # htmltools::br(),
                                                                                                                              shiny::conditionalPanel(condition = "(input.probGroupTypeSimInput == 'Richness and abundance')",
                                                                                                                                                      shiny::uiOutput("slidersProbRicSim"),
                                                                                                                                                      htmltools::br(),
                                                                                                                                                      htmltools::br()
                                                                                                                              ),
                                                                                                                              shiny::conditionalPanel(condition = "(input.probGroupTypeSimInput == 'Abundance' || input.probGroupTypeSimInput == 'Richness and abundance')",
                                                                                                                                                      shiny::uiOutput("slidersProbAbuSim"),
                                                                                                                                                      # htmltools::br()
                                                                                                                              )
                                                                                                      )
                                                                              ),
                                                                              shinydashboardPlus::box(id = "box", width = 12, headerBorder = FALSE,
                                                                                                      title = i18n$t("Advanced options"), 
                                                                                                      collapsible = TRUE,
                                                                                                      collapsed = TRUE,
                                                                                                      shiny::conditionalPanel(condition = "(input.methodSimInput == 'Individuals')",
                                                                                                                              shinyWidgets::pickerInput(inputId = "probSimInput",
                                                                                                                                                        # label = "Probabilities to draw individuals",
                                                                                                                                                        label = htmltools::p(i18n$t("Probabilities to draw individuals"), 
                                                                                                                                                                             shiny::actionButton("probSimInputInfo",
                                                                                                                                                                                                 label = "",
                                                                                                                                                                                                 icon = shiny::icon("info"),
                                                                                                                                                                                                 style = "padding:3px; font-size:60%")),
                                                                                                                                                        choices = NULL,
                                                                                                                                                        multiple = TRUE,
                                                                                                                                                        options = list("max-options" = 1),
                                                                                                                                                        inline = FALSE
                                                                                                                              ),
                                                                                                                              shiny::numericInput(inputId = "cvAbundSimInput", 
                                                                                                                                                  # label = "Coefficient of variation of the relative abundances",
                                                                                                                                                  label = htmltools::p(i18n$t("Coefficient of variation of the relative abundances"), 
                                                                                                                                                                       shiny::actionButton("cvAbundSimInputInfo",
                                                                                                                                                                                           label = "",
                                                                                                                                                                                           icon = shiny::icon("info"),
                                                                                                                                                                                           style = "padding:3px; font-size:60%")),
                                                                                                                                                  value = 1
                                                                                                                              )
                                                                                                      ),
                                                                                                      shiny::sliderInput(inputId = "phiSimInput", 
                                                                                                                         # label = "Weights of either quadratic entropy or entropy",
                                                                                                                         label = htmltools::p(i18n$t("Weights of either quadratic entropy or entropy"), 
                                                                                                                                              shiny::actionButton("phiSimInputInfo",
                                                                                                                                                                  label = "",
                                                                                                                                                                  icon = shiny::icon("info"),
                                                                                                                                                                  style = "padding:3px; font-size:60%")),
                                                                                                                         value = 1,
                                                                                                                         min = 0,
                                                                                                                         max = 1)
                                                                              )
                                                                ), # End column
                                                                shiny::column(width = 4, 
                                                                              shinyWidgets::actionBttn(inputId = "doSimulate",
                                                                                                       label = i18n$t("Simulate"),
                                                                                                       style = "fill",
                                                                                                       size = "lg",
                                                                                                       color = "success"),
                                                                              htmltools::br(),
                                                                              htmltools::br(),
                                                                              shiny::textOutput(outputId = "countScenariosText"),
                                                                              shiny::textOutput(outputId = "countSimulationText"),
                                                                              htmltools::br(),
                                                                              htmltools::br(),
                                                                              shinydashboardPlus::box(id = "box", width = 12, headerBorder = FALSE,
                                                                                                      title = i18n$t("Scenario management"), 
                                                                                                      collapsible = TRUE,
                                                                                                      collapsed = TRUE,
                                                                                                      # htmltools::h4(i18n$t("Merge")),
                                                                                                      # htmltools::hr(),
                                                                                                      shinydashboardPlus::box(id = "box", width = 12, headerBorder = FALSE,
                                                                                                                              title = i18n$t("Merge"), 
                                                                                                                              collapsible = FALSE,
                                                                                                                              collapsed = FALSE,
                                                                                                                              shiny::textInput(inputId = "mergeSimulateNameInput",
                                                                                                                                               label = i18n$t("Merged simulation name"),
                                                                                                                                               value = "Sim_Merged_1"),
                                                                                                                              shinyWidgets::pickerInput(inputId = "mergeSimulateInput",
                                                                                                                                                        label = i18n$t("Choose to merge"),
                                                                                                                                                        choices = NULL,
                                                                                                                                                        multiple = TRUE,
                                                                                                                                                        inline = FALSE
                                                                                                                              ),
                                                                                                                              shinyWidgets::actionBttn(inputId = "doSimulateMerge", 
                                                                                                                                                       label = i18n$t("Merge"),
                                                                                                                                                       style = "fill",
                                                                                                                                                       size = "md",
                                                                                                                                                       color = "success")
                                                                                                      ),
                                                                                                      # htmltools::br(),
                                                                                                      # htmltools::br(),
                                                                                                      # htmltools::br(),
                                                                                                      # htmltools::h4(i18n$t("Remove")),
                                                                                                      # htmltools::hr(),
                                                                                                      shinydashboardPlus::box(id = "box", width = 12, headerBorder = FALSE,
                                                                                                                              title = i18n$t("Remove"), 
                                                                                                                              collapsible = FALSE,
                                                                                                                              collapsed = FALSE,
                                                                                                                              shinyWidgets::pickerInput(inputId = "removeSimulateInput",
                                                                                                                                                        label = i18n$t("Choose to remove"),
                                                                                                                                                        choices = NULL,
                                                                                                                                                        multiple = TRUE,
                                                                                                                                                        inline = FALSE
                                                                                                                              ),
                                                                                                                              shinyWidgets::actionBttn(inputId = "doSimulateRemove", 
                                                                                                                                                       label = i18n$t("Remove"),
                                                                                                                                                       style = "fill",
                                                                                                                                                       size = "md",
                                                                                                                                                       color = "danger"
                                                                                                                              )
                                                                                                      )
                                                                              )
                                                                              # ) # end fixedPanel
                                                                ) # End column
                                                              ) # End row
                                              ),
                                              # shiny::tabPanel(i18n$t("Adjust Simulation"), 
                                              #                 shiny::fluidRow(
                                              #                   htmltools::br(),
                                              #                   shiny::column(width = 8,
                                              #                                 shinydashboardPlus::box(id = "box", width = 12, headerBorder = FALSE, 
                                              #                                                         title = i18n$t("Scenario"), 
                                              #                                                         collapsible = TRUE,
                                              #                                                         collapsed = FALSE,
                                              #                                                         shinyWidgets::pickerInput(inputId = "scenarioSimAdjInput",
                                              #                                                                                   label = i18n$t("Choose scenario"),
                                              #                                                                                   choices = NULL,
                                              #                                                                                   multiple = TRUE,
                                              #                                                                                   options = list("max-options" = 1),
                                              #                                                                                   inline = FALSE
                                              #                                                         )
                                              #                                 ),
                                              #                                 shinydashboardPlus::box(id = "box", width = 12, headerBorder = FALSE, 
                                              #                                                         title = i18n$t("Adjust options"), 
                                              #                                                         collapsible = TRUE,
                                              #                                                         collapsed = FALSE,
                                              #                                                         shiny::numericInput(inputId = "minAbuSliderSimAdjInput",
                                              #                                                                             # label = "Range of richness",
                                              #                                                                             label = htmltools::p(i18n$t("Minimal abundance"), 
                                              #                                                                                                  shiny::actionButton("minAbuSliderSimAdjInputInfo",
                                              #                                                                                                                      label = "",
                                              #                                                                                                                      icon = shiny::icon("info"),
                                              #                                                                                                                      style = "padding:3px; font-size:60%")),
                                              #                                                                             value = 0),
                                              #                                                         # shinyWidgets::sliderTextInput(inputId = "minAbuSliderSimAdjInput",
                                              #                                                         # 							  # label = "Range of richness",
                                              #                                                         # 							  label = htmltools::p(i18n$t("Minimal abundance"), 
                                              #                                                         # 							  					 shiny::actionButton("minAbuSliderSimAdjInputInfo",
                                              #                                                         # 							  					 					label = "",
                                              #                                                         # 							  					 					icon = shiny::icon("info"),
                                              #                                                         # 							  					 					style = "padding:3px; font-size:60%")),
                                              #                                                         # 							  choices = c(0, 1),
                                              #                                                         # 							  selected = 0
                                              #                                                         # ),
                                              #                                                         shiny::uiOutput("radioScenarioSimAdjOutput")
                                              #                                 )
                                              #                   ), # End column
                                              #                   shiny::column(width = 4,
                                              #                                 shinyWidgets::actionBttn(inputId = "doAdjustSim",
                                              #                                                          label = i18n$t("Adjust"),
                                              #                                                          style = "fill",
                                              #                                                          size = "lg",
                                              #                                                          color = "success"),
                                              #                   ) # End column
                                              #                 ) # End row
                                              # ), # End tabPanel
                                              shiny::tabPanel(i18n$t("Simulation Summary"), 
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 12,
                                                                              shinyWidgets::pickerInput(inputId = "scenarioSimulateSummaryInput",
                                                                                                        label = i18n$t("Select scenario"),
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
                            htmltools::h2(i18n$t("Compute parameters")),
                            shiny::fluidRow(
                              shiny::column(width = 12,
                                            shiny::tabsetPanel(
                                              shiny::tabPanel(i18n$t("Basic parameters"), 
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 8,
                                                                              shinydashboardPlus::box(id = "box", width = 12, headerBorder = FALSE, 
                                                                                                      title = i18n$t("Scenario"), 
                                                                                                      collapsible = TRUE,
                                                                                                      collapsed = FALSE,
                                                                                                      shinyWidgets::pickerInput(inputId = "scenarioComParInput",
                                                                                                                                label = i18n$t("Choose scenario"),
                                                                                                                                choices = NULL,
                                                                                                                                multiple = TRUE,
                                                                                                                                options = list("max-options" = 1),
                                                                                                                                inline = FALSE
                                                                                                      )
                                                                              ),
                                                                              shinydashboardPlus::box(id = "box", width = 12, headerBorder = FALSE, 
                                                                                                      title = i18n$t("Ecological indicators"), 
                                                                                                      collapsible = TRUE,
                                                                                                      collapsed = FALSE,
                                                                                                      shinyWidgets::pickerInput(inputId = "avaComInput",
                                                                                                                                # label = "Species availability",
                                                                                                                                label = htmltools::p(i18n$t("Species availability"), 
                                                                                                                                                     shiny::actionButton("avaComInputInfo",
                                                                                                                                                                         label = "",
                                                                                                                                                                         icon = shiny::icon("info"),
                                                                                                                                                                         style = "padding:3px; font-size:60%")),
                                                                                                                                choices = NULL,
                                                                                                                                multiple = TRUE,
                                                                                                                                options = list("max-options" = 1),
                                                                                                                                inline = FALSE
                                                                                                      ),
                                                                                                      shinyWidgets::pickerInput(inputId = "cwmComInput",
                                                                                                                                # label = "Traits to Community Weighted Mean",
                                                                                                                                label = htmltools::p(i18n$t("Traits to Community Weighted Mean"), 
                                                                                                                                                     shiny::actionButton("cwmComInputInfo",
                                                                                                                                                                         label = "",
                                                                                                                                                                         icon = shiny::icon("info"),
                                                                                                                                                                         style = "padding:3px; font-size:60%")),
                                                                                                                                choices = NULL,
                                                                                                                                multiple = TRUE,
                                                                                                                                options = list(`actions-box` = TRUE),
                                                                                                                                inline = FALSE
                                                                                                      ),
                                                                                                      shinyWidgets::pickerInput(inputId = "cwvComInput",
                                                                                                                                # label = "Traits to Community Weighted Variance",
                                                                                                                                label = htmltools::p(i18n$t("Traits to Community Weighted Variance"), 
                                                                                                                                                     shiny::actionButton("cwvComInputInfo",
                                                                                                                                                                         label = "",
                                                                                                                                                                         icon = shiny::icon("info"),
                                                                                                                                                                         style = "padding:3px; font-size:60%")),
                                                                                                                                choices = NULL,
                                                                                                                                multiple = TRUE,
                                                                                                                                options = list(`actions-box` = TRUE),
                                                                                                                                inline = FALSE
                                                                                                      ),
                                                                                                      shinyWidgets::pickerInput(inputId = "raoComInput",
                                                                                                                                # label = "Traits to Rao Quadratic Entropy",
                                                                                                                                label = htmltools::p(i18n$t("Traits to Rao Quadratic Entropy"), 
                                                                                                                                                     shiny::actionButton("raoComInputInfo",
                                                                                                                                                                         label = "",
                                                                                                                                                                         icon = shiny::icon("info"),
                                                                                                                                                                         style = "padding:3px; font-size:60%")),
                                                                                                                                choices = NULL,
                                                                                                                                multiple = TRUE,
                                                                                                                                options = list(`actions-box` = TRUE),
                                                                                                                                inline = FALSE
                                                                                                      ),
                                                                                                      shinyWidgets::pickerInput(inputId = "disComInput",
                                                                                                                                # label = "Traits to dissimilarity between reference sites",
                                                                                                                                label = htmltools::p(i18n$t("Traits to dissimilarity between reference sites"), 
                                                                                                                                                     shiny::actionButton("disComInputInfo",
                                                                                                                                                                         label = "",
                                                                                                                                                                         icon = shiny::icon("info"),
                                                                                                                                                                         style = "padding:3px; font-size:60%")),
                                                                                                                                choices = NULL,
                                                                                                                                multiple = TRUE,
                                                                                                                                options = list(`actions-box` = TRUE),
                                                                                                                                inline = FALSE
                                                                                                      )
                                                                              ),
                                                                              shinydashboardPlus::box(id = "box", width = 12, headerBorder = FALSE, 
                                                                                                      title = i18n$t("Economic indicators"), 
                                                                                                      collapsible = TRUE,
                                                                                                      collapsed = TRUE,
                                                                                                      shinyWidgets::pickerInput(inputId = "costComInput",
                                                                                                                                # label = "Cost per individual",
                                                                                                                                label = htmltools::p(i18n$t("Cost per individual"), 
                                                                                                                                                     shiny::actionButton("costComInputInfo",
                                                                                                                                                                         label = "",
                                                                                                                                                                         icon = shiny::icon("info"),
                                                                                                                                                                         style = "padding:3px; font-size:60%")),
                                                                                                                                choices = NULL,
                                                                                                                                multiple = TRUE,
                                                                                                                                options = list("max-options" = 1),
                                                                                                                                inline = FALSE
                                                                                                      ),
                                                                                                      shinyWidgets::pickerInput(inputId = "densComInput",
                                                                                                                                # label = "Species planting density",
                                                                                                                                label = htmltools::p(i18n$t("Species planting density"), 
                                                                                                                                                     shiny::actionButton("densComInputInfo",
                                                                                                                                                                         label = "",
                                                                                                                                                                         icon = shiny::icon("info"),
                                                                                                                                                                         style = "padding:3px; font-size:60%")),
                                                                                                                                choices = NULL,
                                                                                                                                multiple = TRUE,
                                                                                                                                options = list("max-options" = 1),
                                                                                                                                inline = FALSE
                                                                                                      )
                                                                              )
                                                                ), # End column
                                                                shiny::column(width = 4,
                                                                              shinyWidgets::actionBttn(inputId = "doCompute", 
                                                                                                       label = i18n$t("Compute"),
                                                                                                       style = "fill",
                                                                                                       size = "lg",
                                                                                                       color = "success")
                                                                ) # End column
                                                              ) # End row
                                              ),
                                              shiny::tabPanel(i18n$t("Standardise parameters"), 
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 8,
                                                                              shinydashboardPlus::box(id = "box", width = 12, headerBorder = FALSE, 
                                                                                                      title = i18n$t("Scenario"), 
                                                                                                      collapsible = TRUE,
                                                                                                      collapsed = FALSE,
                                                                                                      shinyWidgets::pickerInput(inputId = "scenarioComStandParInput",
                                                                                                                                label = i18n$t("Choose scenario"),
                                                                                                                                choices = NULL,
                                                                                                                                multiple = TRUE,
                                                                                                                                options = list("max-options" = 1),
                                                                                                                                inline = FALSE
                                                                                                      )
                                                                              ),
                                                                              shinydashboardPlus::box(id = "box", width = 12, headerBorder = FALSE, 
                                                                                                      title = i18n$t("Standardisation options"), 
                                                                                                      collapsible = TRUE,
                                                                                                      collapsed = FALSE,
                                                                                                      shinyWidgets::pickerInput(inputId = "stanComParInput",
                                                                                                                                # label = "Parameters to standardised",
                                                                                                                                label = htmltools::p(i18n$t("Parameters to standardised"), 
                                                                                                                                                     shiny::actionButton("stanComParInputInfo",
                                                                                                                                                                         label = "",
                                                                                                                                                                         icon = shiny::icon("info"),
                                                                                                                                                                         style = "padding:3px; font-size:60%")),
                                                                                                                                choices = NULL,
                                                                                                                                multiple = TRUE,
                                                                                                                                options = list(`actions-box` = TRUE),
                                                                                                                                inline = FALSE
                                                                                                      ),
                                                                                                      shiny::uiOutput("radioSpeficyMethodStanOutput")
                                                                                                      # shinyWidgets::prettyRadioButtons(inputId = "speficyMethodStanInput",
                                                                                                      # 								 # label = "Standardisation method",
                                                                                                      # 								 label = htmltools::p(i18n$t("Standardisation method"), 
                                                                                                      # 								 					 shiny::actionButton("speficyMethodStanInputInfo",
                                                                                                      # 								 					 					label = "",
                                                                                                      # 								 					 					icon = shiny::icon("info"),
                                                                                                      # 								 					 					style = "padding:3px; font-size:60%")),
                                                                                                      # 								 choices = stats::setNames(
                                                                                                      # 								 	c("max", "standardise"),
                                                                                                      # 								 	c("Maximum", "Standardise") # Set labels
                                                                                                      # 								 ),
                                                                                                      # 								 selected = "max",
                                                                                                      # 								 inline = TRUE,
                                                                                                      # 								 status = "primary"
                                                                                                      # )
                                                                              )
                                                                ),
                                                                shiny::column(width = 4,
                                                                              shinyWidgets::actionBttn(inputId = "doStandardise", 
                                                                                                       label = i18n$t("Standardise"),
                                                                                                       style = "fill",
                                                                                                       size = "lg",
                                                                                                       color = "success")
                                                                )
                                                              ) # End row
                                              ), # End tabPanel
                                              shiny::tabPanel(i18n$t("Multifunctionality"), 
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 8,
                                                                              shinydashboardPlus::box(id = "box", width = 12, headerBorder = FALSE, 
                                                                                                      title = i18n$t("Scenario"), 
                                                                                                      collapsible = TRUE,
                                                                                                      collapsed = FALSE,
                                                                                                      shinyWidgets::pickerInput(inputId = "scenarioComMultiInput",
                                                                                                                                label = i18n$t("Choose scenario"),
                                                                                                                                choices = NULL,
                                                                                                                                multiple = TRUE,
                                                                                                                                options = list("max-options" = 1),
                                                                                                                                inline = FALSE
                                                                                                      )
                                                                              ),
                                                                              shinydashboardPlus::box(id = "box", width = 12, headerBorder = FALSE, 
                                                                                                      title = i18n$t("Multifunctionality options"), 
                                                                                                      collapsible = TRUE,
                                                                                                      collapsed = FALSE,
                                                                                                      shinyWidgets::pickerInput(inputId = "testsMultiInput",
                                                                                                                                # label = "Parameters to multifunctionality",
                                                                                                                                label = htmltools::p(i18n$t("Parameters to multifunctionality"), 
                                                                                                                                                     shiny::actionButton("testsMultiInputInfo",
                                                                                                                                                                         label = "",
                                                                                                                                                                         icon = shiny::icon("info"),
                                                                                                                                                                         style = "padding:3px; font-size:60%")),
                                                                                                                                choices = NULL,
                                                                                                                                multiple = TRUE,
                                                                                                                                options = list(`actions-box` = TRUE),
                                                                                                                                inline = FALSE
                                                                                                      ),
                                                                                                      shiny::conditionalPanel(condition = "output.showSlidersMulti == true",
                                                                                                                              shiny::uiOutput("slidersMulti"),
                                                                                                                              htmltools::br(),
                                                                                                      )
                                                                              ),
                                                                ), # End column
                                                                shiny::column(width = 4,
                                                                              shinyWidgets::actionBttn(inputId = "doMultiCompute", 
                                                                                                       label = i18n$t("Compute"),
                                                                                                       style = "fill",
                                                                                                       size = "lg",
                                                                                                       color = "success")
                                                                ) # End column
                                                              ) # End row
                                              ),
                                              shiny::tabPanel(i18n$t("Simulation Summary"), 
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 12,
                                                                              shinyWidgets::pickerInput(inputId = "scenarioComputeSummaryInput",
                                                                                                        label = i18n$t("Select scenario"),
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
                            htmltools::h2(i18n$t("Select communities")),
                            shiny::fluidRow(
                              shiny::column(width = 12,
                                            shiny::tabsetPanel(
                                              shiny::tabPanel(i18n$t("Select"), 
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 8,
                                                                              shinydashboardPlus::box(id = "box", width = 12, headerBorder = FALSE, 
                                                                                                      title = i18n$t("Scenario"), 
                                                                                                      collapsible = TRUE,
                                                                                                      collapsed = FALSE,
                                                                                                      shinyWidgets::pickerInput(inputId = "scenarioSelInput",
                                                                                                                                label = i18n$t("Choose scenario"),
                                                                                                                                choices = NULL,
                                                                                                                                multiple = TRUE,
                                                                                                                                options = list("max-options" = 1),
                                                                                                                                inline = FALSE
                                                                                                      ),
                                                                                                      shiny::textInput(inputId = "prefixSelInput",
                                                                                                                       label = i18n$t("Selection name"),
                                                                                                                       value = "Sel_1"
                                                                                                      )
                                                                              ),
                                                                              shinydashboardPlus::box(id = "box", width = 12, headerBorder = FALSE, 
                                                                                                      title = i18n$t("Selection options"), 
                                                                                                      collapsible = TRUE,
                                                                                                      collapsed = FALSE,
                                                                                                      shinyWidgets::pickerInput(inputId = "testsPrioritySelInput",
                                                                                                                                # label = "Parameters to filter selection",
                                                                                                                                label = htmltools::p(i18n$t("Parameters to filter selection"), 
                                                                                                                                                     shiny::actionButton("testsPrioritySelInputInfo",
                                                                                                                                                                         label = "",
                                                                                                                                                                         icon = shiny::icon("info"),
                                                                                                                                                                         style = "padding:3px; font-size:60%")),
                                                                                                                                choices = NULL,
                                                                                                                                multiple = TRUE,
                                                                                                                                options = list(`actions-box` = TRUE),
                                                                                                                                inline = FALSE
                                                                                                      ),
                                                                                                      shiny::conditionalPanel(condition = "output.showSlidersTestsPrioritySel == true",
                                                                                                                              shiny::uiOutput("slidersTestsPrioritySel"),
                                                                                                                              htmltools::br(),
                                                                                                                              htmltools::br()
                                                                                                      ),
                                                                                                      shinyWidgets::pickerInput(inputId = "testsPrioritySelInput",
                                                                                                                                # label = "Parameters to priority selection",
                                                                                                                                label = htmltools::p(i18n$t("Parameters to priority selection"), 
                                                                                                                                                     shiny::actionButton("testsPrioritySelInputInfo",
                                                                                                                                                                         label = "",
                                                                                                                                                                         icon = shiny::icon("info"),
                                                                                                                                                                         style = "padding:3px; font-size:60%")),
                                                                                                                                choices = NULL,
                                                                                                                                multiple = TRUE,
                                                                                                                                options = list(`actions-box` = TRUE),
                                                                                                                                inline = FALSE
                                                                                                      ),
                                                                                                      shiny::uiOutput(outputId = "outputRankList"),
                                                                                                      shiny::conditionalPanel(condition = "output.showSlidersTestsPrioritySel == true",
                                                                                                                              shiny::uiOutput("slidersTestsPrioritySel"),
                                                                                                                              htmltools::br(),
                                                                                                      ),
                                                                                                      htmltools::br(),
                                                                                                      shiny::uiOutput("radioSpeficyGroupsSelOutput"),
                                                                                                      # shinyWidgets::prettyRadioButtons(inputId = "speficyGroupsSelInput",
                                                                                                      # 								 # label = "Selection inside sites groups",
                                                                                                      # 								 label = htmltools::p(i18n$t("Selection inside sites groups"), 
                                                                                                      # 								 					 shiny::actionButton("speficyGroupsSelInputInfo",
                                                                                                      # 								 					 					label = "",
                                                                                                      # 								 					 					icon = shiny::icon("info"),
                                                                                                      # 								 					 					style = "padding:3px; font-size:60%")),
                                                                                                      # 								 choices = stats::setNames(
                                                                                                      # 								 	c("Yes", "No"),
                                                                                                      # 								 	c("Yes", "No") # Set labels
                                                                                                      # 								 ),
                                                                                                      # 								 selected = "No",
                                                                                                      # 								 inline = TRUE,
                                                                                                      # 								 status = "primary"
                                                                                                      # ),
                                                                                                      shiny::conditionalPanel(condition = "(input.speficyGroupsSelInput == 'Yes')",
                                                                                                                              shinyWidgets::pickerInput(inputId = "groupSelInput",
                                                                                                                                                        label = i18n$t("Species groups"),
                                                                                                                                                        choices = NULL,
                                                                                                                                                        multiple = TRUE,
                                                                                                                                                        options = list("max-options" = 1),
                                                                                                                                                        inline = FALSE
                                                                                                                              )
                                                                                                      ),
                                                                                                      shiny::uiOutput("radioSingleSelectionSelOutput")
                                                                              )
                                                                              # shinyWidgets::prettyRadioButtons(inputId = "singleSelectionInput",
                                                                              # 								 # label = "Selection method",
                                                                              # 								 label = htmltools::p(i18n$t("Selection method"), 
                                                                              # 								 					 shiny::actionButton("singleSelectionInputInfo",
                                                                              # 								 					 					label = "",
                                                                              # 								 					 					icon = shiny::icon("info"),
                                                                              # 								 					 					style = "padding:3px; font-size:60%")),
                                                                              # 								 choices = stats::setNames(
                                                                              # 								 	c(TRUE, FALSE),
                                                                              # 								 	c("Single", "Multiple") # Set labels
                                                                              # 								 ),
                                                                              # 								 selected = TRUE,
                                                                              # 								 inline = TRUE,
                                                                              # 								 status = "primary"
                                                                              # )
                                                                ),
                                                                shiny::column(width = 4,
                                                                              shinyWidgets::actionBttn(inputId = "doSelect", 
                                                                                                       label = i18n$t("Select"),
                                                                                                       style = "fill",
                                                                                                       size = "lg",
                                                                                                       color = "success"),
                                                                              htmltools::br(),
                                                                              htmltools::br(),
                                                                              shiny::textOutput(outputId = "countSelectText"),
                                                                              shiny::textOutput(outputId = "countSimulationSelText"),
                                                                              htmltools::br(),
                                                                              htmltools::br(),
                                                                              # htmltools::h4(i18n$t("Merge")),
                                                                              # htmltools::hr(),
                                                                              shinydashboardPlus::box(id = "box", width = 12, headerBorder = FALSE,
                                                                                                      title = i18n$t("Scenario management"), 
                                                                                                      collapsible = TRUE,
                                                                                                      collapsed = TRUE,
                                                                                                      shinydashboardPlus::box(id = "box", width = 12, headerBorder = FALSE,
                                                                                                                              title = i18n$t("Merge"), 
                                                                                                                              collapsible = FALSE,
                                                                                                                              collapsed = FALSE,
                                                                                                                              shiny::textInput(inputId = "mergeSelectNameInput",
                                                                                                                                               label = i18n$t("Merged selection name"),
                                                                                                                                               value = "Sel_Merged_1"),
                                                                                                                              shinyWidgets::pickerInput(inputId = "mergeSelectInput",
                                                                                                                                                        label = i18n$t("Choose to merge"),
                                                                                                                                                        choices = NULL,
                                                                                                                                                        multiple = TRUE,
                                                                                                                                                        inline = FALSE
                                                                                                                              ),
                                                                                                                              shinyWidgets::actionBttn(inputId = "doSelectMerge", 
                                                                                                                                                       label = i18n$t("Merge"),
                                                                                                                                                       style = "fill",
                                                                                                                                                       size = "md",
                                                                                                                                                       color = "success")
                                                                                                      ),
                                                                                                      # htmltools::br(),
                                                                                                      # htmltools::br(),
                                                                                                      # htmltools::br(),
                                                                                                      # htmltools::h4(i18n$t("Remove")),
                                                                                                      # htmltools::hr(),
                                                                                                      shinydashboardPlus::box(id = "box", width = 12, headerBorder = FALSE,
                                                                                                                              title = i18n$t("Remove"), 
                                                                                                                              collapsible = FALSE,
                                                                                                                              collapsed = FALSE,
                                                                                                                              shinyWidgets::pickerInput(inputId = "removeSelectInput",
                                                                                                                                                        label = i18n$t("Choose to remove"),
                                                                                                                                                        choices = NULL,
                                                                                                                                                        multiple = TRUE,
                                                                                                                                                        inline = FALSE
                                                                                                                              ),
                                                                                                                              shinyWidgets::actionBttn(inputId = "doSelectRemove", 
                                                                                                                                                       label = i18n$t("Remove"),
                                                                                                                                                       style = "fill",
                                                                                                                                                       size = "md",
                                                                                                                                                       color = "danger"
                                                                                                                              )
                                                                                                      )
                                                                              )
                                                                ) # End column
                                                              ) # End row
                                              ), # End tabPanel
                                              shiny::tabPanel(i18n$t("Select Summary"), 
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 12,
                                                                              shinyWidgets::pickerInput(inputId = "scenarioSelectSummaryInput",
                                                                                                        label = i18n$t("Select scenario"),
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
                            htmltools::h2(i18n$t("View results")),
                            shiny::fluidRow(
                              shiny::column(width = 12,
                                            shiny::tabsetPanel(
                                              shiny::tabPanel(i18n$t("Parameters"), 
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 4,
                                                                              shinydashboardPlus::box(id = "boxNH", width = 12, title = NULL, headerBorder = FALSE,
                                                                                                      shiny::uiOutput("radioScenarioTypeViewParOutput"),
                                                                                                      # shinyWidgets::prettyRadioButtons(inputId = "scenarioTypeViewParInput",
                                                                                                      # 								 label = i18n$t("Scenario type"),
                                                                                                      # 								 choices = stats::setNames(
                                                                                                      # 								 	c("Raw", "Selected"),
                                                                                                      # 								 	c("Raw", "Selected") # Set labels
                                                                                                      # 								 ),
                                                                                                      # 								 selected = "Raw",
                                                                                                      # 								 inline = TRUE,
                                                                                                      # 								 status = "primary"
                                                                                                      # ),
                                                                                                      shinyWidgets::pickerInput(inputId = "scenarioViewParInput",
                                                                                                                                label = i18n$t("Choose scenario"),
                                                                                                                                choices = NULL,
                                                                                                                                multiple = TRUE,
                                                                                                                                options = list("max-options" = 1),
                                                                                                                                inline = FALSE
                                                                                                      ),
                                                                                                      shinyWidgets::pickerInput(inputId = "xvarViewInput",
                                                                                                                                label = i18n$t("Variable to x-axis"),
                                                                                                                                choices = NULL,
                                                                                                                                multiple = TRUE,
                                                                                                                                options = list("max-options" = 1),
                                                                                                                                inline = FALSE
                                                                                                      ),
                                                                                                      shinyWidgets::pickerInput(inputId = "yvarViewInput",
                                                                                                                                label = i18n$t("Variable to y-axis"),
                                                                                                                                choices = NULL,
                                                                                                                                multiple = TRUE,
                                                                                                                                options = list("max-options" = 1),
                                                                                                                                inline = FALSE
                                                                                                      ),
                                                                                                      shiny::uiOutput("radioShowRefViewParOutput"),
                                                                                                      # shinyWidgets::prettyRadioButtons(inputId = "hideRefViewParInput",
                                                                                                      # 								 label = i18n$t("Hide reference sites"),
                                                                                                      # 								 choices = stats::setNames(
                                                                                                      # 								 	c(TRUE, FALSE),
                                                                                                      # 								 	c("Yes", "No") # Set labels
                                                                                                      # 								 ),
                                                                                                      # 								 selected = FALSE,
                                                                                                      # 								 inline = TRUE,
                                                                                                      # 								 status = "primary"
                                                                                                      # ),
                                                                                                      # hr(),
                                                                                                      shiny::textInput(inputId = "xvarLab", 
                                                                                                                       label = i18n$t("Label to x-axis"), 
                                                                                                                       value = ""),
                                                                                                      shiny::textInput(inputId = "yvarLab", 
                                                                                                                       label = i18n$t("Label to y-axis"), 
                                                                                                                       value = ""),
                                                                                                      shinyWidgets::actionBttn(inputId = "doPlotPar", 
                                                                                                                               label = i18n$t("Plot"),
                                                                                                                               style = "fill",
                                                                                                                               size = "md",
                                                                                                                               color = "success"),
                                                                                                      shinyWidgets::actionBttn(inputId = "doPlotParClear", 
                                                                                                                               label = i18n$t("Clear"),
                                                                                                                               style = "fill",
                                                                                                                               size = "md",
                                                                                                                               color = "default")
                                                                              )
                                                                ), # End column
                                                                shiny::column(width = 8,
                                                                              shiny::plotOutput("plotParOutput"),
                                                                              htmltools::br(),
                                                                              shinyWidgets::downloadBttn(outputId = "doDownloadParPlot",
                                                                                                         label = i18n$t("Download"),
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
                                                                              shinydashboardPlus::box(id = "boxNH", width = 12, title = NULL, headerBorder = FALSE,
                                                                                                      shiny::uiOutput("radioScenarioTypeViewMultiOutput"),
                                                                                                      # shinyWidgets::prettyRadioButtons(inputId = "scenarioTypeViewMultiInput",
                                                                                                      # 								 label = i18n$t("Scenario type"),
                                                                                                      # 								 choices = stats::setNames(
                                                                                                      # 								 	c("Raw", "Selected"),
                                                                                                      # 								 	c("Raw", "Selected") # Set labels
                                                                                                      # 								 ),
                                                                                                      # 								 selected = "Raw",
                                                                                                      # 								 inline = TRUE,
                                                                                                      # 								 status = "primary"
                                                                                                      # ),
                                                                                                      shinyWidgets::pickerInput(inputId = "scenarioViewMultiInput",
                                                                                                                                label = i18n$t("Choose scenario"),
                                                                                                                                choices = NULL,
                                                                                                                                multiple = TRUE,
                                                                                                                                options = list("max-options" = 1),
                                                                                                                                inline = FALSE
                                                                                                      ),
                                                                                                      shiny::uiOutput("radioShowRefViewMultiOutput"),
                                                                                                      # shinyWidgets::prettyRadioButtons(inputId = "hideRefViewMultiInput",
                                                                                                      # 								 label = i18n$t("Hide reference sites"),
                                                                                                      # 								 choices = stats::setNames(
                                                                                                      # 								 	c(TRUE, FALSE),
                                                                                                      # 								 	c("Yes", "No") # Set labels
                                                                                                      # 								 ),
                                                                                                      # 								 selected = FALSE,
                                                                                                      # 								 inline = TRUE,
                                                                                                      # 								 status = "primary"
                                                                                                      # ),
                                                                                                      shiny::uiOutput("labelMultiOutput"),
                                                                                                      shinyWidgets::actionBttn(inputId = "doPlotMulti", 
                                                                                                                               label = i18n$t("Plot"),
                                                                                                                               style = "fill",
                                                                                                                               size = "md",
                                                                                                                               color = "success"),
                                                                                                      shinyWidgets::actionBttn(inputId = "doPlotMultiClear", 
                                                                                                                               label = i18n$t("Clear"),
                                                                                                                               style = "fill",
                                                                                                                               size = "md",
                                                                                                                               color = "default")
                                                                              )
                                                                ), # End column
                                                                shiny::column(width = 8,
                                                                              shiny::plotOutput("plotMultiOutput"),
                                                                              htmltools::br(),
                                                                              shinyWidgets::downloadBttn(outputId = "doDownloadMultiPlot",
                                                                                                         label = i18n$t("Download"),
                                                                                                         style = "fill",
                                                                                                         icon = NULL,
                                                                                                         size = "md",
                                                                                                         color = "success")
                                                                ) # End column
                                                              ) # End row
                                              ), # End tabPanel
                                              shiny::tabPanel(i18n$t("Export"), 
                                                              shiny::fluidRow(
                                                                htmltools::br(),
                                                                shiny::column(width = 4,
                                                                              shinydashboardPlus::box(id = "boxNH", width = 12, title = NULL, headerBorder = FALSE,
                                                                                                      shiny::uiOutput("radioScenarioTypeExportOutput"),
                                                                                                      # shinyWidgets::prettyRadioButtons(inputId = "scenarioTypeExportInput",
                                                                                                      # 								 label = i18n$t("Scenario type"),
                                                                                                      # 								 choices = stats::setNames(
                                                                                                      # 								 	c("Raw", "Selected"),
                                                                                                      # 								 	c("Raw", "Selected") # Set labels
                                                                                                      # 								 ),
                                                                                                      # 								 selected = "Raw",
                                                                                                      # 								 inline = TRUE,
                                                                                                      # 								 status = "primary"
                                                                                                      # ),
                                                                                                      shinyWidgets::pickerInput(inputId = "scenarioExportInput",
                                                                                                                                label = i18n$t("Choose scenario"),
                                                                                                                                choices = NULL,
                                                                                                                                multiple = TRUE,
                                                                                                                                options = list("max-options" = 1),
                                                                                                                                inline = FALSE
                                                                                                      ),
                                                                                                      shiny::uiOutput("pickerTypeExportOutput"),
                                                                                                      # shinyWidgets::pickerInput(inputId = "typeExportInput",
                                                                                                      # 						  label = i18n$t("Type of result"),
                                                                                                      # 						  choices = stats::setNames(
                                                                                                      # 						  	c("simComposition",
                                                                                                      # 						  	  "simResults", 
                                                                                                      # 						  	  "simMultifunctionality",
                                                                                                      # 						  	  "simUnavailableSpecies",
                                                                                                      # 						  	  "refComposition",
                                                                                                      # 						  	  "refResults",
                                                                                                      # 						  	  "refMultifunctionality", 
                                                                                                      # 						  	  "supComposition", 
                                                                                                      # 						  	  "supResults",
                                                                                                      # 						  	  "supMultifunctionality"),
                                                                                                      # 						  	c("simComposition",
                                                                                                      # 						  	  "simResults", 
                                                                                                      # 						  	  "simMultifunctionality",
                                                                                                      # 						  	  "simUnavailableSpecies",
                                                                                                      # 						  	  "refComposition",
                                                                                                      # 						  	  "refResults",
                                                                                                      # 						  	  "refMultifunctionality", 
                                                                                                      # 						  	  "supComposition", 
                                                                                                      # 						  	  "supResults",
                                                                                                      # 						  	  "supMultifunctionality") # Set labels
                                                                                                      # 						  ),
                                                                                                      # 						  multiple = TRUE,
                                                                                                      # 						  options = list("max-options" = 1),
                                                                                                      # 						  inline = FALSE
                                                                                                      # ),
                                                                                                      shiny::conditionalPanel(condition = "(input.typeExportInput == 'simUnavailableSpecies')",
                                                                                                                              shinyWidgets::pickerInput(inputId = "avaExpInput",
                                                                                                                                                        label = i18n$t("Species availability"),
                                                                                                                                                        choices = NULL,
                                                                                                                                                        multiple = TRUE,
                                                                                                                                                        options = list("max-options" = 1),
                                                                                                                                                        inline = FALSE
                                                                                                                              )
                                                                                                      ),
                                                                                                      shiny::conditionalPanel(condition = "(input.typeExportInput == 'simComposition') ||
												  									  						(input.typeExportInput == 'simBaseline') ||
												  									  						(input.typeExportInput == 'simAdditions') ||
												  									  						(input.typeExportInput == 'refComposition') ||
												  									  						(input.typeExportInput == 'supComposition')",
                                                                                                                              shiny::uiOutput("radiodbFormatExpOutput")
                                                                                                                              # shinyWidgets::prettyRadioButtons(inputId = "dbFormatExpInput",
                                                                                                                              # 								 label = i18n$t("Data base format"),
                                                                                                                              # 								 choices = stats::setNames(
                                                                                                                              # 								 	c(TRUE, FALSE),
                                                                                                                              # 								 	c("Yes", "No") # Set labels
                                                                                                                              # 								 ),
                                                                                                                              # 								 selected = FALSE,
                                                                                                                              # 								 inline = TRUE,
                                                                                                                              # 								 status = "primary"
                                                                                                                              # )
                                                                                                      ),
                                                                                                      shinyWidgets::actionBttn(inputId = "doExport",
                                                                                                                               label = i18n$t("Preview"),
                                                                                                                               style = "fill",
                                                                                                                               size = "md",
                                                                                                                               color = "success")
                                                                              )
                                                                ), # End column
                                                                shiny::column(width = 8,
                                                                              rhandsontable::rHandsontableOutput("outputExportTable"),
                                                                              htmltools::br(),
                                                                              shinyWidgets::downloadBttn(outputId = "doDownloadExport",
                                                                                                         label = i18n$t("Download"),
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
    ) # End viewTab
    #### exportTab
    # shinydashboard::tabItem(tabName = "exportTab",
    # 						h2("Export results"),
    # 						fluidRow(
    # 							shiny::column(width = 12,
    # 										  shiny::tabsetPanel(
    # 										  	shiny::tabPanel("Raw", 
    # 										  					shiny::fluidRow(
    # 										  						htmltools::br(),
    # 										  						shiny::column(width = 4,
    # 										  									  # htmltools::h4("Set basic parameters"),
    # 										  									  # hr(),
    # 										  									  # shinyDirButton(id = "choseDirectory", label = "Chose directory", title = "Chose directory", class = "action-button bttn bttn-fill bttn-md bttn-default bttn-no-outline shiny-bound-input"),
    # 										  									  
    # 										  						), # End column
    # 										  						shiny::column(width = 8,
    # 										  									  # htmltools::h4("Select"),
    # 										  									  # hr()
    # 										  									  shiny::sliderInput("bins3", 
    # 										  									  				   label = htmltools::p(i18n$t("Hello Shiny!"), 
    # 										  									  				   					 shiny::actionButton("titleBtId", 
    # 										  									  				   					 					label = "", 
    # 										  									  				   					 					icon = shiny::icon("info"), 
    # 										  									  				   					 					style = "padding:4px; font-size:60%")), 
    # 										  									  				   min = 1, max = 50, value = 30)
    # 										  						) # End column
    # 										  					) # End row
    # 										  	) # End tabPanel
    # 										  ) # End tabsetPanel
    # 							) # End column
    # 						) # End row
    # ) # End exportTab
  ) # End tabItems
)


## Page UI ----
#' @rdname app
#' @export
appUI <- shinydashboardPlus::dashboardPage(header = header,
                                           sidebar = sidebar,
                                           body = body,
                                           controlbar = controlbar,
                                           skin = "green",
                                           md = FALSE,
                                           scrollToTop = TRUE
)
