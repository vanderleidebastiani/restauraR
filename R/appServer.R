#' @rdname app
#' @export
appServer <- shiny::shinyServer(function(input, output, session) {
  # Input parameters to simulateCommunities
  # output$showData <- FALSE
  # output$showData <- reactive({
  # 	ifelse(
  # 		!is.null(input$traits), 
  # 		TRUE,
  # 		FALSE
  # 	)
  # })
  # outputOptions(output, "showData", suspendWhenHidden = FALSE)
  
  ## Reactive Values ----
  ### Global variables ----
  globalRV <- reactiveValues(digitsVal = 5, 
                             digitsMin = 3,
                             currentDate = format(Sys.Date(), "%Y%m%d"),
                             countPlots = 0,
                             probs = c(0, 0.25, 0.5, 0.75, 1))
  #### Set the minimal decimal places ----
  numVal <- reactive({
    if(!is.null(input$decimalPlaces)){
      if(is.na(input$decimalPlaces)){
        return(globalRV$digitsMin)
      }
      if(input$decimalPlaces < globalRV$digitsMin) {
        return(globalRV$digitsMin)
      }
      return(input$decimalPlaces)
    } else{
      return(globalRV$digitsVal)
    }
  })
  output$decimalPlaces <- renderUI(numericInput(inputId = "decimalPlaces", 
                                                label = "Decimal places", 
                                                min = globalRV$digitsMin, 
                                                value = numVal()))
  ### inputDataRV ----
  inputDataRV <- shiny::reactiveValues(traits = NULL,
                                       restComp = NULL,
                                       restGroup = NULL,
                                       reference = NULL,
                                       supplementary = NULL,
                                       auxTraitsClass = NULL,
                                       auxTraitsVariables = NULL)
  ### inputParSimRV ----
  inputParSimRV <- shiny::reactiveValues(# ava = NULL, # Ok
    restComp = NULL, # Ok
    restGroup = NULL, # Ok
    # und = NULL, # Ok
    it = NULL, # Ok
    # richMin = NULL, # Removed
    # richMax = NULL, # Removed
    # rich = NULL, # Ok
    # cwm = NULL, # Ok
    # rao = NULL, # Ok
    # prob = NULL,
    # phi = NULL, # Ok
    nInd = NULL, # Ok
    cvAbund = NULL, # Ok
    prefix = NULL, # Ok
    # method = NULL, # Ok
    group = NULL, # Ok
    probGroupRich = NULL, # Ok
    probGroupAbund = NULL # Ok
  )
  # inputParComRV <- shiny::reactiveValues(ava = NULL,
  # 									   cwm = NULL,
  # 									   cwv = NULL,
  # 									   rao = NULL,
  # 									   cost = NULL,
  # 									   dens = NULL,
  # 									   stan = NULL)
  ### inputParSelRV ----
  inputParSelRV <- shiny::reactiveValues(testsDet = NULL,
                                         testsHie = NULL,
                                         group = NULL,
                                         # singleselection = NULL,
                                         auxRankHeiSel = NULL)
  ### resultsRV ----
  resultsRV <- shiny::reactiveValues(nSce = 0, # Ok
                                     nSim = 0, # Ok
                                     simulate = list(), # Ok
                                     nSel = 0, # Ok
                                     nSimSel = 0, # Ok
                                     select = list(), # Ok
                                     plotPar = NULL, # Ok
                                     plotMulti = NULL,  # Ok
                                     updatePar = 0 # Ok
  )
  ### exportRV ----
  exportRV <- shiny::reactiveValues(dbFormat = NULL,
                                    table = NULL,
                                    summaryTable = NULL
  )
  ## Input file ----
  ### Input file - Traits data ----
  observeEvent(input$traitsInput, {
    # Read file
    inFile <- input$traitsInput
    if (is.null(inFile)){
      inputDataRV$traits <- NULL
    }
    inputDataRV$traits <- read.csv(inFile$datapath, sep = input$fileSep, row.names = 1)
  }) # End input file
  ### Input file - restComp ----
  observeEvent(input$restCompInput, {
    # Read file
    inFile <- input$restCompInput
    if (is.null(inFile)){
      inputDataRV$restComp <- NULL
    }
    inputDataRV$restComp <- read.csv(inFile$datapath, sep = input$fileSep, row.names = 1)
  }) # End input file
  ### Input file - restGroup ----
  observeEvent(input$restGroupInput, {
    # Read file
    inFile <- input$restGroupInput
    if (is.null(inFile)){
      inputDataRV$restGroup <- NULL
    }
    inputDataRV$restGroup <- read.csv(inFile$datapath, sep = input$fileSep, row.names = 1)
  }) # End input file
  ### Input file - Reference ----
  observeEvent(input$referenceInput, {
    # Read file
    inFile <- input$referenceInput
    if (is.null(inFile)){
      inputDataRV$reference <- NULL
    }
    inputDataRV$reference <- read.csv(inFile$datapath, sep = input$fileSep, row.names = 1)
  }) # End input file
  ### Input file - Supplementary ----
  observeEvent(input$supplementaryInput, {
    # Read file
    inFile <- input$supplementaryInput
    if (is.null(inFile)){
      inputDataRV$supplementary <- NULL
    }
    inputDataRV$supplementary <- read.csv(inFile$datapath, sep = input$fileSep, row.names = 1)
  }) # End input file
  ## doClear buttons ----
  ### doClear CONTINUAR ----
  # remover tambem outros elementos da interface
  observeEvent(input$doClear, {
    inputDataRV$traits <- NULL
    inputDataRV$restComp <- NULL
    inputDataRV$restGroup <- NULL
    inputDataRV$reference <- NULL
    inputDataRV$supplementary <- NULL
    inputDataRV$auxTraitsClass <- NULL
    inputDataRV$auxTraitsVariables <- NULL
    shinyjs::reset("traitsInput")
    shinyjs::reset("restCompInput")
    shinyjs::reset("restGroupInput")
    shinyjs::reset("referenceInput")
    shinyjs::reset("supplementaryInput")
  })
  ### doClearTraits ----
  observeEvent(input$doClearTraits, {
    inputDataRV$traits <- NULL
    inputDataRV$auxTraitsClass <- NULL
    inputDataRV$auxTraitsVariables <- NULL
    shinyjs::reset("traitsInput")
  })
  ### doClearRestComp ----
  observeEvent(input$doClearRestComp, {
    inputDataRV$restComp <- NULL
    shinyjs::reset("restCompInput")
  })
  ### doClearRestGroup ----
  observeEvent(input$doClearRestGroup, {
    inputDataRV$restGroup <- NULL
    shinyjs::reset("restGroupInput")
  })
  ### doClearReference ----
  observeEvent(input$doClearReference, {
    inputDataRV$reference <- NULL
    shinyjs::reset("referenceInput")
  })
  ### doClearSupplementary ----
  observeEvent(input$doClearSupplementary, {
    inputDataRV$supplementary <- NULL
    shinyjs::reset("supplementaryInput")
  })
  ## Update pickers - Traits input ----
  observeEvent(inputDataRV$traits, ignoreNULL = FALSE, {
    if(!is.null(inputDataRV$traits)){
      # Extract basic data information
      inputDataRV$auxTraitsVariables <- colnames(inputDataRV$traits)
      inputDataRV$auxTraitsClass <- data.frame(t(sapply(inputDataRV$traits, vectorClass)), row.names = "Class")
      # Update slider
      updateSliderTextInput(session, inputId = "richSliderSimInput",
                            choices = seq_len(nrow(inputDataRV$traits)),
                            selected = c(1, nrow(inputDataRV$traits)))
    } else{
      inputDataRV$auxTraitsVariables <- character(0)
      inputDataRV$auxTraitsClass <- character(0)
      # Update slider
      updateSliderTextInput(session, inputId = "richSliderSimInput",
                            choices = c(1, 1),
                            selected = c(1, 1))
    }
    # Update pickers
    updatePickerInput(session, inputId = "avaSimInput", choices = inputDataRV$auxTraitsVariables,
                      choicesOpt = list(subtext = inputDataRV$auxTraitsClass))
    updatePickerInput(session, inputId = "undSimInput", choices = inputDataRV$auxTraitsVariables,
                      choicesOpt = list(subtext = inputDataRV$auxTraitsClass))
    updatePickerInput(session, inputId = "cwmSimInput", choices = inputDataRV$auxTraitsVariables,
                      choicesOpt = list(subtext = inputDataRV$auxTraitsClass))
    updatePickerInput(session, inputId = "raoSimInput", choices = inputDataRV$auxTraitsVariables,
                      choicesOpt = list(subtext = inputDataRV$auxTraitsClass))
    updatePickerInput(session, inputId = "probSimInput", choices = inputDataRV$auxTraitsVariables,
                      choicesOpt = list(subtext = inputDataRV$auxTraitsClass))
    updatePickerInput(session, inputId = "groupSimInput", choices = inputDataRV$auxTraitsVariables,
                      choicesOpt = list(subtext = inputDataRV$auxTraitsClass))
    updatePickerInput(session, inputId = "avaComInput", choices = inputDataRV$auxTraitsVariables,
                      choicesOpt = list(subtext = inputDataRV$auxTraitsClass))
    updatePickerInput(session, inputId = "cwmComInput", choices = inputDataRV$auxTraitsVariables,
                      choicesOpt = list(subtext = inputDataRV$auxTraitsClass))
    updatePickerInput(session, inputId = "cwvComInput", choices = inputDataRV$auxTraitsVariables,
                      choicesOpt = list(subtext = inputDataRV$auxTraitsClass))
    updatePickerInput(session, inputId = "raoComInput", choices = inputDataRV$auxTraitsVariables,
                      choicesOpt = list(subtext = inputDataRV$auxTraitsClass))
    updatePickerInput(session, inputId = "costComInput", choices = inputDataRV$auxTraitsVariables,
                      choicesOpt = list(subtext = inputDataRV$auxTraitsClass))
    updatePickerInput(session, inputId = "densComInput", choices = inputDataRV$auxTraitsVariables,
                      choicesOpt = list(subtext = inputDataRV$auxTraitsClass))
    updatePickerInput(session, inputId = "disComInput", choices = inputDataRV$auxTraitsVariables,
                      choicesOpt = list(subtext = inputDataRV$auxTraitsClass))
    updatePickerInput(session, inputId = "avaExpInput", choices = inputDataRV$auxTraitsVariables,
                      choicesOpt = list(subtext = inputDataRV$auxTraitsClass))
  })
  ## Selected language ----
  observeEvent(input$selectedLanguage, {
    shiny.i18n::update_lang(input$selectedLanguage, session)
  })
  ## Update pickers - Scenarios ----
  ### Scenarios pickers ----
  # Observe changes in any scenario
  # Update choices and keep selected
  obsListAllScenarios <- reactive({
    list(resultsRV$simulate, resultsRV$select)
  })
  observeEvent(obsListAllScenarios(), {
    # Simulate tab
    updatePickerInput(session, inputId = "mergeSimulateInput", choices = names(resultsRV$simulate),
                      selected = input$mergeSimulateInput)
    updatePickerInput(session, inputId = "removeSimulateInput", choices = names(resultsRV$simulate),
                      selected = input$removeSimulateInput)
    updatePickerInput(session, inputId = "scenarioSimulateSummaryInput", choices = names(resultsRV$simulate),
                      selected = input$scenarioSimulateSummaryInput)
    # Compute tab
    updatePickerInput(session, inputId = "scenarioComParInput", choices = names(resultsRV$simulate), 
                      selected = input$scenarioComParInput)
    updatePickerInput(session, inputId = "scenarioComStandParInput", choices = names(resultsRV$simulate), 
                      selected = input$scenarioComStandParInput)
    updatePickerInput(session, inputId = "scenarioComMultiInput", choices = names(resultsRV$simulate),
                      selected = input$scenarioComMultiInput)
    updatePickerInput(session, inputId = "scenarioComputeSummaryInput", choices = names(resultsRV$simulate),
                      selected = input$scenarioComputeSummaryInput)
    # Select tab
    updatePickerInput(session, inputId = "scenarioSelInput", choices = names(resultsRV$simulate),
                      selected = input$scenarioSelInput)
    updatePickerInput(session, inputId = "mergeSelectInput", choices = names(resultsRV$select),
                      selected = input$mergeSelectInput)
    updatePickerInput(session, inputId = "removeSelectInput", choices = names(resultsRV$select),
                      selected = input$removeSelectInput)
    updatePickerInput(session, inputId = "scenarioSelectSummaryInput", choices = names(resultsRV$select),
                      selected = input$scenarioSelectSummaryInput)
    # View tab
    if(input$scenarioTypeViewParInput == "Raw"){
      updatePickerInput(session, inputId = "scenarioViewParInput", choices = names(resultsRV$simulate),
                        selected = input$scenarioViewParInput)	
    } else{
      updatePickerInput(session, inputId = "scenarioViewParInput", choices = names(resultsRV$select),
                        selected = input$scenarioViewParInput)
    }
    if(input$scenarioTypeViewMultiInput == "Raw"){
      updatePickerInput(session, inputId = "scenarioViewMultiInput", choices = names(resultsRV$simulate),
                        selected = input$scenarioViewMultiInput)	
    } else{
      updatePickerInput(session, inputId = "scenarioViewMultiInput", choices = names(resultsRV$select),
                        selected = input$scenarioViewMultiInput)
    }
    # Export tab
    if(input$scenarioTypeExportInput == "Raw"){
      updatePickerInput(session, inputId = "scenarioExportInput", choices = names(resultsRV$simulate),
                        selected = input$scenarioExportInput)	
    } else{
      updatePickerInput(session, inputId = "scenarioExportInput", choices = names(resultsRV$select),
                        selected = input$scenarioExportInput)
    }
  })
  ### View tab - scenarioTypeViewParInput ----
  observeEvent(input$scenarioTypeViewParInput, {
    if(input$scenarioTypeViewParInput == "Raw"){
      if(!is.null(names(resultsRV$simulate))){
        updatePickerInput(session, inputId = "scenarioViewParInput", choices = names(resultsRV$simulate),
                          selected = NULL)	
      } else{
        updatePickerInput(session, inputId = "scenarioViewParInput", choices = character(0),
                          selected = NULL)
      }
    } else{
      if(!is.null(names(resultsRV$select))){
        updatePickerInput(session, inputId = "scenarioViewParInput", choices = names(resultsRV$select),
                          selected = NULL)	
      } else{
        updatePickerInput(session, inputId = "scenarioViewParInput", choices = character(0),
                          selected = NULL)
      }
    }
  })
  ### View tab - scenarioTypeViewMultiInput ----
  observeEvent(input$scenarioTypeViewMultiInput, {
    if(input$scenarioTypeViewMultiInput == "Raw"){
      if(!is.null(names(resultsRV$simulate))){
        updatePickerInput(session, inputId = "scenarioViewMultiInput", choices = names(resultsRV$simulate),
                          selected = NULL)	
      } else{
        updatePickerInput(session, inputId = "scenarioViewMultiInput", choices = character(0),
                          selected = NULL)	
      }
    } else{
      if(!is.null(names(resultsRV$select))){
        updatePickerInput(session, inputId = "scenarioViewMultiInput", choices = names(resultsRV$select),
                          selected = NULL)	
      } else{
        updatePickerInput(session, inputId = "scenarioViewMultiInput", choices = character(0),
                          selected = NULL)
      }
    }
  })
  ### Export tab - scenarioTypeExportInput ----
  observeEvent(input$scenarioTypeExportInput, {
    if(input$scenarioTypeExportInput == "Raw"){
      if(!is.null(names(resultsRV$simulate))){
        updatePickerInput(session, inputId = "scenarioExportInput", choices = names(resultsRV$simulate),
                          selected = NULL)		
      } else{
        updatePickerInput(session, inputId = "scenarioExportInput", choices = character(0),
                          selected = NULL)	
      }
    } else{
      if(!is.null(names(resultsRV$select))){
        updatePickerInput(session, inputId = "scenarioExportInput", choices = names(resultsRV$select),
                          selected = NULL)	
      } else{
        updatePickerInput(session, inputId = "scenarioExportInput", choices = character(0),
                          selected = NULL)
      }
    }
  })
  ## Update pickers - Parameters ----
  ### Select tab - testsDetSelInput, testsHieSelInput and groupSelInput ----
  # Update choices 
  obsListSelPar <- reactive({
    list(input$scenarioSelInput, resultsRV$updatePar)
  })
  observeEvent(obsListSelPar(), {
    if(!is.null(input$scenarioSelInput)){
      scenario <- resultsRV$simulate[[input$scenarioSelInput]]
      if(!is.null(colnames(scenario$simulation$results))){
        updatePickerInput(session, inputId = "testsDetSelInput", choices = colnames(scenario$simulation$results))
        updatePickerInput(session, inputId = "testsHieSelInput", choices = colnames(scenario$simulation$results))
        updatePickerInput(session, inputId = "groupSelInput", choices = colnames(scenario$simulation$results))
      } else{
        updatePickerInput(session, inputId = "testsDetSelInput", choices = character(0),
                          selected = NULL)
        updatePickerInput(session, inputId = "testsHieSelInput", choices = character(0),
                          selected = NULL)
        updatePickerInput(session, inputId = "groupSelInput", choices = character(0),
                          selected = NULL)
      }
    } else{
      updatePickerInput(session, inputId = "testsDetSelInput", choices = character(0),
                        selected = NULL)
      updatePickerInput(session, inputId = "testsHieSelInput", choices = character(0),
                        selected = NULL)
      updatePickerInput(session, inputId = "groupSelInput", choices = character(0),
                        selected = NULL)
    }
  })
  ### Compute tab - testsMultiInput ----
  # Update choices 
  obsListComMulti <- reactive({
    list(input$scenarioComMultiInput, resultsRV$updatePar)
  })
  observeEvent(obsListComMulti(), {
    if(!is.null(input$scenarioComMultiInput)){
      scenario <- resultsRV$simulate[[input$scenarioComMultiInput]]
      if(!is.null(colnames(scenario$simulation$results))){
        updatePickerInput(session, inputId = "testsMultiInput", choices = colnames(scenario$simulation$results))
      } else{
        updatePickerInput(session, inputId = "testsMultiInput", choices = character(0),
                          selected = NULL)	
      }
    } else{
      updatePickerInput(session, inputId = "testsMultiInput", choices = character(0),
                        selected = NULL)
    }
  })
  ### Compute tab - stanComParInput----
  # Update choices 
  obsListComStandPar <- reactive({
    list(input$scenarioComStandParInput, resultsRV$updatePar)
  })
  observeEvent(obsListComStandPar(), {
    if(!is.null(input$scenarioComStandParInput)){
      scenario <- resultsRV$simulate[[input$scenarioComStandParInput]]
      if(!is.null(colnames(scenario$simulation$results))){
        updatePickerInput(session, inputId = "stanComParInput", choices = colnames(scenario$simulation$results))	
      } else{
        updatePickerInput(session, inputId = "stanComParInput", choices = character(0),
                          selected = NULL)	
      }
    } else{
      updatePickerInput(session, inputId = "stanComParInput", choices = character(0),
                        selected = NULL)
    }
  })
  ### View tab - xvarViewInput and yvarViewInput ----
  # Update choices 
  obsListViewPar <- reactive({
    list(input$scenarioViewParInput, resultsRV$updatePar)
  })
  observeEvent(obsListViewPar(), {
    if(!is.null(input$scenarioViewParInput)){
      if(input$scenarioTypeViewParInput == "Raw"){
        scenario <- resultsRV$simulate[[input$scenarioViewParInput]]
      } else{
        scenario <- resultsRV$select[[input$scenarioViewParInput]]
      }
      if (inherits(scenario, "simRest")) {
        res <- scenario$simulation$results
      }
      else {
        res <- scenario$selection$results
      }
      if(!is.null(res)){
        updatePickerInput(session, inputId = "xvarViewInput", choices = colnames(res))
        updatePickerInput(session, inputId = "yvarViewInput", choices = colnames(res))
      } else{
        updatePickerInput(session, inputId = "xvarViewInput", choices = character(0), selected = NULL)
        updatePickerInput(session, inputId = "yvarViewInput", choices = character(0), selected = NULL)
      }
    } else{
      updatePickerInput(session, inputId = "xvarViewInput", choices = character(0), selected = NULL)
      updatePickerInput(session, inputId = "yvarViewInput", choices = character(0), selected = NULL)
    }
  })
  ## Create dynamic slides ----
  ### Group probability sliders ----
  observeEvent(input$groupSimInput, {
    output$slidersProbRicSim <- renderUI({
      inVars <- unique(inputDataRV$traits[, input$groupSimInput])
      pvars <- length(inVars)
      if (pvars > 0) {
        lapply(seq(pvars), function(i) {
          shiny::sliderInput(inputId = paste0("probRichSimGrop", inVars[i]), 
                             label = paste0("Probability to draw richness - Group: ", inVars[i]), 
                             value = 1/length(inVars),
                             min = 0,
                             max = 1)
        })
      }
    })
    output$slidersProbAbuSim <- renderUI({
      inVars <- unique(inputDataRV$traits[, input$groupSimInput])
      pvars <- length(inVars)
      if (pvars > 0) {
        lapply(seq(pvars), function(i) {
          shiny::sliderInput(inputId = paste0("probAbunSimGrop", inVars[i]), 
                             label = paste0("Probability to draw abundance - Group: ", inVars[i]), 
                             value = 1/length(inVars),
                             min = 0,
                             max = 1)
        })
      }
    })
  })
  ### Hierarchical selection sliders ----
  observeEvent(input$testsHieSelInput, {
    output$slidersTestsHieSel <- renderUI({
      inVars <- input$testsHieSelInput
      pvars <- length(inVars)
      scenario <- resultsRV$simulate[[input$scenarioSelInput]]
      scenario <- scenario$simulation$results
      if (pvars > 0) {
        lapply(seq(pvars), function(i) {
          if(vectorClass(scenario[,inVars[i]]) == "numeric"){
            observeEvent(input[[paste0("logicalTestHieSelInput", inVars[i], "chart")]], {
              output$plotVarModal <- renderPlot({
                quantiles <- quantile(scenario[[inVars[i]]], prob = globalRV$probs, na.rm = TRUE)
                df.quantiles <- data.frame(q = quantiles, label = paste0(round(quantiles, 3), " \n q = ", globalRV$probs))
                ggplot2::ggplot(data = scenario) +
                  ggplot2::aes(x = .data[[inVars[i]]]) +
                  ggplot2::geom_histogram(bins = grDevices::nclass.FD(scenario[[inVars[i]]][!is.na(scenario[[inVars[i]]])]), col = "#ffffff", fill = "#1d4b61") +
                  ggplot2::scale_x_continuous(breaks = df.quantiles$q, labels = df.quantiles$label, guide = ggplot2::guide_axis(n.dodge = 2)) +
                  themeResbiota(baseSize = 15)
              })
              showModal(modalDialog(plotOutput("plotVarModal"),
                                    footer = modalButton("Dismiss"),
                                    fade = FALSE,
                                    easyClose = TRUE,
                                    size = "xl"), session = session)
            })
            # If integers
            if(all(scenario[,inVars[i]] == floor(scenario[,inVars[i]]), na.rm = TRUE)){
              choicesTemp <- seq(min(scenario[,inVars[i]], na.rm = TRUE), max(scenario[,inVars[i]], na.rm = TRUE))
              selectedTemp <- c(min(scenario[,inVars[i]], na.rm = TRUE), max(scenario[,inVars[i]], na.rm = TRUE))
            } else { # If reals
              choicesTemp <- round(seq(min(scenario[,inVars[i]], na.rm = TRUE), max(scenario[,inVars[i]], na.rm = TRUE), 
                                       length.out = 100), digits = input$decimalPlaces)
              selectedTemp <- choicesTemp[c(1, 100)]
            }
            shinyWidgets::sliderTextInput(inputId = paste0("logicalTestHieSelInput", inVars[i]),
                                          label = htmltools::p(i18n$t("Parameter:"),
                                                               inVars[i],
                                                               shiny::actionButton(paste0("logicalTestHieSelInput", inVars[i], "chart"), 
                                                                                   label = "",
                                                                                   icon = shiny::icon("chart-simple"),
                                                                                   style = 'padding:4px; font-size:60%')),
                                          choices = choicesTemp,
                                          selected = selectedTemp
            )
          } else {
            observeEvent(input[[paste0("logicalTestHieSelInput", inVars[i], "chart")]], {
              output$plotVarModal <- renderPlot({
                ggplot2::ggplot(data = scenario) +
                  ggplot2::aes(x = .data[[inVars[i]]]) +
                  ggplot2::geom_bar(fill = "#1d4b61") +
                  themeResbiota(baseSize = 15)
              })
              showModal(modalDialog(plotOutput("plotVarModal"),
                                    footer = modalButton("Dismiss"),
                                    fade = FALSE,
                                    easyClose = TRUE,
                                    size = "xl"), session = session)
            })
            shinyWidgets::pickerInput(inputId = paste0("logicalTestHieSelInput", inVars[i]),
                                      label = htmltools::p(i18n$t("Parameter:"),
                                                           inVars[i],
                                                           shiny::actionButton(paste0("logicalTestHieSelInput", inVars[i], "chart"), 
                                                                               label = "",
                                                                               icon = shiny::icon("chart-simple"),
                                                                               style = 'padding:4px; font-size:60%')),
                                      choices = unique(scenario[,inVars[i]]),
                                      multiple = TRUE,
                                      options = list(`actions-box` = TRUE),
                                      inline = FALSE
            )
          }
        })
      }
    })
  })
  ### Deterministic selection sliders ----
  observeEvent(input$testsDetSelInput, {
    output$slidersTestsDetSel <- renderUI({
      inVars <- input$testsDetSelInput
      pvars <- length(inVars)
      scenario <- resultsRV$simulate[[input$scenarioSelInput]]
      scenario <- scenario$simulation$results
      if (pvars > 0) {
        lapply(seq(pvars), function(i) {
          if(vectorClass(scenario[,inVars[i]]) == "numeric"){
            observeEvent(input[[paste0("logicalTestDetSelInput", inVars[i], "chart")]], {
              output$plotVarModal <- renderPlot({
                quantiles <- quantile(scenario[[inVars[i]]], prob = globalRV$probs, na.rm = TRUE)
                df.quantiles <- data.frame(q = quantiles, label = paste0(round(quantiles, 3), " \n q = ", globalRV$probs))
                ggplot2::ggplot(data = scenario) +
                  ggplot2::aes(x = .data[[inVars[i]]]) +
                  ggplot2::geom_histogram(bins = grDevices::nclass.FD(scenario[[inVars[i]]][!is.na(scenario[[inVars[i]]])]), col = "#ffffff", fill = "#1d4b61") +
                  ggplot2::scale_x_continuous(breaks = df.quantiles$q, labels = df.quantiles$label, guide = ggplot2::guide_axis(n.dodge = 2)) +
                  themeResbiota(baseSize = 15)
              })
              showModal(modalDialog(plotOutput("plotVarModal"),
                                    footer = modalButton(i18n$t("Dismiss")),
                                    fade = FALSE,
                                    easyClose = TRUE,
                                    size = "xl"), session = session)
            })
            # If integers
            if(all(scenario[,inVars[i]] == floor(scenario[,inVars[i]]), na.rm = TRUE)){
              choicesTemp <- seq(min(scenario[,inVars[i]], na.rm = TRUE), max(scenario[,inVars[i]], na.rm = TRUE))
              selectedTemp <- c(min(scenario[,inVars[i]], na.rm = TRUE), max(scenario[,inVars[i]], na.rm = TRUE))
            } else { # If reals
              choicesTemp <- round(seq(min(scenario[,inVars[i]], na.rm = TRUE), max(scenario[,inVars[i]], na.rm = TRUE), 
                                       length.out = 100), digits = input$decimalPlaces)
              selectedTemp <- choicesTemp[c(1, 100)]
            }
            shinyWidgets::sliderTextInput(inputId = paste0("logicalTestDetSelInput", inVars[i]),
                                          label = htmltools::p(i18n$t("Parameter:"),
                                                               inVars[i],
                                                               shiny::actionButton(paste0("logicalTestDetSelInput", inVars[i], "chart"), 
                                                                                   label = "",
                                                                                   icon = shiny::icon("chart-simple"),
                                                                                   style = 'padding:4px; font-size:60%')),
                                          choices = choicesTemp,
                                          selected = selectedTemp
            )
          } else {
            observeEvent(input[[paste0("logicalTestDetSelInput", inVars[i], "chart")]], {
              output$plotVarModal <- renderPlot({
                ggplot2::ggplot(data = scenario) +
                  ggplot2::aes(x = .data[[inVars[i]]]) +
                  ggplot2::geom_bar(fill = "#1d4b61") +
                  themeResbiota(baseSize = 15)
              })
              showModal(modalDialog(plotOutput("plotVarModal"),
                                    footer = modalButton(i18n$t("Dismiss")),
                                    fade = FALSE,
                                    easyClose = TRUE,
                                    size = "xl"), session = session)
            })
            shinyWidgets::pickerInput(inputId = paste0("logicalTestDetSelInput", inVars[i]),
                                      label = htmltools::p(i18n$t("Parameter:"),
                                                           inVars[i],
                                                           shiny::actionButton(paste0("logicalTestDetSelInput", inVars[i], "chart"), 
                                                                               label = "",
                                                                               icon = shiny::icon("chart-simple"),
                                                                               style = 'padding:4px; font-size:60%')),
                                      choices = unique(scenario[,inVars[i]]),
                                      multiple = TRUE,
                                      options = list(`actions-box` = TRUE),
                                      inline = FALSE
            )
          }
        })
      }
    })
  })
  ### Multifunctionality sliders ----
  observeEvent(input$testsMultiInput, {
    output$slidersMulti <- renderUI({
      inVars <- input$testsMultiInput
      pvars <- length(inVars)
      scenario <- resultsRV$simulate[[input$scenarioComMultiInput]]
      scenario <- scenario$simulation$results
      if (pvars > 0) {
        lapply(seq(pvars), function(i) {
          if(vectorClass(scenario[,inVars[i]]) == "numeric"){
            observeEvent(input[[paste0("logicalTestMultiInput", inVars[i], "chart")]], {
              output$plotVarModal <- renderPlot({
                quantiles <- quantile(scenario[[inVars[i]]], prob = globalRV$probs, na.rm = TRUE)
                df.quantiles <- data.frame(q = quantiles, label = paste0(round(quantiles, 3), " \n q = ", globalRV$probs))
                ggplot2::ggplot(data = scenario) +
                  ggplot2::aes(x = .data[[inVars[i]]]) +
                  ggplot2::geom_histogram(bins = grDevices::nclass.FD(scenario[[inVars[i]]][!is.na(scenario[[inVars[i]]])]), col = "#ffffff", fill = "#1d4b61") +
                  ggplot2::scale_x_continuous(breaks = df.quantiles$q, labels = df.quantiles$label, guide = ggplot2::guide_axis(n.dodge = 2)) +
                  themeResbiota(baseSize = 15)
              })
              showModal(modalDialog(plotOutput("plotVarModal"),
                                    footer = modalButton(i18n$t("Dismiss")),
                                    fade = FALSE,
                                    easyClose = TRUE,
                                    size = "xl"), session = session)
            })
            # If integers
            if(all(scenario[,inVars[i]] == floor(scenario[,inVars[i]]), na.rm = TRUE)){
              choicesTemp <- seq(min(scenario[,inVars[i]], na.rm = TRUE), max(scenario[,inVars[i]], na.rm = TRUE))
              selectedTemp <- c(min(scenario[,inVars[i]], na.rm = TRUE), max(scenario[,inVars[i]], na.rm = TRUE))
            } else { # If reals
              choicesTemp <- round(seq(min(scenario[,inVars[i]], na.rm = TRUE), max(scenario[,inVars[i]], na.rm = TRUE), 
                                       length.out = 100), digits = input$decimalPlaces)
              selectedTemp <- choicesTemp[c(1, 100)]
            }
            shinyWidgets::sliderTextInput(inputId = paste0("logicalTestMultiInput", inVars[i]),
                                          label = htmltools::p(i18n$t("Parameter:"),
                                                               inVars[i],
                                                               shiny::actionButton(paste0("logicalTestMultiInput", inVars[i], "chart"), 
                                                                                   label = "",
                                                                                   icon = shiny::icon("chart-simple"),
                                                                                   style = 'padding:4px; font-size:60%')),
                                          choices = choicesTemp,
                                          selected = selectedTemp
            )
          } else {
            observeEvent(input[[paste0("logicalTestMultiInput", inVars[i], "chart")]], {
              output$plotVarModal <- renderPlot({
                ggplot2::ggplot(data = scenario) +
                  ggplot2::aes(x = .data[[inVars[i]]]) +
                  ggplot2::geom_bar(fill = "#1d4b61") +
                  themeResbiota(baseSize = 15)
              })
              showModal(modalDialog(plotOutput("plotVarModal"),
                                    footer = modalButton(i18n$t("Dismiss")),
                                    fade = FALSE,
                                    easyClose = TRUE,
                                    size = "xl"), session = session)
            })
            shinyWidgets::pickerInput(inputId = paste0("logicalTestMultiInput", inVars[i]),
                                      label = htmltools::p(i18n$t("Parameter:"),
                                                           inVars[i],
                                                           shiny::actionButton(paste0("logicalTestMultiInput", inVars[i], "chart"),
                                                                               label = "",
                                                                               icon = shiny::icon("chart-simple"),
                                                                               style = 'padding:4px; font-size:60%')),
                                      choices = unique(scenario[,inVars[i]]),
                                      multiple = TRUE,
                                      options = list(`actions-box` = TRUE),
                                      inline = FALSE
            )
          }
        })
      }
    })
  })
  ## Check buttons - OK ----
  ### Check doCompute button ----
  observeEvent(input$scenarioComParInput, ignoreNULL = FALSE, {
    if(is.null(input$scenarioComParInput)){
      updateActionButton(session, "doCompute", disabled = TRUE)
    } else(
      updateActionButton(session, "doCompute", disabled = FALSE)
    )
  })
  ### Check doStandardize button ----
  observeEvent(input$scenarioComStandParInput, ignoreNULL = FALSE, {
    if(is.null(input$scenarioComStandParInput)){
      updateActionButton(session, "doStandardize", disabled = TRUE)
    } else(
      updateActionButton(session, "doStandardize", disabled = FALSE)
    )
  })
  ### Check doMultiCompute button ----
  observeEvent(input$scenarioComMultiInput, ignoreNULL = FALSE, {
    if(is.null(input$scenarioComMultiInput)){
      updateActionButton(session, "doMultiCompute", disabled = TRUE)
    } else(
      updateActionButton(session, "doMultiCompute", disabled = FALSE)
    )
  })
  ### Check doSelect button ----
  obsListDoSelect <- reactive({
    list(input$prefixSelInput, input$scenarioSelInput)
  })
  observeEvent(obsListDoSelect(), ignoreNULL = FALSE, {
    if(is.null(input$scenarioSelInput) || input$prefixSelInput == ""){
      updateActionButton(session, "doSelect", disabled = TRUE)
    } else(
      updateActionButton(session, "doSelect", disabled = FALSE)
    )
  })
  ### Check doPlotPar button ----
  observeEvent(input$scenarioViewParInput, ignoreNULL = FALSE, {
    if(is.null(input$scenarioViewParInput)){
      updateActionButton(session, "doPlotPar", disabled = TRUE)
    } else(
      updateActionButton(session, "doPlotPar", disabled = FALSE)
    )
  })
  ### Check doPlotMulti button ----
  observeEvent(input$scenarioViewMultiInput, ignoreNULL = FALSE, {
    if(is.null(input$scenarioViewMultiInput)){
      updateActionButton(session, "doPlotMulti", disabled = TRUE)
    } else(
      updateActionButton(session, "doPlotMulti", disabled = FALSE)
    )
  })
  ### Check doExport button ----
  observeEvent(input$scenarioExportInput, ignoreNULL = FALSE, {
    if(is.null(input$scenarioExportInput)){
      updateActionButton(session, "doExport", disabled = TRUE)
    } else(
      updateActionButton(session, "doExport", disabled = FALSE)
    )
  })
  ### Check doSelectMerge button ----
  obsListDoSelectMerge <- reactive({
    list(input$mergeSelectNameInput, input$mergeSelectInput)
  })
  observeEvent(obsListDoSelectMerge(), ignoreNULL = FALSE, {
    if(is.null(input$mergeSelectInput) || input$mergeSelectNameInput == ""){
      updateActionButton(session, "doSelectMerge", disabled = TRUE)
    } else(
      updateActionButton(session, "doSelectMerge", disabled = FALSE)
    )
  })
  ### Check doSimulateMerge button ----
  obsListDoSimulateMerge <- reactive({
    list(input$mergeSimulateNameInput, input$mergeSimulateInput)
  })
  observeEvent(obsListDoSimulateMerge(), ignoreNULL = FALSE, {
    if(is.null(input$mergeSimulateInput) || input$mergeSimulateNameInput == ""){
      updateActionButton(session, "doSimulateMerge", disabled = TRUE)
    } else(
      updateActionButton(session, "doSimulateMerge", disabled = FALSE)
    )
  })
  ### Check doSelectRemove button ----
  observeEvent(input$removeSelectInput, ignoreNULL = FALSE, {
    if(is.null(input$removeSelectInput)){
      updateActionButton(session, "doSelectRemove", disabled = TRUE)
    } else(
      updateActionButton(session, "doSelectRemove", disabled = FALSE)
    )
  })
  ### Check doSimulateRemove button ----
  observeEvent(input$removeSimulateInput, ignoreNULL = FALSE, {
    if(is.null(input$removeSimulateInput)){
      updateActionButton(session, "doSimulateRemove", disabled = TRUE)
    } else(
      updateActionButton(session, "doSimulateRemove", disabled = FALSE)
    )
  })
  ## Input aux ----
  ### Simulate tab - prefixSimInput ----
  observeEvent(input$prefixSimInput, {
    if(!input$prefixSimInput == ""){
      updateActionButton(session, "doSimulate", disabled = FALSE)
    } else{
      updateActionButton(session, "doSimulate", disabled = TRUE)
    }
  })
  ### Simulate tab - itSimInput ----
  # Check - If itSimInput is NA or < 4 disable the simulate button
  observeEvent(input$itSimInput, {
    if(!is.na(input$itSimInput)){
      inputParSimRV$it <- input$itSimInput
      if(input$itSimInput>=4){
        updateActionButton(session, "doSimulate", disabled = FALSE)	
      } else{
        updateActionButton(session, "doSimulate", disabled = TRUE)
      }
    } else{
      updateActionButton(session, "doSimulate", disabled = TRUE)
      inputParSimRV$it <- NULL
    }
  })
  ### Simulate tab - restComp and restGroup----
  obsListRest <- reactive({
    list(input$goalsSimInput, inputDataRV$restComp, inputDataRV$restGroup)
  })
  observeEvent(obsListRest(), {
    if(input$goalsSimInput == "New"){
      inputParSimRV$restComp <- NULL
      inputParSimRV$restGroup <- NULL
    } else{ # Ongoing
      inputParSimRV$restComp <- inputDataRV$restComp
      inputParSimRV$restGroup <- inputDataRV$restGroup
    }
  })
  ### Simulate tab - nIndSimInput and cvAbundSimInput ----
  obsListSimMethod <- reactive({
    list(input$methodSimInput, input$nIndSimInput, input$cvAbundSimInput)
  })
  observeEvent(obsListSimMethod(), {
    if(tolower(input$methodSimInput) == "individuals"){
      if(is.na(input$nIndSimInput) || is.na(input$cvAbundSimInput)){
        updateActionButton(session, "doSimulate", disabled = TRUE)
      } else{
        updateActionButton(session, "doSimulate", disabled = FALSE)
      }
      inputParSimRV$nInd <- input$nIndSimInput
      inputParSimRV$cvAbund <- input$cvAbundSimInput
    } else{
      updateActionButton(session, "doSimulate", disabled = FALSE)
      inputParSimRV$nInd <- NULL
      inputParSimRV$cvAbund <- NULL
    }
  })
  ### View tab - dbFormatExpInput ----
  observeEvent(input$dbFormatExpInput, ignoreNULL = FALSE, {
    if(!is.null(input$dbFormatExpInput)){
      exportRV$dbFormat <- input$dbFormatExpInput
    } else{
      exportRV$dbFormat <- FALSE
    }
  })
  ## Action buttons ----
  ### doSimulate ----
  observeEvent(input$doSimulate, {
    # Remove any open modal
    removeModal(session = session)
    # Check if species trait data exist
    if(is.null(inputDataRV$traits)){
      sendSweetAlert(
        session = session,
        title = "Error!",
        text = "Load the species traits data",
        type = "error"
      )
    } else {
      showModal(modalDialog(title = "Running", footer = NULL), session = session)
      # Set and update the arguments group, probGroupRich and probGroupAbund
      # Update using dynamic slides
      if(input$speficyGroupsSimInput == "Yes" && !is.null(input$groupSimInput)){
        inputParSimRV$group <- input$groupSimInput
        inVars <- unique(inputDataRV$traits[, input$groupSimInput])
        pvars <- length(inVars)
        if(input$probGroupTypeSimInput == "Abundance"){
          argListAbunTemp <- vector("list", length = pvars)
          names(argListAbunTemp) <- inVars
          for(i in 1:pvars){
            argListAbunTemp[[i]] <- input[[paste0("probAbunSimGrop", inVars[i])]]
          }
          inputParSimRV$probGroupRich <- NULL
          inputParSimRV$probGroupAbund <- unlist(argListAbunTemp, use.names = TRUE)
        }
        if(input$probGroupTypeSimInput == "Richness and abundance"){
          argListRichTemp <- vector("list", length = pvars)
          names(argListRichTemp) <- inVars
          argListAbunTemp <- vector("list", length = pvars)
          names(argListAbunTemp) <- inVars
          for(i in 1:pvars){
            argListRichTemp[[i]] <- input[[paste0("probRichSimGrop", inVars[i])]]
            argListAbunTemp[[i]] <- input[[paste0("probAbunSimGrop", inVars[i])]]
          }
          inputParSimRV$probGroupRich <- unlist(argListRichTemp, use.names = TRUE)
          inputParSimRV$probGroupAbund <- unlist(argListAbunTemp, use.names = TRUE)
        }
      } else{
        inputParSimRV$group <- NULL
        inputParSimRV$probGroupRich <- NULL
        inputParSimRV$probGroupAbund <- NULL
      }
      scenario <- simulateCommunities(trait = inputDataRV$traits,
                                      restComp = inputParSimRV$restComp, # Ok
                                      restGroup = inputParSimRV$restGroup, # Ok
                                      ava = input$avaSimInput, # straight input
                                      und = input$undSimInput, # straight input
                                      it = inputParSimRV$it, # Ok
                                      rich = input$richSliderSimInput, # straight input
                                      cwm = input$cwmSimInput, # straight input
                                      rao = input$raoSimInput, # straight input
                                      prob = input$probSimInput, # straight input
                                      phi = input$phiSimInput, # straight input
                                      nInd = inputParSimRV$nInd, # Ok
                                      cvAbund = inputParSimRV$cvAbund, # Ok
                                      prefix = input$prefixSimInput, # straigth input
                                      method = tolower(input$methodSimInput), # straight input
                                      group = inputParSimRV$group, # Ok
                                      probGroupRich = inputParSimRV$probGroupRich, # Ok
                                      probGroupAbund = inputParSimRV$probGroupAbund # Ok
      )
      resultsRV$simulate[[input$prefixSimInput]] <- scenario
      # Update basic informations
      resultsRV$nSim <- sum(sapply(resultsRV$simulate, function(x) nrow(x$simulation$composition)))
      resultsRV$nSce <- length(resultsRV$simulate)
      removeModal(session = session)
      sendSweetAlert(
        session = session,
        title = "Done!",
        text = paste0("Simulations scenarios: ", resultsRV$nSce),
        type = "success"
      )
    }
  })
  ### doSimulateMerge ----
  observeEvent(input$doSimulateMerge, {
    # If the picker input is valid
    if(length(input$mergeSimulateInput)>0){
      tempRV <- vector("list", length = length(input$mergeSimulateInput))
      for(i in 1:length(input$mergeSimulateInput)){
        # Copy to temp list
        tempRV[[i]] <- resultsRV$simulate[[input$mergeSimulateInput[i]]]
        # Then remove
        resultsRV$simulate[[input$mergeSimulateInput[i]]] <- NULL
      }
      # Merge
      resultsRV$simulate[[input$mergeSimulateNameInput]] <- do.call(mergeSimulations, tempRV)
      resultsRV$simulate[[input$mergeSimulateNameInput]]$call <- "Call" # Remove long call
    }
    # Update basic informations
    resultsRV$nSim <- sum(sapply(resultsRV$simulate, function(x) nrow(x$simulation$composition)))
    resultsRV$nSce <- length(resultsRV$simulate)
    sendSweetAlert(
      session = session,
      title = "Done!",
      text = paste0("Simulations scenarios: ", resultsRV$nSce),
      type = "success"
    )
  })
  ### doSimulateRemove ----
  observeEvent(input$doSimulateRemove, {
    # If the picker input is valid
    if(length(input$removeSimulateInput)>0){
      for(i in 1:length(input$removeSimulateInput)){
        resultsRV$simulate[[input$removeSimulateInput[i]]] <- NULL
      }
    }
    # Update basic informations
    resultsRV$nSce <- length(resultsRV$simulate)
    if(resultsRV$nSce>0){
      resultsRV$nSim <- sum(sapply(resultsRV$simulate, function(x) nrow(x$simulation$composition)))	
    } else{
      resultsRV$nSim <- 0
    }
    sendSweetAlert(
      session = session,
      title = "Done!",
      text = paste0("Simulations scenarios: ", resultsRV$nSce),
      type = "success"
    )
  })
  ### doCompute ----
  observeEvent(input$doCompute, {
    # Remove any open modal
    removeModal(session = session)
    checkCost <- c(is.null(input$costComInput), is.null(input$densComInput))
    if(!c(all(checkCost == TRUE) || all(checkCost == FALSE))){
      sendSweetAlert(
        session = session,
        title = "Error!",
        text = "Specify cost and density. Or none of them",
        type = "error"
      )
    } else {
      showModal(modalDialog(title = "Running", footer = NULL), session = session)
      scenario <- computeParameters(x = resultsRV$simulate[[input$scenarioComParInput]],
                                    trait = inputDataRV$traits,
                                    ava = input$avaComInput, # straight input
                                    cwm = input$cwmComInput, # straight input
                                    cwv = input$cwvComInput, # straight input
                                    rao = input$raoComInput, # straight input
                                    cost = input$costComInput, # straight input
                                    dens = input$densComInput, # straight input
                                    dissimilarity = input$disComInput, # straight input
                                    reference = inputDataRV$reference,
                                    supplementary = inputDataRV$supplementary
      )
      # Round for facilitate next steps (sliders)
      nums <- vapply(scenario$simulation$results, is.numeric, FUN.VALUE = logical(1))
      scenario$simulation$results[,nums] <- round(scenario$simulation$results[,nums], digits = input$decimalPlaces)
      if(!is.null(scenario$reference$results)){
        nums <- vapply(scenario$reference$results, is.numeric, FUN.VALUE = logical(1))
        scenario$reference$results[, nums] <- round(scenario$reference$results[, nums], digits = input$decimalPlaces)
      }
      if(!is.null(scenario$supplementary$results)){
        nums <- vapply(scenario$supplementary$results, is.numeric, FUN.VALUE = logical(1))
        scenario$supplementary$results[, nums] <- round(scenario$supplementary$results[, nums], digits = input$decimalPlaces)
      }
      resultsRV$simulate[[input$scenarioComParInput]] <- scenario
      removeModal(session = session)
      # Force update parameters
      resultsRV$updatePar <- ifelse(resultsRV$updatePar == 1, 0, 1)
      sendSweetAlert(
        session = session,
        title = "Done!",
        type = "success"
      )
    }
  })
  ### doMultiCompute ----
  observeEvent(input$doMultiCompute, {
    # Set and update the argument tests
    # Update using dynamic slides
    if(!is.null(input$testsMultiInput)){
      inVars <- input$testsMultiInput
      names(inVars) <- inVars
      pvars <- length(inVars)
      testList <- c()
      scenario <- resultsRV$simulate[[input$scenarioComMultiInput]]
      scenario <- scenario$simulation$results
      for(i in seq_len(pvars)){
        if(vectorClass(scenario[,inVars[i]]) == "numeric"){
          nameTest <- inVars[i]
          valueTest <- input[[paste0("logicalTestMultiInput", inVars[i])]]
          testTemp <- paste(nameTest, ">=", valueTest[1], "&", nameTest, "<=", valueTest[2])
          testList <- c(testList, testTemp)
        } else {
          nameTest <- inVars[i]
          valueTest <- input[[paste0("logicalTestMultiInput", inVars[i])]]
          valueTest <- paste("'", valueTest, "'", sep = "")
          testTemp <- paste(paste(nameTest, "==", valueTest), collapse = " | ")
          testList <- c(testList, testTemp)
        }
      }
    } else{
      testList <- NULL
    }
    scenario <- computeMultifunctionality(x = resultsRV$simulate[[input$scenarioComMultiInput]],
                                          tests = testList)
    # No need to round the numbers
    resultsRV$simulate[[input$scenarioComMultiInput]] <- scenario
    # Force update parameters
    resultsRV$updatePar <- ifelse(resultsRV$updatePar == 1, 0, 1)
    sendSweetAlert(
      session = session,
      title = "Done!",
      type = "success"
    )
  })
  ### doStandardize ----
  observeEvent(input$doStandardize, {
    scenario <- resultsRV$simulate[[input$scenarioComStandParInput]]
    # if (inherits(scenario, "simRest")) {
    res <- scenario$simulation$results
    # } else {
    # 	res <- scenario$selection$results
    # }
    if(is.null(res)){
      sendSweetAlert(
        session = session,
        title = "Error!",
        text = "Compute functional parameters in this scenario",
        type = "error"
      )
    } else{
      scenario <- standardizeParameters(x = scenario,
                                        parameters = input$stanComParInput, # straight input
                                        method = input$speficyMethodStanInput # straight input
      )
      # Round for facilitate next steps (sliders)
      nums <- vapply(scenario$simulation$results, is.numeric, FUN.VALUE = logical(1))
      scenario$simulation$results[,nums] <- round(scenario$simulation$results[, nums], digits = input$decimalPlaces)
      if(!is.null(scenario$reference$results)){
        nums <- vapply(scenario$reference$results, is.numeric, FUN.VALUE = logical(1))
        scenario$reference$results[, nums] <- round(scenario$reference$results[, nums], digits = input$decimalPlaces)
      }
      if(!is.null(scenario$supplementary$results)){
        nums <- vapply(scenario$supplementary$results, is.numeric, FUN.VALUE = logical(1))
        scenario$supplementary$results[, nums] <- round(scenario$supplementary$results[, nums], digits = input$decimalPlaces)
      }
      resultsRV$simulate[[input$scenarioComStandParInput]] <- scenario
      # Force update parameters
      resultsRV$updatePar <- ifelse(resultsRV$updatePar == 1, 0, 1)
      sendSweetAlert(
        session = session,
        title = "Done!",
        type = "success"
      )
    }
  })
  
  ### doSelect ----
  observeEvent(input$doSelect, {
    # Set and update the arguments group, probGroupRich and probGroupAbund
    # Update using dynamic slides
    scenario <- resultsRV$simulate[[input$scenarioSelInput]]
    scenario <- scenario$simulation$results
    if(!is.null(input$testsHieSelInput)){
      # Parameters order
      parOrd <- input$rankHeiSelInput
      inVars <- input$testsHieSelInput
      names(inVars) <- inVars
      pvars <- length(inVars)
      # Order inVars
      if(!is.null(parOrd)){
        inVars <- inVars[parOrd]
      }
      testList <- c()
      for(i in seq_len(pvars)){
        if(vectorClass(scenario[,inVars[i]]) == "numeric"){
          nameTest <- inVars[i]
          valueTest <- input[[paste0("logicalTestHieSelInput", inVars[i])]]
          testTemp <- paste(nameTest, ">=", valueTest[1], "&", nameTest, "<=", valueTest[2])
          testList <- c(testList, testTemp)
        } else {
          nameTest <- inVars[i]
          valueTest <- input[[paste0("logicalTestHieSelInput", inVars[i])]]
          valueTest <- paste("'", valueTest, "'", sep = "")
          valueTest
          testTemp <- paste(paste(nameTest, "==", valueTest), collapse = " | ")
          testList <- c(testList, testTemp)
        }
      }
      inputParSelRV$testsHie <- testList
    } else{
      inputParSelRV$testsHie <- NULL
    }
    if(!is.null(input$testsDetSelInput)){
      inVars <- input$testsDetSelInput
      names(inVars) <- inVars
      pvars <- length(inVars)
      testList <- c()
      for(i in seq_len(pvars)){
        if(vectorClass(scenario[,inVars[i]]) == "numeric"){
          nameTest <- inVars[i]
          valueTest <- input[[paste0("logicalTestDetSelInput", inVars[i])]]
          testTemp <- paste(nameTest, ">=", valueTest[1], "&", nameTest, "<=", valueTest[2])
          testList <- c(testList, testTemp)
        } else {
          nameTest <- inVars[i]
          valueTest <- input[[paste0("logicalTestDetSelInput", inVars[i])]]
          valueTest <- paste("'", valueTest, "'", sep = "")
          testTemp <- paste(paste(nameTest, "==", valueTest), collapse = " | ")
          testList <- c(testList, testTemp)
        }
      }
      inputParSelRV$testsDet <- testList
    } else{
      inputParSelRV$testsDet <- NULL
    }
    if(input$speficyGroupsSelInput == "Yes" && !is.null(input$groupSelInput)){
      inputParSelRV$group <- input$groupSelInput
    } else{
      inputParSelRV$group <- NULL
    }
    scenario <- selectCommunities(x = resultsRV$simulate[[input$scenarioSelInput]],
                                  testsDet = inputParSelRV$testsDet,
                                  testsHie = inputParSelRV$testsHie,
                                  group = inputParSelRV$group,
                                  singleselection = as.logical(input$singleSelectionInput) # straight input
    )
    resultsRV$select[[input$prefixSelInput]] <- scenario
    # Update basic informations
    resultsRV$nSimSel <- sum(sapply(resultsRV$select, function(x) nrow(x$selection$composition)))
    resultsRV$nSel <- length(resultsRV$select)
    sendSweetAlert(
      session = session,
      title = "Done!",
      type = "success"
    )
  })
  ### doSelectMerge ----
  observeEvent(input$doSelectMerge, {
    # If the picker input is valid
    if(length(input$mergeSelectInput)>0){
      tempRV <- vector("list", length = length(input$mergeSelectInput))
      for(i in 1:length(input$mergeSelectInput)){
        # Copy to temp list
        tempRV[[i]] <- resultsRV$select[[input$mergeSelectInput[i]]]
        # Then remove
        resultsRV$select[[input$mergeSelectInput[i]]] <- NULL
      }
      # Merge
      resultsRV$select[[input$mergeSelectNameInput]] <- do.call(mergeSelection, tempRV)
      resultsRV$select[[input$mergeSelectNameInput]]$call <- "Call" # Remove long call
    }
    # Update basic informations
    resultsRV$nSimSel <- sum(sapply(resultsRV$select, function(x) nrow(x$selection$composition)))
    resultsRV$nSel <- length(resultsRV$select)
    sendSweetAlert(
      session = session,
      title = "Done!",
      type = "success"
    )
  })
  ### doSelectRemove ----
  observeEvent(input$doSelectRemove, {
    # If the picker input is valid
    if(length(input$removeSelectInput)>0){
      for(i in 1:length(input$removeSelectInput)){
        resultsRV$select[[input$removeSelectInput[i]]] <- NULL
      }
    }
    # Update basic informations
    resultsRV$nSel <- length(resultsRV$select)
    if(resultsRV$nSel>0){
      resultsRV$nSimSel <- sum(sapply(resultsRV$select, function(x) nrow(x$selection$composition)))	
    } else{
      resultsRV$nSimSel <- 0
    }
    sendSweetAlert(
      session = session,
      title = "Done!",
      type = "success"
    )
  })
  ### doPlotPar ----
  observeEvent(input$doPlotPar, {
    if(input$scenarioTypeViewParInput == "Raw"){
      scenario <- resultsRV$simulate[[input$scenarioViewParInput]]
    } else{
      scenario <- resultsRV$select[[input$scenarioViewParInput]]
    }
    if (!is.null(input$xvarViewInput) && !is.null(input$yvarViewInput)) {
      resultsRV$plotPar <- viewResults(x = scenario,
                                       xvar = input$xvarViewInput,
                                       yvar = input$yvarViewInput,
                                       hideref = as.logical(input$hideRefViewParInput)) +
        ggplot2::labs(x = input$xvarLab, y = input$yvarLab)
    } else {
      resultsRV$plotPar <- NULL
      sendSweetAlert(
        session = session,
        title = "Error!",
        text = "Select variables for both axes",
        type = "error"
      )
    }
  })
  ### doPlotParClear ----
  observeEvent(input$doPlotParClear, {
    resultsRV$plotPar <- NULL
  })
  ### doPlotMulti ----
  observeEvent(input$doPlotMulti, {
    if(input$scenarioTypeViewParInput == "Raw"){
      scenario <- resultsRV$simulate[[input$scenarioViewMultiInput]]
    } else{
      scenario <- resultsRV$select[[input$scenarioViewMultiInput]]
    }
    if (inherits(scenario, "simRest")) {
      resMulti <- scenario$simulation$multifunctionality
    }
    else {
      resMulti <- scenario$selection$multifunctionality
    }
    if (!is.null(resMulti)) {
      resultsRV$plotMulti <- viewMultifunctionality(x = scenario,
                                                    hideref = as.logical(input$hideRefViewMultiInput))
    } else {
      resultsRV$plotMulti <- NULL
      sendSweetAlert(
        session = session,
        title = "Error!",
        text = "Scenario must include multifunctionality results",
        type = "error"
      )
    }
  })
  ### doPlotMultiClear ----
  observeEvent(input$doPlotMultiClear, {
    resultsRV$plotMulti <- NULL
  })
  ### doExport ----
  observeEvent(input$doExport, {
    if(input$scenarioTypeExportInput == "Raw"){
      scenario <- resultsRV$simulate[[input$scenarioExportInput]]
    } else{
      scenario <- resultsRV$select[[input$scenarioExportInput]]
    }
    exportRV$table <- extractResults(x = scenario,
                                     type = input$typeExportInput, # straight input
                                     dbFormat = as.logical(exportRV$dbFormat),
                                     trait = inputDataRV$traits, 
                                     ava = input$avaExpInput # straight input
    )
    if(!is.null(exportRV$table)){
      exportRV$summaryTable <- as.data.frame(exportRV$table)
      exportRV$summaryTable[] <- lapply(exportRV$summaryTable, as.character)
      nRowTemp <- nrow(exportRV$summaryTable)
      nColTemp <- ncol(exportRV$summaryTable)
      if(nRowTemp>10 || nColTemp>10){
        if(nColTemp>10){
          exportRV$summaryTable <- exportRV$summaryTable[, c(1:6, (nColTemp-4):nColTemp), drop = FALSE]
          # Replace the six col
          exportRV$summaryTable[, 6] <- rep("⋯", nRowTemp)
          colnames(exportRV$summaryTable)[6] <- "⋯"
        }
        if(nRowTemp>10){
          exportRV$summaryTable <- exportRV$summaryTable[c(1:6, (nRowTemp-4):nRowTemp), , drop = FALSE]
          # Replace the six line
          exportRV$summaryTable[6, ] <- rep("⋮", ncol(exportRV$summaryTable))
        }
      }
    } else{
      exportRV$summaryTable <- NULL
    }
  })
  ### doDownloadParPlot ----
  output$doDownloadParPlot <- downloadHandler(
    filename <- function() {paste0(input$projectName, "_", input$scenarioViewParInput, "_", input$xvarViewInput, input$yvarViewInput, "_", globalRV$currentDate, ".png")},
    content <- function(file) {
      ggplot2::ggsave(file,
                      width = input$saveWidth,
                      height = input$saveHeight,
                      units = "mm",
                      dpi = input$saveDPI,
                      plot = resultsRV$plotPar)
    }
  )
  ### doDownloadMultiPlot ----
  output$doDownloadMultiPlot <- downloadHandler(
    filename <- function() {paste0(input$projectName, "_", input$scenarioViewMultiInput, "_Multifunctionality_", globalRV$currentDate, ".png")},
    content <- function(file) {
      ggplot2::ggsave(file,
                      width = input$saveWidth,
                      height = input$saveHeight,
                      units = "mm",
                      dpi = input$saveDPI,
                      plot = resultsRV$plotMulti)
    }
  )
  ### doDownloadExport ----
  output$doDownloadExport <- downloadHandler(
    filename <- function() {paste0(input$projectName, "_", input$scenarioExportInput, "_", input$typeExportInput, "_", globalRV$currentDate, ".csv")},
    content <- function(file) {
      write.csv(exportRV$table, file = file)
    }
  )
  # IMPLEMENTAR ----
  ## Help buttons ----
  observeEvent(input$titleBtId, {
    shinyalert(
      title = "",
      text = i18n$t("Test:"),
      size = "xs", 
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      type = "",
      showConfirmButton = FALSE,
      showCancelButton = FALSE,
      animation = FALSE,
      session = session
    )
  })
  # observeEvent(input$info3, {
  # 	# shinyalert(text = "Info 3", type = "info")
  # 	shinyalert(
  # 		title = "",
  # 		text = "This is a modal",
  # 		size = "s", 
  # 		closeOnEsc = TRUE,
  # 		closeOnClickOutside = TRUE,
  # 		html = FALSE,
  # 		type = "",
  # 		showConfirmButton = FALSE,
  # 		showCancelButton = FALSE,
  # 		timer = 0,
  # 		imageUrl = "",
  # 		animation = TRUE
  # 	)
  # })
  # Output aux - outputRankList ----
  observeEvent(input$testsHieSelInput, ignoreNULL = FALSE, {
    inputParSelRV$auxRankHeiSel <- input$testsHieSelInput
  })
  output$outputRankList <- renderUI({
    if(length(inputParSelRV$auxRankHeiSel)>1){
      sortable::rank_list(
        input_id = "rankHeiSelInput",
        text = "Drag to rank parameters in desired order",
        labels = inputParSelRV$auxRankHeiSel
      )
    } else {
      NULL
    }
  })
  ### Output aux - showSlidersTestsHieSel ----
  output$showSlidersTestsHieSel <- reactive({
    !is.null(input$testsHieSelInput)
  })
  outputOptions(output, "showSlidersTestsHieSel", suspendWhenHidden = FALSE)
  ### Output aux - testsDetSelInput ----
  output$showSlidersTestsDetSel <- reactive({
    !is.null(input$testsDetSelInput)
  })
  outputOptions(output, "showSlidersTestsDetSel", suspendWhenHidden = FALSE)
  ### Output aux - showSlidersMulti ----
  output$showSlidersMulti <- reactive({
    !is.null(input$testsMultiInput)
  })
  outputOptions(output, "showSlidersMulti", suspendWhenHidden = FALSE)
  
  
  ### Update text - xvar - View tab ----
  observeEvent(input$xvarViewInput, ignoreNULL = FALSE, {
    if(!is.null(input$xvarViewInput)){
      shiny::updateTextInput(session = session,
                             inputId = "xvarLab",
                             value = input$xvarViewInput)	
    } else{
      shiny::updateTextInput(session = session,
                             inputId = "xvarLab",
                             value = "")
    }
    
  })
  ### Update text - yvar -View tab ----
  observeEvent(input$yvarViewInput, ignoreNULL = FALSE, {
    if(!is.null(input$yvarViewInput)){
      shiny::updateTextInput(session = session,
                             inputId = "yvarLab",
                             value = input$yvarViewInput)
    } else{
      shiny::updateTextInput(session = session,
                             inputId = "yvarLab",
                             value = "")
    }
  })
  ### Output aux - showTraitsData ----
  output$showTraitsData <- reactive({
    !is.null(inputDataRV[["traits"]])
  })
  outputOptions(output, "showTraitsData", suspendWhenHidden = FALSE)
  ### Output aux - showRestComp ----
  output$showRestComp <- reactive({
    !is.null(inputDataRV[["restComp"]])
  })
  outputOptions(output, "showRestComp", suspendWhenHidden = FALSE)
  ### Output aux - showRestGroup ----
  output$showRestGroup <- reactive({
    !is.null(inputDataRV[["restGroup"]])
  })
  outputOptions(output, "showRestGroup", suspendWhenHidden = FALSE)
  ### Output aux - showReference ----
  output$showReference <- reactive({
    !is.null(inputDataRV[["reference"]])
  })
  outputOptions(output, "showReference", suspendWhenHidden = FALSE)
  ### Output aux - showSupplementary ----
  output$showSupplementary <- reactive({
    !is.null(inputDataRV[["supplementary"]])
  })
  outputOptions(output, "showSupplementary", suspendWhenHidden = FALSE)
  ### Output text - countScenariosText ----
  output$countScenariosText <- shiny::renderText({
    paste0("Simulations scenarios: ",  resultsRV[["nSce"]])
  })
  ### Output text - countSimulationText ----
  output$countSimulationText <- shiny::renderText({
    paste0("Total simulations: ",  resultsRV[["nSim"]])
  })
  ### Output text - countSelectText ----
  output$countSelectText <- shiny::renderText({
    paste0("Selected scenarios: ",  resultsRV[["nSel"]])
  })
  ### Output text - countSimulationSelText ----
  output$countSimulationSelText <- shiny::renderText({
    paste0("Total simulations selected: ",  resultsRV[["nSimSel"]])
  })
  ### Output text - Simulate tab ----
  observeEvent(input$scenarioSimulateSummaryInput, {
    output$outputSimulateSummaryText <- renderUI({
      if(!is.null(input$scenarioSimulateSummaryInput)){
        x <- resultsRV$simulate[[input$scenarioSimulateSummaryInput]]
        str1 <- paste0("Pool size: ", ncol(x$simulation$composition))
        str2 <- paste0("Number of simulations: ", nrow(x$simulation$composition))
        str3 <- paste0("Reference communities: ", ifelse(is.null(x$reference), "No", "Yes"))
        str4 <- paste0("Supplementary communities: ", ifelse(is.null(x$supplementary), "No", "Yes"))
        if(!is.null(x$simulation$results)) {
          str5 <- paste0("Parameters: ")
          str6 <- paste0("&emsp;", colnames(x$simulation$results), collapse = '<br/>')
          shiny::HTML(paste(str1, str2, str3, str4, str5, str6, sep = '<br/>'))
        } else{
          str5 <- paste0("Parameters: ", ifelse(is.null(x$simulation$results), "No", "Yes"))
          shiny::HTML(paste(str1, str2, str3, str4, str5, sep = '<br/>'))
        }
      }
    })
  })
  ### Output text - Compute tab ----
  observeEvent(input$scenarioComputeSummaryInput, {
    output$outputComputeSummaryText <- renderUI({
      if(!is.null(input$scenarioComputeSummaryInput)){
        x <- resultsRV$simulate[[input$scenarioComputeSummaryInput]]
        str1 <- paste0("Pool size: ", ncol(x$simulation$composition))
        str2 <- paste0("Number of simulations: ", nrow(x$simulation$composition))
        str3 <- paste0("Reference communities: ", ifelse(is.null(x$reference), "No", "Yes"))
        str4 <- paste0("Supplementary communities: ", ifelse(is.null(x$supplementary), "No", "Yes"))
        if(!is.null(x$simulation$results)) {
          str5 <- paste0("Parameters: ")
          str6 <- paste0("&emsp;", colnames(x$simulation$results), collapse = '<br/>')
          shiny::HTML(paste(str1, str2, str3, str4, str5, str6, sep = '<br/>'))
        } else{
          str5 <- paste0("Parameters: ", ifelse(is.null(x$simulation$results), "No", "Yes"))
          shiny::HTML(paste(str1, str2, str3, str4, str5, sep = '<br/>'))
        }
      }
    })
  })
  ### Output text - Select tab ----
  observeEvent(input$scenarioSelectSummaryInput, {
    output$outputSelectSummaryText <- renderUI({
      if(!is.null(input$scenarioSelectSummaryInput)){
        x <- resultsRV$select[[input$scenarioSelectSummaryInput]]
        str1 <- paste0("Pool size: ", ncol(x$selection$composition))
        str2 <- paste0("Number of simulations selected: ", nrow(x$selection$composition))
        str3 <- paste0("Reference communities: ", ifelse(is.null(x$reference), "No", "Yes"))
        str4 <- paste0("Supplementary communities: ", ifelse(is.null(x$supplementary), "No", "Yes"))
        if(!is.null(x$selection$results)) {
          str5 <- paste0("Parameters: ")
          str6 <- paste0("&emsp;", colnames(x$selection$results), collapse = '<br/>')
          shiny::HTML(paste(str1, str2, str3, str4, str5, str6, sep = '<br/>'))
        } else{
          str5 <- paste0("Parameters: ", ifelse(is.null(x$selection$results), "No", "Yes"))
          shiny::HTML(paste(str1, str2, str3, str4, str5, sep = '<br/>'))
        }
      }
    })
  })
  ### Output table - Traits data ----
  output$outputTableTraitsData <- rhandsontable::renderRHandsontable({
    if(is.null(inputDataRV$traits)){
      return(NULL)
    }
    # rhandsontable::rhandsontable(inputDataRV$traits, contextMenu =  FALSE, readOnly = TRUE, height = 550, stretchH = "all", rowHeaderWidth = 200)
    rhandsontable::rhandsontable(inputDataRV$traits, contextMenu =  FALSE, readOnly = TRUE, stretchH = "all", rowHeaderWidth = 200)
  })
  ### Output table - Traits class ----
  output$outputTableTraitsClass <- rhandsontable::renderRHandsontable({
    if(is.null(inputDataRV$auxTraitsClass)){
      return(NULL)
    }
    rhandsontable::rhandsontable(inputDataRV$auxTraitsClass, contextMenu =  FALSE, readOnly = TRUE, height = 50, stretchH = "all", rowHeaderWidth = 200)
  })
  ### Output table - Species composition of restoration sites ----
  output$outputTableRestComp <- rhandsontable::renderRHandsontable({
    if(is.null(inputDataRV$restComp)){
      return(NULL)
    }
    rhandsontable::rhandsontable(inputDataRV$restComp, contextMenu =  FALSE, readOnly = TRUE, stretchH = "all", rowHeaderWidth = 200)
  })
  ### Output table - Complementary information for restoration sites ----
  output$outputTableRestGroup <- rhandsontable::renderRHandsontable({
    if(is.null(inputDataRV$restGroup)){
      return(NULL)
    }
    rhandsontable::rhandsontable(inputDataRV$restGroup, contextMenu =  FALSE, readOnly = TRUE, stretchH = "all", rowHeaderWidth = 200)
  })
  ### Output table - Species composition of reference sites ----
  output$outputTableRefComp <- rhandsontable::renderRHandsontable({
    if(is.null(inputDataRV$reference)){
      return(NULL)
    }
    rhandsontable::rhandsontable(inputDataRV$reference, contextMenu =  FALSE, readOnly = TRUE, stretchH = "all", rowHeaderWidth = 200)
  })
  ### Output table - Species composition of supplementary sites ----
  output$outputTableSuppleComp <- rhandsontable::renderRHandsontable({
    if(is.null(inputDataRV$supplementary)){
      return(NULL)
    }
    rhandsontable::rhandsontable(inputDataRV$supplementary, contextMenu =  FALSE, readOnly = TRUE, stretchH = "all", rowHeaderWidth = 200)
  })
  ### Output table - Export table ----
  output$outputExportTable <- rhandsontable::renderRHandsontable({
    if(is.null(exportRV$summaryTable)){
      return(NULL)
    }
    rhandsontable::rhandsontable(exportRV$summaryTable, contextMenu =  FALSE, readOnly = TRUE, stretchH = "all", rowHeaders = NULL)
  })
  ### Output plot - plotParOutput ----
  output$plotParOutput <- renderPlot({
    resultsRV$plotPar
  })
  ### Output plot - plotMultiOutput ----
  output$plotMultiOutput <- renderPlot({
    resultsRV$plotMulti
  })
})
# END appServer ----