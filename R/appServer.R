#' @rdname app
#' @export
appServer <- shiny::shinyServer(function(input, output, session) {
  ## Miscellanea ----
  # Welcome alert 
  shinyalert::shinyalert(title = NULL,
                         # text = "Welcome to resbiota",
                         text = htmltools::tagList(tags$img(src = "startup.png", width = "300px", height = "200px")),
                         className = "alertStartup",
                         type = "",
                         html = TRUE,
                         size = "xs",
                         showConfirmButton = FALSE,
                         showCancelButton = FALSE,
                         animation = FALSE,
                         timer = 3000,
                         session = session)
  # Collapse controlbar when changing the language
  observeEvent(input$selectedLanguage, {
    shinydashboardPlus::updateControlbar(id = "controlbar", session = session)
  }, ignoreInit = TRUE)
  ## Reactive Values ----
  ### Global variables ----
  globalRV <- shiny::reactiveValues(digitsVal = 5, 
                                    digitsMin = 3,
                                    currentDate = format(Sys.Date(), "%Y%m%d"),
                                    countPlots = 0,
                                    probs = c(0, 0.25, 0.5, 0.75, 1))
  #### Set the minimal decimal places ----
  numVal <- shiny::reactive({
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
  output$decimalPlaces <- shiny::renderUI(numericInput(inputId = "decimalPlaces", 
                                                       # label = "Decimal places", 
                                                       label = i18n$t("Decimal places"),
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
  ### infoRV ----
  infoRV <- shiny::reactiveValues(
    # DataInputTab
    traitsInputInfo = 0,
    restCompInputInfo = 0,
    restGroupInputInfo = 0,
    referenceInputInfo = 0,
    supplementaryInputInfo = 0,
    # simulateTab
    goalsSimInputInfo = 0,
    methodSimInputInfo = 0,
    nIndSimInputInfo = 0,
    richSliderSimInputInfo = 0,
    itSimInputInfo = 0,
    avaSimInputInfo = 0,
    undSimInputInfo = 0,
    cwmSimInputInfo = 0,
    raoSimInputInfo = 0,
    speficyGroupsSimInputInfo = 0,
    probGroupTypeSimInputInfo = 0,
    probSimInputInfo = 0,
    cvAbundSimInputInfo = 0,
    phiSimInputInfo = 0,
    minAbuSliderSimAdjInputInfo = 0,
    reallocateAdjSimInputInfo = 0,
    # computeTab
    avaComInputInfo = 0,
    cwmComInputInfo = 0,
    cwvComInputInfo = 0,
    raoComInputInfo = 0,
    disComInputInfo = 0,
    costComInputInfo = 0,
    densComInputInfo = 0,
    stanComParInputInfo = 0,
    speficyMethodStanInputInfo = 0,
    testsMultiInputInfo = 0,
    # SelectTab
    testsDetSelInputInfo = 0,
    testsHieSelInputInfo = 0,
    speficyGroupsSelInputInfo = 0,
    singleSelectionInputInfo = 0
  )
  ## Input file ----
  ### Input file - Traits data ----
  shiny::observeEvent(input$traitsInput, {
    # Read file
    inFile <- input$traitsInput
    if (is.null(inFile)){
      inputDataRV$traits <- NULL
    }
    inputDataRV$traits <- utils::read.csv(inFile$datapath, sep = input$fileSep, row.names = 1)
  }) # End input file
  ### Input file - restComp ----
  shiny::observeEvent(input$restCompInput, {
    # Read file
    inFile <- input$restCompInput
    if (is.null(inFile)){
      inputDataRV$restComp <- NULL
    }
    inputDataRV$restComp <- utils::read.csv(inFile$datapath, sep = input$fileSep, row.names = 1)
  }) # End input file
  ### Input file - restGroup ----
  shiny::observeEvent(input$restGroupInput, {
    # Read file
    inFile <- input$restGroupInput
    if (is.null(inFile)){
      inputDataRV$restGroup <- NULL
    }
    inputDataRV$restGroup <- utils::read.csv(inFile$datapath, sep = input$fileSep, row.names = 1)
  }) # End input file
  ### Input file - Reference ----
  shiny::observeEvent(input$referenceInput, {
    # Read file
    inFile <- input$referenceInput
    if (is.null(inFile)){
      inputDataRV$reference <- NULL
    }
    inputDataRV$reference <- utils::read.csv(inFile$datapath, sep = input$fileSep, row.names = 1)
  }) # End input file
  ### Input file - Supplementary ----
  shiny::observeEvent(input$supplementaryInput, {
    # Read file
    inFile <- input$supplementaryInput
    if (is.null(inFile)){
      inputDataRV$supplementary <- NULL
    }
    inputDataRV$supplementary <- utils::read.csv(inFile$datapath, sep = input$fileSep, row.names = 1)
  }) # End input file
  ## doClear buttons ----
  ### doClear CONTINUAR ----
  # remover tambem outros elementos da interface
  shiny::observeEvent(input$doClear, {
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
  shiny::observeEvent(input$doClearTraits, {
    inputDataRV$traits <- NULL
    inputDataRV$auxTraitsClass <- NULL
    inputDataRV$auxTraitsVariables <- NULL
    shinyjs::reset("traitsInput")
  })
  ### doClearRestComp ----
  shiny::observeEvent(input$doClearRestComp, {
    inputDataRV$restComp <- NULL
    shinyjs::reset("restCompInput")
  })
  ### doClearRestGroup ----
  shiny::observeEvent(input$doClearRestGroup, {
    inputDataRV$restGroup <- NULL
    shinyjs::reset("restGroupInput")
  })
  ### doClearReference ----
  shiny::observeEvent(input$doClearReference, {
    inputDataRV$reference <- NULL
    shinyjs::reset("referenceInput")
  })
  ### doClearSupplementary ----
  shiny::observeEvent(input$doClearSupplementary, {
    inputDataRV$supplementary <- NULL
    shinyjs::reset("supplementaryInput")
  })
  ## Update pickers - Traits input ----
  shiny::observeEvent(inputDataRV$traits, ignoreNULL = FALSE, {
    if(!is.null(inputDataRV$traits)){
      # Extract basic data information
      inputDataRV$auxTraitsVariables <- colnames(inputDataRV$traits)
      inputDataRV$auxTraitsClass <- data.frame(t(sapply(inputDataRV$traits, vectorClass)), row.names = "Class")
      # Update slider
      shinyWidgets::updateSliderTextInput(session, inputId = "richSliderSimInput",
                                          choices = seq_len(nrow(inputDataRV$traits)),
                                          selected = c(1, nrow(inputDataRV$traits)))
    } else{
      inputDataRV$auxTraitsVariables <- character(0)
      inputDataRV$auxTraitsClass <- character(0)
      # Update slider
      shinyWidgets::updateSliderTextInput(session, inputId = "richSliderSimInput",
                                          choices = c(1, 1),
                                          selected = c(1, 1))
    }
    # Update pickers
    shinyWidgets::updatePickerInput(session, inputId = "avaSimInput", choices = inputDataRV$auxTraitsVariables,
                                    choicesOpt = list(subtext = inputDataRV$auxTraitsClass))
    shinyWidgets::updatePickerInput(session, inputId = "undSimInput", choices = inputDataRV$auxTraitsVariables,
                                    choicesOpt = list(subtext = inputDataRV$auxTraitsClass))
    shinyWidgets::updatePickerInput(session, inputId = "cwmSimInput", choices = inputDataRV$auxTraitsVariables,
                                    choicesOpt = list(subtext = inputDataRV$auxTraitsClass))
    shinyWidgets::updatePickerInput(session, inputId = "raoSimInput", choices = inputDataRV$auxTraitsVariables,
                                    choicesOpt = list(subtext = inputDataRV$auxTraitsClass))
    shinyWidgets::updatePickerInput(session, inputId = "probSimInput", choices = inputDataRV$auxTraitsVariables,
                                    choicesOpt = list(subtext = inputDataRV$auxTraitsClass))
    shinyWidgets::updatePickerInput(session, inputId = "groupSimInput", choices = inputDataRV$auxTraitsVariables,
                                    choicesOpt = list(subtext = inputDataRV$auxTraitsClass))
    shinyWidgets::updatePickerInput(session, inputId = "avaComInput", choices = inputDataRV$auxTraitsVariables,
                                    choicesOpt = list(subtext = inputDataRV$auxTraitsClass))
    shinyWidgets::updatePickerInput(session, inputId = "cwmComInput", choices = inputDataRV$auxTraitsVariables,
                                    choicesOpt = list(subtext = inputDataRV$auxTraitsClass))
    shinyWidgets::updatePickerInput(session, inputId = "cwvComInput", choices = inputDataRV$auxTraitsVariables,
                                    choicesOpt = list(subtext = inputDataRV$auxTraitsClass))
    shinyWidgets::updatePickerInput(session, inputId = "raoComInput", choices = inputDataRV$auxTraitsVariables,
                                    choicesOpt = list(subtext = inputDataRV$auxTraitsClass))
    shinyWidgets::updatePickerInput(session, inputId = "costComInput", choices = inputDataRV$auxTraitsVariables,
                                    choicesOpt = list(subtext = inputDataRV$auxTraitsClass))
    shinyWidgets::updatePickerInput(session, inputId = "densComInput", choices = inputDataRV$auxTraitsVariables,
                                    choicesOpt = list(subtext = inputDataRV$auxTraitsClass))
    shinyWidgets::updatePickerInput(session, inputId = "disComInput", choices = inputDataRV$auxTraitsVariables,
                                    choicesOpt = list(subtext = inputDataRV$auxTraitsClass))
    shinyWidgets::updatePickerInput(session, inputId = "avaExpInput", choices = inputDataRV$auxTraitsVariables,
                                    choicesOpt = list(subtext = inputDataRV$auxTraitsClass))
  })
  
  ## Update pickers - Min abun input ----
  # shiny::observeEvent(input$scenarioSimAdjInput, ignoreNULL = TRUE, {
  # 	# 
  # 	# minAbuSliderSimAdjInput
  # 	# 
  # 	scenario <- resultsRV$simulate[[input$scenarioSimAdjInput]]
  # 	# min(scenario$scenario$simulation$composition)
  # 	print(max(scenario$simulation$composition))
  # 	maxVal <- max(scenario$simulation$composition)
  # 	# 
  # 	# fi
  # 	# 
  # 	if(maxVal>1){
  # 		seqVal <- seq(from = 0, to = maxVal, by = 1)
  # 	} else{
  # 		seqVal <- seq(from = 0, to = 1, length.out = 100)
  # 	}
  # 	# Update slider
  # 	shinyWidgets::updateSliderTextInput(session, inputId = "minAbuSliderSimAdjInput",
  # 										choices = seqVal,
  # 										selected = 0)
  # 	# if(is.null(input$scenarioSimAdjInput)){
  # 	# 	shiny::updateActionButton(session, "doAdjustSim", disabled = TRUE)
  # 	# } else(
  # 	# 	shiny::updateActionButton(session, "doAdjustSim", disabled = FALSE)
  # 	# )
  # })
  ## Selected language ----
  shiny::observeEvent(input$selectedLanguage, {
    shiny.i18n::update_lang(input$selectedLanguage, session)
  })
  ## Update pickers - Scenarios ----
  ### Scenarios pickers ----
  # Observe changes in any scenario
  # Update choices and keep selected
  obsListAllScenarios <- shiny::reactive({
    list(resultsRV$simulate, resultsRV$select)
  })
  shiny::observeEvent(obsListAllScenarios(), {
    # Simulate tab
    shinyWidgets::updatePickerInput(session, inputId = "mergeSimulateInput", choices = names(resultsRV$simulate),
                                    selected = input$mergeSimulateInput)
    shinyWidgets::updatePickerInput(session, inputId = "removeSimulateInput", choices = names(resultsRV$simulate),
                                    selected = input$removeSimulateInput)
    shinyWidgets::updatePickerInput(session, inputId = "scenarioSimulateSummaryInput", choices = names(resultsRV$simulate),
                                    selected = input$scenarioSimulateSummaryInput)
    shinyWidgets::updatePickerInput(session, inputId = "scenarioSimAdjInput", choices = names(resultsRV$simulate),
                                    selected = input$scenarioSimAdjInput)
    # Compute tab
    shinyWidgets::updatePickerInput(session, inputId = "scenarioComParInput", choices = names(resultsRV$simulate), 
                                    selected = input$scenarioComParInput)
    shinyWidgets::updatePickerInput(session, inputId = "scenarioComStandParInput", choices = names(resultsRV$simulate), 
                                    selected = input$scenarioComStandParInput)
    shinyWidgets::updatePickerInput(session, inputId = "scenarioComMultiInput", choices = names(resultsRV$simulate),
                                    selected = input$scenarioComMultiInput)
    shinyWidgets::updatePickerInput(session, inputId = "scenarioComputeSummaryInput", choices = names(resultsRV$simulate),
                                    selected = input$scenarioComputeSummaryInput)
    # Select tab
    shinyWidgets::updatePickerInput(session, inputId = "scenarioSelInput", choices = names(resultsRV$simulate),
                                    selected = input$scenarioSelInput)
    shinyWidgets::updatePickerInput(session, inputId = "mergeSelectInput", choices = names(resultsRV$select),
                                    selected = input$mergeSelectInput)
    shinyWidgets::updatePickerInput(session, inputId = "removeSelectInput", choices = names(resultsRV$select),
                                    selected = input$removeSelectInput)
    shinyWidgets::updatePickerInput(session, inputId = "scenarioSelectSummaryInput", choices = names(resultsRV$select),
                                    selected = input$scenarioSelectSummaryInput)
    # View tab
    if(!is.null(input$scenarioTypeViewParInput)){
      if(input$scenarioTypeViewParInput == "Raw"){
        shinyWidgets::updatePickerInput(session, inputId = "scenarioViewParInput", choices = names(resultsRV$simulate),
                                        selected = input$scenarioViewParInput)	
      } else{
        shinyWidgets::updatePickerInput(session, inputId = "scenarioViewParInput", choices = names(resultsRV$select),
                                        selected = input$scenarioViewParInput)
      }
    }
    if(!is.null(input$scenarioTypeViewMultiInput)){
      if(input$scenarioTypeViewMultiInput == "Raw"){
        shinyWidgets::updatePickerInput(session, inputId = "scenarioViewMultiInput", choices = names(resultsRV$simulate),
                                        selected = input$scenarioViewMultiInput)	
      } else{
        shinyWidgets::updatePickerInput(session, inputId = "scenarioViewMultiInput", choices = names(resultsRV$select),
                                        selected = input$scenarioViewMultiInput)
      }
    }
    # Export tab
    if(!is.null(input$scenarioTypeExportInput)){
      if(input$scenarioTypeExportInput == "Raw"){
        shinyWidgets::updatePickerInput(session, inputId = "scenarioExportInput", choices = names(resultsRV$simulate),
                                        selected = input$scenarioExportInput)	
      } else{
        shinyWidgets::updatePickerInput(session, inputId = "scenarioExportInput", choices = names(resultsRV$select),
                                        selected = input$scenarioExportInput)
      }
    }
  })
  ### View tab - scenarioTypeViewParInput ----
  shiny::observeEvent(input$scenarioTypeViewParInput, {
    if(!is.null(input$scenarioTypeViewParInput)){
      if(input$scenarioTypeViewParInput == "Raw"){
        if(!is.null(names(resultsRV$simulate))){
          shinyWidgets::updatePickerInput(session, inputId = "scenarioViewParInput", choices = names(resultsRV$simulate),
                                          selected = NULL)	
        } else{
          shinyWidgets::updatePickerInput(session, inputId = "scenarioViewParInput", choices = character(0),
                                          selected = NULL)
        }
      } else{
        if(!is.null(names(resultsRV$select))){
          shinyWidgets::updatePickerInput(session, inputId = "scenarioViewParInput", choices = names(resultsRV$select),
                                          selected = NULL)	
        } else{
          shinyWidgets::updatePickerInput(session, inputId = "scenarioViewParInput", choices = character(0),
                                          selected = NULL)
        }
      }
    }
  })
  ### View tab - scenarioTypeViewMultiInput ----
  shiny::observeEvent(input$scenarioTypeViewMultiInput, {
    if(!is.null(input$scenarioTypeViewMultiInput)){
      if(input$scenarioTypeViewMultiInput == "Raw"){
        if(!is.null(names(resultsRV$simulate))){
          shinyWidgets::updatePickerInput(session, inputId = "scenarioViewMultiInput", choices = names(resultsRV$simulate),
                                          selected = NULL)	
        } else{
          shinyWidgets::updatePickerInput(session, inputId = "scenarioViewMultiInput", choices = character(0),
                                          selected = NULL)	
        }
      } else{
        if(!is.null(names(resultsRV$select))){
          shinyWidgets::updatePickerInput(session, inputId = "scenarioViewMultiInput", choices = names(resultsRV$select),
                                          selected = NULL)	
        } else{
          shinyWidgets::updatePickerInput(session, inputId = "scenarioViewMultiInput", choices = character(0),
                                          selected = NULL)
        }
      }
    }
  })
  ### Export tab - scenarioTypeExportInput ----
  shiny::observeEvent(input$scenarioTypeExportInput, {
    if(!is.null(input$scenarioTypeExportInput)){
      if(input$scenarioTypeExportInput == "Raw"){
        if(!is.null(names(resultsRV$simulate))){
          shinyWidgets::updatePickerInput(session, inputId = "scenarioExportInput", choices = names(resultsRV$simulate),
                                          selected = NULL)		
        } else{
          shinyWidgets::updatePickerInput(session, inputId = "scenarioExportInput", choices = character(0),
                                          selected = NULL)	
        }
      } else{
        if(!is.null(names(resultsRV$select))){
          shinyWidgets::updatePickerInput(session, inputId = "scenarioExportInput", choices = names(resultsRV$select),
                                          selected = NULL)	
        } else{
          shinyWidgets::updatePickerInput(session, inputId = "scenarioExportInput", choices = character(0),
                                          selected = NULL)
        }
      }
    }
  })
  ## Update pickers - Parameters ----
  ### Select tab - testsDetSelInput, testsHieSelInput and groupSelInput ----
  # Update choices 
  obsListSelPar <- shiny::reactive({
    list(input$scenarioSelInput, resultsRV$updatePar)
  })
  shiny::observeEvent(obsListSelPar(), {
    if(!is.null(input$scenarioSelInput)){
      scenario <- resultsRV$simulate[[input$scenarioSelInput]]
      if(!is.null(colnames(scenario$simulation$results))){
        shinyWidgets::updatePickerInput(session, inputId = "testsDetSelInput", choices = colnames(scenario$simulation$results))
        shinyWidgets::updatePickerInput(session, inputId = "testsHieSelInput", choices = colnames(scenario$simulation$results))
        shinyWidgets::updatePickerInput(session, inputId = "groupSelInput", choices = colnames(scenario$simulation$results))
      } else{
        shinyWidgets::updatePickerInput(session, inputId = "testsDetSelInput", choices = character(0),
                                        selected = NULL)
        shinyWidgets::updatePickerInput(session, inputId = "testsHieSelInput", choices = character(0),
                                        selected = NULL)
        shinyWidgets::updatePickerInput(session, inputId = "groupSelInput", choices = character(0),
                                        selected = NULL)
      }
    } else{
      shinyWidgets::updatePickerInput(session, inputId = "testsDetSelInput", choices = character(0),
                                      selected = NULL)
      shinyWidgets::updatePickerInput(session, inputId = "testsHieSelInput", choices = character(0),
                                      selected = NULL)
      shinyWidgets::updatePickerInput(session, inputId = "groupSelInput", choices = character(0),
                                      selected = NULL)
    }
  })
  ### Compute tab - testsMultiInput ----
  # Update choices 
  obsListComMulti <- shiny::reactive({
    list(input$scenarioComMultiInput, resultsRV$updatePar)
  })
  shiny::observeEvent(obsListComMulti(), {
    if(!is.null(input$scenarioComMultiInput)){
      scenario <- resultsRV$simulate[[input$scenarioComMultiInput]]
      if(!is.null(colnames(scenario$simulation$results))){
        shinyWidgets::updatePickerInput(session, inputId = "testsMultiInput", choices = colnames(scenario$simulation$results))
      } else{
        shinyWidgets::updatePickerInput(session, inputId = "testsMultiInput", choices = character(0),
                                        selected = NULL)	
      }
    } else{
      shinyWidgets::updatePickerInput(session, inputId = "testsMultiInput", choices = character(0),
                                      selected = NULL)
    }
  })
  ### Compute tab - stanComParInput----
  # Update choices 
  obsListComStandPar <- shiny::reactive({
    list(input$scenarioComStandParInput, resultsRV$updatePar)
  })
  shiny::observeEvent(obsListComStandPar(), {
    if(!is.null(input$scenarioComStandParInput)){
      scenario <- resultsRV$simulate[[input$scenarioComStandParInput]]
      if(!is.null(colnames(scenario$simulation$results))){
        shinyWidgets::updatePickerInput(session, inputId = "stanComParInput", choices = colnames(scenario$simulation$results))	
      } else{
        shinyWidgets::updatePickerInput(session, inputId = "stanComParInput", choices = character(0),
                                        selected = NULL)	
      }
    } else{
      shinyWidgets::updatePickerInput(session, inputId = "stanComParInput", choices = character(0),
                                      selected = NULL)
    }
  })
  ### View tab - xvarViewInput and yvarViewInput ----
  # Update choices 
  obsListViewPar <- shiny::reactive({
    list(input$scenarioViewParInput, resultsRV$updatePar)
  })
  shiny::observeEvent(obsListViewPar(), {
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
      }
      if(!is.null(res)){
        shinyWidgets::updatePickerInput(session, inputId = "xvarViewInput", choices = colnames(res))
        shinyWidgets::updatePickerInput(session, inputId = "yvarViewInput", choices = colnames(res))
      } else{
        shinyWidgets::updatePickerInput(session, inputId = "xvarViewInput", choices = character(0), selected = NULL)
        shinyWidgets::updatePickerInput(session, inputId = "yvarViewInput", choices = character(0), selected = NULL)
      }
    } else{
      shinyWidgets::updatePickerInput(session, inputId = "xvarViewInput", choices = character(0), selected = NULL)
      shinyWidgets::updatePickerInput(session, inputId = "yvarViewInput", choices = character(0), selected = NULL)
    }
  })
  ## Create dynamic slides ----
  ### Group probability sliders ----
  shiny::observeEvent(input$groupSimInput, {
    output$slidersProbRicSim <- shiny::renderUI({
      inVars <- unique(inputDataRV$traits[, input$groupSimInput])
      pvars <- length(inVars)
      if (pvars > 0) {
        lapply(seq(pvars), function(i) {
          shiny::sliderInput(inputId = paste0("probRichSimGrop", inVars[i]), 
                             label = paste0(i18n$t("Probability to draw richness - Group: "), inVars[i]), 
                             value = 1/length(inVars),
                             min = 0,
                             max = 1)
        })
      }
    })
    output$slidersProbAbuSim <- shiny::renderUI({
      inVars <- unique(inputDataRV$traits[, input$groupSimInput])
      pvars <- length(inVars)
      if (pvars > 0) {
        lapply(seq(pvars), function(i) {
          shiny::sliderInput(inputId = paste0("probAbunSimGrop", inVars[i]), 
                             label = paste0(i18n$t("Probability to draw abundance - Group: "), inVars[i]), 
                             value = 1/length(inVars),
                             min = 0,
                             max = 1)
        })
      }
    })
  })
  ### Hierarchical selection sliders ----
  shiny::observeEvent(input$testsHieSelInput, {
    output$slidersTestsHieSel <- shiny::renderUI({
      inVars <- input$testsHieSelInput
      pvars <- length(inVars)
      scenario <- resultsRV$simulate[[input$scenarioSelInput]]
      scenario <- scenario$simulation$results
      if (pvars > 0) {
        lapply(seq(pvars), function(i) {
          if(vectorClass(scenario[,inVars[i]]) == "numeric"){
            shiny::observeEvent(input[[paste0("logicalTestHieSelInput", inVars[i], "chart")]], {
              output$plotVarModal <- shiny::renderPlot({
                quantiles <- stats::quantile(scenario[[inVars[i]]], prob = globalRV$probs, na.rm = TRUE)
                df.quantiles <- data.frame(q = quantiles, label = paste0(round(quantiles, 3), " \n q = ", globalRV$probs))
                ggplot2::ggplot(data = scenario) +
                  ggplot2::aes(x = .data[[inVars[i]]]) +
                  ggplot2::geom_histogram(bins = grDevices::nclass.FD(scenario[[inVars[i]]][!is.na(scenario[[inVars[i]]])]), col = "#ffffff", fill = "#1d4b61") +
                  ggplot2::scale_x_continuous(breaks = df.quantiles$q, labels = df.quantiles$label, guide = ggplot2::guide_axis(n.dodge = 2)) +
                  themeResbiota(baseSize = 15)
              })
              shiny::showModal(shiny::modalDialog(shiny::plotOutput("plotVarModal"),
                                                  footer = shiny::modalButton(i18n$t("Dismiss")),
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
                                                                                   style = "padding:4px; font-size:60%")),
                                          choices = choicesTemp,
                                          selected = selectedTemp
            )
          } else {
            shiny::observeEvent(input[[paste0("logicalTestHieSelInput", inVars[i], "chart")]], {
              output$plotVarModal <- shiny::renderPlot({
                ggplot2::ggplot(data = scenario) +
                  ggplot2::aes(x = .data[[inVars[i]]]) +
                  ggplot2::geom_bar(fill = "#1d4b61") +
                  themeResbiota(baseSize = 15)
              })
              shiny::showModal(shiny::modalDialog(shiny::plotOutput("plotVarModal"),
                                                  footer = shiny::modalButton("Dismiss"),
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
                                                                               style = "padding:4px; font-size:60%")),
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
  shiny::observeEvent(input$testsDetSelInput, {
    output$slidersTestsDetSel <- shiny::renderUI({
      inVars <- input$testsDetSelInput
      pvars <- length(inVars)
      scenario <- resultsRV$simulate[[input$scenarioSelInput]]
      scenario <- scenario$simulation$results
      if (pvars > 0) {
        lapply(seq(pvars), function(i) {
          if(vectorClass(scenario[,inVars[i]]) == "numeric"){
            shiny::observeEvent(input[[paste0("logicalTestDetSelInput", inVars[i], "chart")]], {
              output$plotVarModal <- shiny::renderPlot({
                quantiles <- stats::quantile(scenario[[inVars[i]]], prob = globalRV$probs, na.rm = TRUE)
                df.quantiles <- data.frame(q = quantiles, label = paste0(round(quantiles, 3), " \n q = ", globalRV$probs))
                ggplot2::ggplot(data = scenario) +
                  ggplot2::aes(x = .data[[inVars[i]]]) +
                  ggplot2::geom_histogram(bins = grDevices::nclass.FD(scenario[[inVars[i]]][!is.na(scenario[[inVars[i]]])]), col = "#ffffff", fill = "#1d4b61") +
                  ggplot2::scale_x_continuous(breaks = df.quantiles$q, labels = df.quantiles$label, guide = ggplot2::guide_axis(n.dodge = 2)) +
                  themeResbiota(baseSize = 15)
              })
              shiny::showModal(shiny::modalDialog(shiny::plotOutput("plotVarModal"),
                                                  footer = shiny::modalButton(i18n$t("Dismiss")),
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
                                                                                   style = "padding:4px; font-size:60%")),
                                          choices = choicesTemp,
                                          selected = selectedTemp
            )
          } else {
            shiny::observeEvent(input[[paste0("logicalTestDetSelInput", inVars[i], "chart")]], {
              output$plotVarModal <- shiny::renderPlot({
                ggplot2::ggplot(data = scenario) +
                  ggplot2::aes(x = .data[[inVars[i]]]) +
                  ggplot2::geom_bar(fill = "#1d4b61") +
                  themeResbiota(baseSize = 15)
              })
              shiny::showModal(shiny::modalDialog(shiny::plotOutput("plotVarModal"),
                                                  footer = shiny::modalButton(i18n$t("Dismiss")),
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
                                                                               style = "padding:4px; font-size:60%")),
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
  shiny::observeEvent(input$testsMultiInput, {
    output$slidersMulti <- shiny::renderUI({
      inVars <- input$testsMultiInput
      pvars <- length(inVars)
      scenario <- resultsRV$simulate[[input$scenarioComMultiInput]]
      scenario <- scenario$simulation$results
      if (pvars > 0) {
        lapply(seq(pvars), function(i) {
          if(vectorClass(scenario[,inVars[i]]) == "numeric"){
            shiny::observeEvent(input[[paste0("logicalTestMultiInput", inVars[i], "chart")]], {
              output$plotVarModal <- shiny::renderPlot({
                quantiles <- stats::quantile(scenario[[inVars[i]]], prob = globalRV$probs, na.rm = TRUE)
                df.quantiles <- data.frame(q = quantiles, label = paste0(round(quantiles, 3), " \n q = ", globalRV$probs))
                ggplot2::ggplot(data = scenario) +
                  ggplot2::aes(x = .data[[inVars[i]]]) +
                  ggplot2::geom_histogram(bins = grDevices::nclass.FD(scenario[[inVars[i]]][!is.na(scenario[[inVars[i]]])]), col = "#ffffff", fill = "#1d4b61") +
                  ggplot2::scale_x_continuous(breaks = df.quantiles$q, labels = df.quantiles$label, guide = ggplot2::guide_axis(n.dodge = 2)) +
                  themeResbiota(baseSize = 15)
              })
              shiny::showModal(shiny::modalDialog(shiny::plotOutput("plotVarModal"),
                                                  footer = shiny::modalButton(i18n$t("Dismiss")),
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
                                                                                   style = "padding:4px; font-size:60%")),
                                          choices = choicesTemp,
                                          selected = selectedTemp
            )
          } else {
            shiny::observeEvent(input[[paste0("logicalTestMultiInput", inVars[i], "chart")]], {
              output$plotVarModal <- shiny::renderPlot({
                ggplot2::ggplot(data = scenario) +
                  ggplot2::aes(x = .data[[inVars[i]]]) +
                  ggplot2::geom_bar(fill = "#1d4b61") +
                  themeResbiota(baseSize = 15)
              })
              shiny::showModal(shiny::modalDialog(shiny::plotOutput("plotVarModal"),
                                                  footer = shiny::modalButton(i18n$t("Dismiss")),
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
                                                                               style = "padding:4px; font-size:60%")),
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
  ### Update labelMultiOutput  ----
  shiny::observeEvent(input$scenarioViewMultiInput, ignoreNULL = FALSE, {
    output$labelMultiOutput <- shiny::renderUI({
      if(!is.null(input$scenarioViewMultiInput)){
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
          inVars <- colnames(resMulti)[-1]
          pvars <- length(inVars)
          if (pvars > 0) {
            lapply(seq(pvars), function(i) {
              shiny::textInput(inputId = paste0("labMulti", inVars[i]), 
                               label = i18n$t("Label"), 
                               value = inVars[i])
            })
          }
        }
      } 
    })
  })
  ## Create dynamic radio buttons and select input ----
  # Inside server to allow the use of translate functions
  ### goalsSimInput ----
  output$radioGoalsSimOutput <- renderUI({
    shinyWidgets::prettyRadioButtons(inputId = "goalsSimInput",
                                     # label = "Restoration goals",
                                     label = htmltools::p(i18n$t("Restoration goals"),
                                                          shiny::actionButton("goalsSimInputInfo",
                                                                              label = "",
                                                                              icon = shiny::icon("info"),
                                                                              style = "padding:3px; font-size:60%")),
                                     choices = stats::setNames(
                                       c("New", "Ongoing"),
                                       c(i18n$t("New"), i18n$t("Ongoing")) # Set labels
                                     ),
                                     selected = "New",
                                     inline = TRUE,
                                     status = "primary"
    )
  })
  
  output$radioMethodSimOutput <- renderUI({
    shinyWidgets::prettyRadioButtons(inputId = "methodSimInput",
                                     # label = "Method",
                                     label = htmltools::p(i18n$t("Method"), 
                                                          shiny::actionButton("methodSimInputInfo",
                                                                              label = "",
                                                                              icon = shiny::icon("info"),
                                                                              style = "padding:3px; font-size:60%")),
                                     choices = stats::setNames(
                                       c("Proportions", "Individuals"),
                                       c(i18n$t("Proportions"), i18n$t("Individuals")) # Set labels
                                     ),
                                     selected = "Proportions",
                                     inline = TRUE,
                                     status = "primary"
    )
  })
  
  output$radioSpeficyGroupsSimOutput <- renderUI({
    shinyWidgets::prettyRadioButtons(inputId = "speficyGroupsSimInput",
                                     # label = "Specify probabilities for groups of species",
                                     label = htmltools::p(i18n$t("Specify probabilities for groups of species"), 
                                                          shiny::actionButton("speficyGroupsSimInputInfo",
                                                                              label = "",
                                                                              icon = shiny::icon("info"),
                                                                              style = "padding:3px; font-size:60%")),
                                     choices = stats::setNames(
                                       c("Yes", "No"),
                                       c(i18n$t("Yes"), i18n$t("No")) # Set labels
                                     ),
                                     selected = "No",
                                     inline = TRUE,
                                     status = "primary"
    )
  })
  
  output$radioProbGroupTypeSimOutput <- renderUI({
    shinyWidgets::prettyRadioButtons(inputId = "probGroupTypeSimInput",
                                     # label = "Probabilities to draw species",
                                     label = htmltools::p(i18n$t("Probabilities to draw species"), 
                                                          shiny::actionButton("probGroupTypeSimInputInfo",
                                                                              label = "",
                                                                              icon = shiny::icon("info"),
                                                                              style = "padding:3px; font-size:60%")),
                                     choices = stats::setNames(
                                       c("Abundance", "Richness and abundance"),
                                       c(i18n$t("Abundance"), i18n$t("Richness and abundance")) # Set labels
                                     ),
                                     selected = "Abundance",
                                     inline = TRUE,
                                     status = "primary"
    )
  })
  
  
  output$radioScenarioSimAdjOutput <- renderUI({
    shinyWidgets::prettyRadioButtons(inputId = "reallocateAdjSimInput",
                                     # label = "Reallocate removed individuals",
                                     label = htmltools::p(i18n$t("Reallocate removed individuals"),
                                                          shiny::actionButton("reallocateAdjSimInputInfo",
                                                                              label = "",
                                                                              icon = shiny::icon("info"),
                                                                              style = "padding:3px; font-size:60%")),
                                     choices = stats::setNames(
                                       c(TRUE, FALSE),
                                       c(i18n$t("Yes"), i18n$t("No")) # Set labels
                                     ),
                                     selected = FALSE,
                                     inline = TRUE,
                                     status = "primary"
    )
  })
  
  
  output$radioSpeficyMethodStanOutput <- renderUI({
    shinyWidgets::prettyRadioButtons(inputId = "speficyMethodStanInput",
                                     # label = "Standardization method",
                                     label = htmltools::p(i18n$t("Standardization method"), 
                                                          shiny::actionButton("speficyMethodStanInputInfo",
                                                                              label = "",
                                                                              icon = shiny::icon("info"),
                                                                              style = "padding:3px; font-size:60%")),
                                     choices = stats::setNames(
                                       c("max", "standardize"),
                                       c(i18n$t("Maximum"), i18n$t("Standardize")) # Set labels
                                     ),
                                     selected = "max",
                                     inline = TRUE,
                                     status = "primary"
    )
  })
  
  output$radioSpeficyGroupsSelOutput <- renderUI({
    shinyWidgets::prettyRadioButtons(inputId = "speficyGroupsSelInput",
                                     # label = "Selection inside sites groups",
                                     label = htmltools::p(i18n$t("Selection inside sites groups"), 
                                                          shiny::actionButton("speficyGroupsSelInputInfo",
                                                                              label = "",
                                                                              icon = shiny::icon("info"),
                                                                              style = "padding:3px; font-size:60%")),
                                     choices = stats::setNames(
                                       c("Yes", "No"),
                                       c(i18n$t("Yes"), i18n$t("No")) # Set labels
                                     ),
                                     selected = "No",
                                     inline = TRUE,
                                     status = "primary"
    )
  })
  
  output$radioSingleSelectionSelOutput <- renderUI({
    shinyWidgets::prettyRadioButtons(inputId = "singleSelectionInput",
                                     # label = "Selection method",
                                     label = htmltools::p(i18n$t("Selection method"), 
                                                          shiny::actionButton("singleSelectionInputInfo",
                                                                              label = "",
                                                                              icon = shiny::icon("info"),
                                                                              style = "padding:3px; font-size:60%")),
                                     choices = stats::setNames(
                                       c(TRUE, FALSE),
                                       c(i18n$t("Single"), i18n$t("Multiple")) # Set labels
                                     ),
                                     selected = TRUE,
                                     inline = TRUE,
                                     status = "primary"
    )
  })
  
  output$radioScenarioTypeViewParOutput <- renderUI({
    shinyWidgets::prettyRadioButtons(inputId = "scenarioTypeViewParInput",
                                     label = i18n$t("Scenario type"),
                                     choices = stats::setNames(
                                       c("Raw", "Selected"),
                                       c(i18n$t("Raw"), i18n$t("Selected")) # Set labels
                                     ),
                                     selected = "Raw",
                                     inline = TRUE,
                                     status = "primary"
    )
  })
  
  output$radioHideRefViewParOutput <- renderUI({
    shinyWidgets::prettyRadioButtons(inputId = "hideRefViewParInput",
                                     label = i18n$t("Hide reference sites"),
                                     choices = stats::setNames(
                                       c(TRUE, FALSE),
                                       c(i18n$t("Yes"), i18n$t("No")) # Set labels
                                     ),
                                     selected = FALSE,
                                     inline = TRUE,
                                     status = "primary"
    )
  })
  
  output$radioScenarioTypeViewMultiOutput <- renderUI({
    shinyWidgets::prettyRadioButtons(inputId = "scenarioTypeViewMultiInput",
                                     label = i18n$t("Scenario type"),
                                     choices = stats::setNames(
                                       c("Raw", "Selected"),
                                       c(i18n$t("Raw"), i18n$t("Selected")) # Set labels
                                     ),
                                     selected = "Raw",
                                     inline = TRUE,
                                     status = "primary"
    )
  })
  
  output$radioHideRefViewMultiOutput <- renderUI({
    shinyWidgets::prettyRadioButtons(inputId = "hideRefViewMultiInput",
                                     label = i18n$t("Hide reference sites"),
                                     choices = stats::setNames(
                                       c(TRUE, FALSE),
                                       c(i18n$t("Yes"), i18n$t("No")) # Set labels
                                     ),
                                     selected = FALSE,
                                     inline = TRUE,
                                     status = "primary"
    )
  })
  
  
  
  output$radioScenarioTypeExportOutput <- renderUI({
    shinyWidgets::prettyRadioButtons(inputId = "scenarioTypeExportInput",
                                     label = i18n$t("Scenario type"),
                                     choices = stats::setNames(
                                       c("Raw", "Selected"),
                                       c(i18n$t("Raw"), i18n$t("Selected")) # Set labels
                                     ),
                                     selected = "Raw",
                                     inline = TRUE,
                                     status = "primary"
    )
  })
  
  
  
  output$radiodbFormatExpOutput <- renderUI({
    shinyWidgets::prettyRadioButtons(inputId = "dbFormatExpInput",
                                     label = i18n$t("Data base format"),
                                     choices = stats::setNames(
                                       c(TRUE, FALSE),
                                       c(i18n$t("Yes"), i18n$t("No")) # Set labels
                                     ),
                                     selected = FALSE,
                                     inline = TRUE,
                                     status = "primary"
    )
  })
  
  
  output$pickerTypeExportOutput <- renderUI({
    shinyWidgets::pickerInput(inputId = "typeExportInput",
                              label = i18n$t("Type of result"),
                              choices = stats::setNames(
                                c("simComposition",
                                  "simResults", 
                                  "simMultifunctionality",
                                  "simUnavailableSpecies",
                                  "refComposition",
                                  "refResults",
                                  "refMultifunctionality", 
                                  "supComposition", 
                                  "supResults",
                                  "supMultifunctionality"),
                                c(i18n$t("simComposition"),
                                  i18n$t("simResults"), 
                                  i18n$t("simMultifunctionality"),
                                  i18n$t("simUnavailableSpecies"),
                                  i18n$t("refComposition"),
                                  i18n$t("refResults"),
                                  i18n$t("refMultifunctionality"), 
                                  i18n$t("supComposition"), 
                                  i18n$t("supResults"),
                                  i18n$t("supMultifunctionality")) # Set labels
                              ),
                              multiple = TRUE,
                              options = list("max-options" = 1),
                              inline = FALSE
    )
  })
  
  ## Check buttons - OK ----
  ### Check doAdjustSim button ----
  shiny::observeEvent(input$scenarioSimAdjInput, ignoreNULL = FALSE, {
    if(is.null(input$scenarioSimAdjInput)){
      shiny::updateActionButton(session, "doAdjustSim", disabled = TRUE)
    } else(
      shiny::updateActionButton(session, "doAdjustSim", disabled = FALSE)
    )
  })
  ### Check doCompute button ----
  shiny::observeEvent(input$scenarioComParInput, ignoreNULL = FALSE, {
    if(is.null(input$scenarioComParInput)){
      shiny::updateActionButton(session, "doCompute", disabled = TRUE)
    } else(
      shiny::updateActionButton(session, "doCompute", disabled = FALSE)
    )
  })
  ### Check doStandardize button ----
  shiny::observeEvent(input$scenarioComStandParInput, ignoreNULL = FALSE, {
    if(is.null(input$scenarioComStandParInput)){
      shiny::updateActionButton(session, "doStandardize", disabled = TRUE)
    } else(
      shiny::updateActionButton(session, "doStandardize", disabled = FALSE)
    )
  })
  ### Check doMultiCompute button ----
  shiny::observeEvent(input$scenarioComMultiInput, ignoreNULL = FALSE, {
    if(is.null(input$scenarioComMultiInput)){
      shiny::updateActionButton(session, "doMultiCompute", disabled = TRUE)
    } else(
      shiny::updateActionButton(session, "doMultiCompute", disabled = FALSE)
    )
  })
  ### Check doSelect button ----
  obsListDoSelect <- shiny::reactive({
    list(input$prefixSelInput, input$scenarioSelInput)
  })
  shiny::observeEvent(obsListDoSelect(), ignoreNULL = FALSE, {
    if(is.null(input$scenarioSelInput) || input$prefixSelInput == ""){
      shiny::updateActionButton(session, "doSelect", disabled = TRUE)
    } else(
      shiny::updateActionButton(session, "doSelect", disabled = FALSE)
    )
  })
  ### Check doPlotPar button ----
  shiny::observeEvent(input$scenarioViewParInput, ignoreNULL = FALSE, {
    if(is.null(input$scenarioViewParInput)){
      shiny::updateActionButton(session, "doPlotPar", disabled = TRUE)
    } else(
      shiny::updateActionButton(session, "doPlotPar", disabled = FALSE)
    )
  })
  ### Check doPlotMulti button ----
  shiny::observeEvent(input$scenarioViewMultiInput, ignoreNULL = FALSE, {
    if(is.null(input$scenarioViewMultiInput)){
      shiny::updateActionButton(session, "doPlotMulti", disabled = TRUE)
    } else(
      shiny::updateActionButton(session, "doPlotMulti", disabled = FALSE)
    )
  })
  ### Check doExport button ----
  shiny::observeEvent(input$scenarioExportInput, ignoreNULL = FALSE, {
    if(is.null(input$scenarioExportInput)){
      shiny::updateActionButton(session, "doExport", disabled = TRUE)
    } else(
      shiny::updateActionButton(session, "doExport", disabled = FALSE)
    )
  })
  ### Check doSelectMerge button ----
  obsListDoSelectMerge <- shiny::reactive({
    list(input$mergeSelectNameInput, input$mergeSelectInput)
  })
  shiny::observeEvent(obsListDoSelectMerge(), ignoreNULL = FALSE, {
    if(is.null(input$mergeSelectInput) || input$mergeSelectNameInput == ""){
      shiny::updateActionButton(session, "doSelectMerge", disabled = TRUE)
    } else(
      shiny::updateActionButton(session, "doSelectMerge", disabled = FALSE)
    )
  })
  ### Check doSimulateMerge button ----
  obsListDoSimulateMerge <- shiny::reactive({
    list(input$mergeSimulateNameInput, input$mergeSimulateInput)
  })
  shiny::observeEvent(obsListDoSimulateMerge(), ignoreNULL = FALSE, {
    if(is.null(input$mergeSimulateInput) || input$mergeSimulateNameInput == ""){
      shiny::updateActionButton(session, "doSimulateMerge", disabled = TRUE)
    } else(
      shiny::updateActionButton(session, "doSimulateMerge", disabled = FALSE)
    )
  })
  ### Check doSelectRemove button ----
  shiny::observeEvent(input$removeSelectInput, ignoreNULL = FALSE, {
    if(is.null(input$removeSelectInput)){
      shiny::updateActionButton(session, "doSelectRemove", disabled = TRUE)
    } else(
      shiny::updateActionButton(session, "doSelectRemove", disabled = FALSE)
    )
  })
  ### Check doSimulateRemove button ----
  shiny::observeEvent(input$removeSimulateInput, ignoreNULL = FALSE, {
    if(is.null(input$removeSimulateInput)){
      shiny::updateActionButton(session, "doSimulateRemove", disabled = TRUE)
    } else(
      shiny::updateActionButton(session, "doSimulateRemove", disabled = FALSE)
    )
  })
  ## Input aux ----
  ### Simulate tab - prefixSimInput ----
  shiny::observeEvent(input$prefixSimInput, {
    if(!input$prefixSimInput == ""){
      shiny::updateActionButton(session, "doSimulate", disabled = FALSE)
    } else{
      shiny::updateActionButton(session, "doSimulate", disabled = TRUE)
    }
  })
  ### Simulate tab - itSimInput ----
  # Check - If itSimInput is NA or < 4 disable the simulate button
  shiny::observeEvent(input$itSimInput, {
    if(!is.na(input$itSimInput)){
      inputParSimRV$it <- input$itSimInput
      if(input$itSimInput>=4){
        shiny::updateActionButton(session, "doSimulate", disabled = FALSE)	
      } else{
        shiny::updateActionButton(session, "doSimulate", disabled = TRUE)
      }
    } else{
      shiny::updateActionButton(session, "doSimulate", disabled = TRUE)
      inputParSimRV$it <- NULL
    }
  })
  ### Simulate tab - restComp and restGroup----
  obsListRest <- shiny::reactive({
    list(input$goalsSimInput, inputDataRV$restComp, inputDataRV$restGroup)
  })
  shiny::observeEvent(obsListRest(), {
    if(!is.null(input$goalsSimInput)){
      if(input$goalsSimInput == "New"){
        inputParSimRV$restComp <- NULL
        inputParSimRV$restGroup <- NULL
      } else{ # Ongoing
        inputParSimRV$restComp <- inputDataRV$restComp
        inputParSimRV$restGroup <- inputDataRV$restGroup
      }
    }
  })
  ### Simulate tab - nIndSimInput and cvAbundSimInput ----
  obsListSimMethod <- shiny::reactive({
    list(input$methodSimInput, input$nIndSimInput, input$cvAbundSimInput)
  })
  shiny::observeEvent(obsListSimMethod(), {
    if(!is.null(input$methodSimInput)){
      if(tolower(input$methodSimInput) == "individuals"){
        if(is.na(input$nIndSimInput) || is.na(input$cvAbundSimInput)){
          shiny::updateActionButton(session, "doSimulate", disabled = TRUE)
        } else{
          shiny::updateActionButton(session, "doSimulate", disabled = FALSE)
        }
        inputParSimRV$nInd <- input$nIndSimInput
        inputParSimRV$cvAbund <- input$cvAbundSimInput
      } else{
        shiny::updateActionButton(session, "doSimulate", disabled = FALSE)
        inputParSimRV$nInd <- NULL
        inputParSimRV$cvAbund <- NULL
      }
    }
  })
  ### View tab - dbFormatExpInput ----
  shiny::observeEvent(input$dbFormatExpInput, ignoreNULL = FALSE, {
    if(!is.null(input$dbFormatExpInput)){
      exportRV$dbFormat <- input$dbFormatExpInput
    } else{
      exportRV$dbFormat <- FALSE
    }
  })
  ## Action buttons ----
  ### doCheck ----
  shiny::observeEvent(input$doCheck, {
    # Remove any open modal
    shiny::removeModal(session = session)
    scenario <- tryCatch(checkResbiotaData(traits = inputDataRV$traits,
                                           restComp = inputDataRV$restComp, 
                                           restGroup = inputDataRV$restGroup,
                                           reference = inputDataRV$reference,
                                           supplementary = inputDataRV$supplementary
                                           
    ), error = function(e) e)
    if(inherits(scenario, what = "error")){
      shinyWidgets::sendSweetAlert(
        session = session,
        title = i18n$t("Something went wrong!"),
        type = "error"
      )
    } else{
      if(scenario$checkStatus == "success"){
        shinyWidgets::sendSweetAlert(
          session = session,
          title = i18n$t("Success!"),
          type = scenario$checkStatus
        )
      } else{
        if(scenario$checkStatus == "error"){
          shinyWidgets::sendSweetAlert(
            session = session,
            title = i18n$t("Error!"),
            text = scenario$checkMessage,
            type = scenario$checkStatus
          )
        } else{
          shinyWidgets::sendSweetAlert(
            session = session,
            title = i18n$t("Warning!"),
            text = scenario$checkNA,
            type = scenario$checkStatus
          )
        }
      }
    }
  })
  ### doSimulate ----
  shiny::observeEvent(input$doSimulate, {
    # Remove any open modal
    shiny::removeModal(session = session)
    # Check if species trait data exist
    if(is.null(inputDataRV$traits)){
      shinyWidgets::sendSweetAlert(
        session = session,
        title = i18n$t("Error!"),
        text = i18n$t("Load the species traits data"),
        type = "error"
      )
    } else {
      shiny::showModal(shiny::modalDialog(title = i18n$t("Running"), footer = NULL), session = session)
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
      scenario <- tryCatch(simulateCommunities(trait = inputDataRV$traits,
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
      ), error = function(e) e)
      if(inherits(scenario, what = "error")){
        shinyWidgets::sendSweetAlert(
          session = session,
          title = i18n$t("Something went wrong!"),
          type = "error"
        )
      } else{
        resultsRV$simulate[[input$prefixSimInput]] <- scenario
        # Update basic informations
        resultsRV$nSim <- sum(sapply(resultsRV$simulate, function(x) nrow(x$simulation$composition)))
        resultsRV$nSce <- length(resultsRV$simulate)
        shiny::removeModal(session = session)
        shinyWidgets::sendSweetAlert(
          session = session,
          title = i18n$t("Done!"),
          text = paste0(i18n$t("Simulations scenarios: "), resultsRV$nSce),
          type = "success"
        )
      }
    }
  })
  ### doSimulateMerge ----
  # shiny::observeEvent(input$doSimulateMerge, {
  # 	# If the picker input is valid
  # 	if(length(input$mergeSimulateInput)>0){
  # 		tempRV <- vector("list", length = length(input$mergeSimulateInput))
  # 		for(i in 1:length(input$mergeSimulateInput)){
  # 			# Copy to temp list
  # 			tempRV[[i]] <- resultsRV$simulate[[input$mergeSimulateInput[i]]]
  # 			# Then remove
  # 			resultsRV$simulate[[input$mergeSimulateInput[i]]] <- NULL
  # 		}
  # 		# Merge
  # 		resultsRV$simulate[[input$mergeSimulateNameInput]] <- do.call(mergeSimulations, tempRV)
  # 		resultsRV$simulate[[input$mergeSimulateNameInput]]$call <- "Call" # Remove long call
  # 	}
  # 	# Update basic informations
  # 	resultsRV$nSim <- sum(sapply(resultsRV$simulate, function(x) nrow(x$simulation$composition)))
  # 	resultsRV$nSce <- length(resultsRV$simulate)
  # 	shinyWidgets::sendSweetAlert(
  # 		session = session,
  # 		title = i18n$t("Done!"),
  # 		text = paste0(i18n$t("Simulations scenarios: "), resultsRV$nSce),
  # 		type = "success"
  # 	)
  # })
  shiny::observeEvent(input$doSimulateMerge, {
    # If the picker input is valid
    if(length(input$mergeSimulateInput)>0){
      tempRV <- vector("list", length = length(input$mergeSimulateInput))
      for(i in 1:length(input$mergeSimulateInput)){
        # Copy to temp list
        tempRV[[i]] <- resultsRV$simulate[[input$mergeSimulateInput[i]]]
      }
      scenario <- tryCatch(do.call(mergeSimulations, tempRV), 
                           error = function(e) e)
      if(inherits(scenario, what = "error")){
        shinyWidgets::sendSweetAlert(
          session = session,
          title = i18n$t("Something went wrong!"),
          type = "error"
        )
      } else{
        for(i in 1:length(input$mergeSimulateInput)){
          # Then remove
          resultsRV$simulate[[input$mergeSimulateInput[i]]] <- NULL
        }
        # Merge
        resultsRV$simulate[[input$mergeSimulateNameInput]] <- scenario
        resultsRV$simulate[[input$mergeSimulateNameInput]]$call <- "Call" # Remove long call
        # Update basic informations
        resultsRV$nSim <- sum(sapply(resultsRV$simulate, function(x) nrow(x$simulation$composition)))
        resultsRV$nSce <- length(resultsRV$simulate)
        shinyWidgets::sendSweetAlert(
          session = session,
          title = i18n$t("Done!"),
          text = paste0(i18n$t("Simulations scenarios: "), resultsRV$nSce),
          type = "success"
        )
      }
    }
  })
  ### doSimulateRemove ----
  shiny::observeEvent(input$doSimulateRemove, {
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
    shinyWidgets::sendSweetAlert(
      session = session,
      title = i18n$t("Done!"),
      text = paste0(i18n$t("Simulations scenarios: "), resultsRV$nSce),
      type = "success"
    )
  })
  ### doAdjustSim ----
  shiny::observeEvent(input$doAdjustSim, {
    scenario <- resultsRV$simulate[[input$scenarioSimAdjInput]]
    scenario <- tryCatch(adjustSimulations(x = scenario,
                                           minAbun = input$minAbuSliderSimAdjInput, # straight input
                                           reallocate = as.logical(input$reallocateAdjSimInput) # straight input
    ), error = function(e) e)
    if(inherits(scenario, what = "error")){
      shinyWidgets::sendSweetAlert(
        session = session,
        title = i18n$t("Something went wrong!"),
        type = "error"
      )
    } else{
      print(scenario$simulation$composition)
      resultsRV$simulate[[input$scenarioSimAdjInput]] <- scenario
      shinyWidgets::sendSweetAlert(
        session = session,
        title = i18n$t("Done!"),
        type = "success"
      )
    }
  })
  ### doCompute ----
  shiny::observeEvent(input$doCompute, {
    # Remove any open modal
    shiny::removeModal(session = session)
    checkCost <- c(is.null(input$costComInput), is.null(input$densComInput))
    if(!c(all(checkCost == TRUE) || all(checkCost == FALSE))){
      shinyWidgets::sendSweetAlert(
        session = session,
        title = i18n$t("Error!"),
        text = i18n$t("Specify cost and density. Or none of them"),
        type = "error"
      )
    } else {
      shiny::showModal(shiny::modalDialog(title = i18n$t("Running"), footer = NULL), session = session)
      scenario <- tryCatch(computeParameters(x = resultsRV$simulate[[input$scenarioComParInput]],
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
      ), error = function(e) e)
      if(inherits(scenario, what = "error")){
        shinyWidgets::sendSweetAlert(
          session = session,
          title = i18n$t("Something went wrong!"),
          type = "error"
        )
      } else{
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
        shiny::removeModal(session = session)
        # Force update parameters
        resultsRV$updatePar <- ifelse(resultsRV$updatePar == 1, 0, 1)
        shinyWidgets::sendSweetAlert(
          session = session,
          title = i18n$t("Done!"),
          type = "success"
        )
      }
    }
  })
  ### doMultiCompute ----
  shiny::observeEvent(input$doMultiCompute, {
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
    scenario <- tryCatch(computeMultifunctionality(x = resultsRV$simulate[[input$scenarioComMultiInput]],
                                                   tests = testList), error = function(e) e)
    if(inherits(scenario, what = "error")){
      shinyWidgets::sendSweetAlert(
        session = session,
        title = i18n$t("Something went wrong!"),
        type = "error"
      )
    } else{
      # No need to round the numbers
      resultsRV$simulate[[input$scenarioComMultiInput]] <- scenario
      # Force update parameters
      resultsRV$updatePar <- ifelse(resultsRV$updatePar == 1, 0, 1)
      shinyWidgets::sendSweetAlert(
        session = session,
        title = i18n$t("Done!"),
        type = "success"
      )
    }
  })
  ### doStandardize ----
  shiny::observeEvent(input$doStandardize, {
    scenario <- resultsRV$simulate[[input$scenarioComStandParInput]]
    # if (inherits(scenario, "simRest")) {
    res <- scenario$simulation$results
    # } else {
    # 	res <- scenario$selection$results
    # }
    if(is.null(res)){
      shinyWidgets::sendSweetAlert(
        session = session,
        title = i18n$t("Error!"),
        text = i18n$t("Compute functional parameters in this scenario"),
        type = "error"
      )
    } else{
      scenario <- tryCatch(standardizeParameters(x = scenario,
                                                 parameters = input$stanComParInput, # straight input
                                                 method = input$speficyMethodStanInput # straight input
      ), error = function(e) e)
      if(inherits(scenario, what = "error")){
        shinyWidgets::sendSweetAlert(
          session = session,
          title = i18n$t("Something went wrong!"),
          type = "error"
        )
      } else{
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
        shinyWidgets::sendSweetAlert(
          session = session,
          title = i18n$t("Done!"),
          type = "success"
        )
      }
    }
  })
  
  ### doSelect ----
  shiny::observeEvent(input$doSelect, {
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
    scenario <- tryCatch(selectCommunities(x = resultsRV$simulate[[input$scenarioSelInput]],
                                           testsDet = inputParSelRV$testsDet,
                                           testsHie = inputParSelRV$testsHie,
                                           group = inputParSelRV$group,
                                           singleselection = as.logical(input$singleSelectionInput) # straight input
    ), error = function(e) e)
    if(inherits(scenario, what = "error")){
      shinyWidgets::sendSweetAlert(
        session = session,
        title = i18n$t("Something went wrong!"),
        type = "error"
      )
    } else{
      resultsRV$select[[input$prefixSelInput]] <- scenario
      # Update basic informations
      resultsRV$nSimSel <- sum(sapply(resultsRV$select, function(x) nrow(x$selection$composition)))
      resultsRV$nSel <- length(resultsRV$select)
      shinyWidgets::sendSweetAlert(
        session = session,
        title = i18n$t("Done!"),
        type = "success"
      )
    }
  })
  ### doSelectMerge ----
  # shiny::observeEvent(input$doSelectMerge, {
  # 	# If the picker input is valid
  # 	if(length(input$mergeSelectInput)>0){
  # 		tempRV <- vector("list", length = length(input$mergeSelectInput))
  # 		for(i in 1:length(input$mergeSelectInput)){
  # 			# Copy to temp list
  # 			tempRV[[i]] <- resultsRV$select[[input$mergeSelectInput[i]]]
  # 			# Then remove
  # 			resultsRV$select[[input$mergeSelectInput[i]]] <- NULL
  # 		}
  # 		# Merge
  # 		resultsRV$select[[input$mergeSelectNameInput]] <- do.call(mergeSelection, tempRV)
  # 		resultsRV$select[[input$mergeSelectNameInput]]$call <- "Call" # Remove long call
  # 	}
  # 	# Update basic informations
  # 	resultsRV$nSimSel <- sum(sapply(resultsRV$select, function(x) nrow(x$selection$composition)))
  # 	resultsRV$nSel <- length(resultsRV$select)
  # 	shinyWidgets::sendSweetAlert(
  # 		session = session,
  # 		title = i18n$t("Done!"),
  # 		type = "success"
  # 	)
  # })
  shiny::observeEvent(input$doSelectMerge, {
    # If the picker input is valid
    if(length(input$mergeSelectInput)>0){
      tempRV <- vector("list", length = length(input$mergeSelectInput))
      for(i in 1:length(input$mergeSelectInput)){
        # Copy to temp list
        tempRV[[i]] <- resultsRV$select[[input$mergeSelectInput[i]]]
        # Then remove
        # resultsRV$select[[input$mergeSelectInput[i]]] <- NULL
      }
      scenario <- tryCatch(do.call(mergeSelection, tempRV), 
                           error = function(e) e)
      if(inherits(scenario, what = "error")){
        shinyWidgets::sendSweetAlert(
          session = session,
          title = i18n$t("Something went wrong!"),
          type = "error"
        )
      } else{
        for(i in 1:length(input$mergeSelectInput)){
          # Then remove
          resultsRV$select[[input$mergeSelectInput[i]]] <- NULL
        }
        # Merge
        resultsRV$select[[input$mergeSelectNameInput]] <- scenario
        resultsRV$select[[input$mergeSelectNameInput]]$call <- "Call" # Remove long call
        # Update basic informations
        resultsRV$nSimSel <- sum(sapply(resultsRV$select, function(x) nrow(x$selection$composition)))
        resultsRV$nSel <- length(resultsRV$select)
        shinyWidgets::sendSweetAlert(
          session = session,
          title = i18n$t("Done!"),
          type = "success"
        )
      }
    }
  })
  ### doSelectRemove ----
  shiny::observeEvent(input$doSelectRemove, {
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
    shinyWidgets::sendSweetAlert(
      session = session,
      title = i18n$t("Done!"),
      type = "success"
    )
  })
  ### doPlotPar ----
  shiny::observeEvent(input$doPlotPar, {
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
      shinyWidgets::sendSweetAlert(
        session = session,
        title = i18n$t("Error!"),
        text = i18n$t("Select variables for both axes"),
        type = "error"
      )
    }
  })
  ### doPlotParClear ----
  shiny::observeEvent(input$doPlotParClear, {
    resultsRV$plotPar <- NULL
  })
  ### doPlotMulti ----
  shiny::observeEvent(input$doPlotMulti, {
    if(input$scenarioTypeViewMultiInput == "Raw"){
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
      # Update using dynamic text inputs
      # Force labels change
      inVars <- colnames(resMulti)[-1]
      pvars <- length(inVars)
      if (pvars > 0) {
        lapply(seq(pvars), function(i) {
          colnames(resMulti)[which(colnames(resMulti) == inVars[i])] <<- input[[paste0("labMulti", inVars[i])]]
          if(!is.null(scenario$reference$multifunctionality)){
            colnames(scenario$reference$multifunctionality)[which(colnames(scenario$reference$multifunctionality) == inVars[i])] <<- input[[paste0("labMulti", inVars[i])]]
          }
        })
      }
      # Update multifunctionality matrix
      if (inherits(scenario, "simRest")) {
        scenario$simulation$multifunctionality <- resMulti
      }
      else {
        scenario$selection$multifunctionality  <- resMulti
      }
      resultsRV$plotMulti <- viewMultifunctionality(x = scenario,
                                                    hideref = as.logical(input$hideRefViewMultiInput))
    } else {
      resultsRV$plotMulti <- NULL
      shinyWidgets::sendSweetAlert(
        session = session,
        title = i18n$t("Error!"),
        text = i18n$t("Scenario must include multifunctionality results"),
        type = "error"
      )
    }
  })
  ### doPlotMultiClear ----
  shiny::observeEvent(input$doPlotMultiClear, {
    resultsRV$plotMulti <- NULL
  })
  ### doExport ----
  shiny::observeEvent(input$doExport, {
    if(input$scenarioTypeExportInput == "Raw"){
      scenario <- resultsRV$simulate[[input$scenarioExportInput]]
    } else{
      scenario <- resultsRV$select[[input$scenarioExportInput]]
    }
    if(input$typeExportInput == "simUnavailableSpecies" & is.null(input$avaExpInput)){
      exportRV$table <- NULL
      exportRV$summaryTable <- NULL
      shinyWidgets::sendSweetAlert(
        session = session,
        title = i18n$t("Error!"),
        text = i18n$t("Set species availability"),
        type = "error"
      )
    } else{
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
            # Replace the six col (Centred ellipsis)
            exportRV$summaryTable[, 6] <- rep("\u22ef", nRowTemp)
            colnames(exportRV$summaryTable)[6] <- "\u22ef"
          }
          if(nRowTemp>10){
            exportRV$summaryTable <- exportRV$summaryTable[c(1:6, (nRowTemp-4):nRowTemp), , drop = FALSE]
            # Replace the six line (Vertical ellipsis)
            exportRV$summaryTable[6, ] <- rep("\u22ee", ncol(exportRV$summaryTable))
          }
        }
      } else{
        exportRV$summaryTable <- NULL
      }
    }
  })
  ### doDownloadParPlot ----
  output$doDownloadParPlot <- shiny::downloadHandler(
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
  output$doDownloadMultiPlot <- shiny::downloadHandler(
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
  output$doDownloadExport <- shiny::downloadHandler(
    filename <- function() {paste0(input$projectName, "_", input$scenarioExportInput, "_", input$typeExportInput, "_", globalRV$currentDate, ".csv")},
    content <- function(file) {
      utils::write.csv(exportRV$table, file = file)
    }
  )
  ## Help buttons ----
  # Update choices 
  obsListInfoButtons <- shiny::reactive({
    list(
      # DataInputTab
      input$traitsInputInfo,
      input$restCompInputInfo,
      input$restGroupInputInfo,
      input$referenceInputInfo,
      input$supplementaryInputInfo,
      # simulateTab
      input$goalsSimInputInfo,
      input$methodSimInputInfo,
      input$nIndSimInputInfo,
      input$richSliderSimInputInfo,
      input$itSimInputInfo,
      input$avaSimInputInfo,
      input$undSimInputInfo,
      input$cwmSimInputInfo,
      input$raoSimInputInfo,
      input$speficyGroupsSimInputInfo,
      input$probGroupTypeSimInputInfo,
      input$probSimInputInfo,
      input$cvAbundSimInputInfo,
      input$phiSimInputInfo,
      input$minAbuSliderSimAdjInputInfo,
      input$reallocateAdjSimInputInfo,
      # computeTab
      input$avaComInputInfo,
      input$cwmComInputInfo,
      input$cwvComInputInfo,
      input$raoComInputInfo,
      input$disComInputInfo,
      input$costComInputInfo,
      input$densComInputInfo,
      input$stanComParInputInfo,
      input$speficyMethodStanInputInfo,
      input$testsMultiInputInfo,
      # SelectTab
      input$testsDetSelInputInfo,
      input$testsHieSelInputInfo,
      input$speficyGroupsSelInputInfo,
      input$singleSelectionInputInfo
    )
  })
  shiny::observeEvent(obsListInfoButtons(), ignoreInit = TRUE, {
    # if(sum(unlist(obsListInfoButtons()))>0){
    # Default values
    infoText <- NULL
    # DataInputTab
    if(!is.null(input$traitsInputInfo)){
      if(input$traitsInputInfo>infoRV$traitsInputInfo){
        infoText <- i18n$t("Data table with species traits in a way that arranged species names in the rows and traits in the columns. This table should include any species information that can be used to calculate functional metrics, costs or any information that can be used as parameters to simulate and select simulated communities.")
        infoRV$traitsInputInfo <- input$traitsInputInfo
      }
    }
    if(!is.null(input$restCompInputInfo)){
      if(input$restCompInputInfo>infoRV$restCompInputInfo){
        infoText <- i18n$t("Data table with species composition in the restoration sites in a way that arranged sites in the rows and species names in the columns. Missing data (NA) are not accepted.")
        infoRV$restCompInputInfo <- input$restCompInputInfo
      }
    }
    if(!is.null(input$restGroupInputInfo)){
      if(input$restGroupInputInfo>infoRV$restGroupInputInfo){
        infoText <- i18n$t("Data table with complementary information for restoration sites in a way that arranged sites in the rows and variables in the columns. This information will be included in the metrics calculated for each community and can be used as complementary information to select the simulations.")
        infoRV$restGroupInputInfo <- input$restGroupInputInfo
      }
    }
    if(!is.null(input$referenceInputInfo)){
      if(input$referenceInputInfo>infoRV$referenceInputInfo){
        infoText <- i18n$t("Data table with species composition in the reference sites in a way that arranged sites in the rows and species names in the columns. Missing data (NA) are not accepted.")
        infoRV$referenceInputInfo <- input$referenceInputInfo
      }
    }
    if(!is.null(input$supplementaryInputInfo)){
      if(input$supplementaryInputInfo>infoRV$supplementaryInputInfo){
        infoText <- i18n$t("Data table with species composition in the supplementary sites in a way that arranged sites in the rows and species names in the columns. Missing data (NA) are not accepted.")
        infoRV$supplementaryInputInfo <- input$supplementaryInputInfo
      }
    }
    # simulateTab
    if(!is.null(input$goalsSimInputInfo)){
      if(input$goalsSimInputInfo>infoRV$goalsSimInputInfo){
        infoText <- i18n$t("Restoration goals. When goals are equal to 'New', no restoration sites are considered, and the simulated communities are set as empty communities (sites to restore start with no species, and all species must be planted for restoration). Alternatively, when goals are equal to 'Ongoing', the species composition in the restoration sites must be informed (in the data input tab). Thus, the new species and individuals are introduced into the established communities (sites to restore can start with pre-existing species).")
        infoRV$goalsSimInputInfo <- input$goalsSimInputInfo
      }
    }
    if(!is.null(input$methodSimInputInfo)){
      if(input$methodSimInputInfo>infoRV$methodSimInputInfo){
        infoText <- i18n$t("Method to obtain the samples. The 'proportions' method simulates the species composition sampled taken from a log-normal distribution. Species composition is given in proportions of the total number of individuals. The 'individuals' method performs the simulation sampling individuals taken from a distribution. The probabilities to draw individuals can be supplied by the user or taken from a log-normal distribution. However, the number of individuals to sample in each community must be supplied. In this method, the species composition is raw abundance, given by counting individuals.")
        infoRV$methodSimInputInfo <- input$methodSimInputInfo
      }
    }
    if(!is.null(input$nIndSimInputInfo)){
      if(input$nIndSimInputInfo>infoRV$nIndSimInputInfo){
        infoText <- i18n$t("The number of individuals to be sampled in each community.")
        infoRV$nIndSimInputInfo <- input$nIndSimInputInfo
      }
    }
    if(!is.null(input$richSliderSimInputInfo)){
      if(input$richSliderSimInputInfo>infoRV$richSliderSimInputInfo){
        infoText <- i18n$t("The range of richness values in each community.")
        infoRV$richSliderSimInputInfo <- input$richSliderSimInputInfo
      }
    }
    if(!is.null(input$itSimInputInfo)){
      if(input$itSimInputInfo>infoRV$itSimInputInfo){
        infoText <- i18n$t("The number of communities to simulate.")
        infoRV$itSimInputInfo <- input$itSimInputInfo
      }
    }
    if(!is.null(input$avaSimInputInfo)){
      if(input$avaSimInputInfo>infoRV$avaSimInputInfo){
        infoText <- i18n$t("Variable to indicate species availability in the market (1 or 0).")
        infoRV$avaSimInputInfo <- input$avaSimInputInfo
      }
    }
    if(!is.null(input$undSimInputInfo)){
      if(input$undSimInputInfo>infoRV$undSimInputInfo){
        infoText <- i18n$t("Variable to indicate undesired species (1 or 0). Undesired species may already be present in restoration or reference communities but are not appropriate in new restorations.")
        infoRV$undSimInputInfo <- input$undSimInputInfo
      }
    }
    if(!is.null(input$cwmSimInputInfo)){
      if(input$cwmSimInputInfo>infoRV$cwmSimInputInfo){
        infoText <- i18n$t("Traits names to indicate which traits are used to constrain Community Weighted Mean (CWM) while maximising functional diversity. Constraints are driven over the range of each trait and allow create a wide range of means of those traits across the simulated communities.")
        infoRV$cwmSimInputInfo <- input$cwmSimInputInfo
      }
    }
    if(!is.null(input$raoSimInputInfo)){
      if(input$raoSimInputInfo>infoRV$raoSimInputInfo){
        infoText <- i18n$t("Traits names to indicate which traits are used to maximize functional diversity (Rao Quadratic Entropy).")
        infoRV$raoSimInputInfo <- input$raoSimInputInfo
      }
    }
    if(!is.null(input$speficyGroupsSimInputInfo)){
      if(input$speficyGroupsSimInputInfo>infoRV$speficyGroupsSimInputInfo){
        infoText <- i18n$t("The sampling process can be constrained inside species groups. That procedure can be conducted to draw abundances with or without constraints in species richness. First, set the variable which indicates the group to which the species belongs and the constraint method. Then, adjust the sliders to set the probabilities in each species group.")
        infoRV$speficyGroupsSimInputInfo <- input$speficyGroupsSimInputInfo
      }
    }
    if(!is.null(input$probGroupTypeSimInputInfo)){
      if(input$probGroupTypeSimInputInfo>infoRV$probGroupTypeSimInputInfo){
        infoText <- i18n$t("Sampling constraint method. The method 'abundance' constrains only the simulation of species abundance. The method 'Richness and abundance' constrains species abundance and species richness in the communities simulation.")
        infoRV$probGroupTypeSimInputInfo <- input$probGroupTypeSimInputInfo
      }
    }
    if(!is.null(input$probSimInputInfo)){
      if(input$probSimInputInfo>infoRV$probSimInputInfo){
        infoText <- i18n$t("Variable to indicate the probabilities of individuals being drawn.")
        infoRV$probSimInputInfo <- input$probSimInputInfo
      }
    }
    if(!is.null(input$cvAbundSimInputInfo)){
      if(input$cvAbundSimInputInfo>infoRV$cvAbundSimInputInfo){
        infoText <- i18n$t("Coefficient of variation (CV) of the relative abundances in the species pool.")
        infoRV$cvAbundSimInputInfo <- input$cvAbundSimInputInfo
      }
    }
    if(!is.null(input$phiSimInputInfo)){
      if(input$phiSimInputInfo>infoRV$phiSimInputInfo){
        infoText <- i18n$t("A parameter bounded between 0 and 1 that weights the importance of either quadratic entropy or entropy.")
        infoRV$phiSimInputInfo <- input$phiSimInputInfo
      }
    }
    if(!is.null(input$minAbuSliderSimAdjInputInfo)){
      if(input$minAbuSliderSimAdjInputInfo>infoRV$minAbuSliderSimAdjInputInfo){
        infoText <- i18n$t("Minimal abundance or proportion to keep in simulated communities.")
        infoRV$minAbuSliderSimAdjInputInfo <- input$minAbuSliderSimAdjInputInfo
      }
    }
    
    if(!is.null(input$reallocateAdjSimInputInfo)){
      if(input$reallocateAdjSimInputInfo>infoRV$reallocateAdjSimInputInfo){
        infoText <- i18n$t("Reallocate removed individuals to species with some abundance.")
        infoRV$reallocateAdjSimInputInfo <- input$reallocateAdjSimInputInfo
      }
    }
    # computeTab
    if(!is.null(input$avaComInputInfo)){
      if(input$avaComInputInfo>infoRV$avaComInputInfo){
        infoText <- i18n$t("Variable to indicate species availability in the market (1 or 0).")
        infoRV$avaComInputInfo <- input$avaComInputInfo
      }
    }
    if(!is.null(input$cwmComInputInfo)){
      if(input$cwmComInputInfo>infoRV$cwmComInputInfo){
        infoText <- i18n$t("Traits names to calculate Community Weighted Mean (CWM). One CWM is calculated for each trait.")
        infoRV$cwmComInputInfo <- input$cwmComInputInfo
      }
    }
    if(!is.null(input$cwvComInputInfo)){
      if(input$cwvComInputInfo>infoRV$cwvComInputInfo){
        infoText <- i18n$t("Traits names to calculate Community Weighted Variance (CWV). One CWV is calculated for each trait.")
        infoRV$cwvComInputInfo <- input$cwvComInputInfo
      }
    }
    if(!is.null(input$raoComInputInfo)){
      if(input$raoComInputInfo>infoRV$raoComInputInfo){
        infoText <- i18n$t("Traits names to calculate Rao Quadratic Entropy (rao). Only a single rao measure is calculated with the set input species traits.")
        infoRV$raoComInputInfo <- input$raoComInputInfo
      }
    }
    if(!is.null(input$disComInputInfo)){
      if(input$disComInputInfo>infoRV$disComInputInfo){
        infoText <- i18n$t("Traits names to calculate functional dissimilarity with the reference sites.")
        infoRV$disComInputInfo <- input$disComInputInfo
      }
    }
    if(!is.null(input$costComInputInfo)){
      if(input$costComInputInfo>infoRV$costComInputInfo){
        infoText <- i18n$t("Variable to indicate the species cost per individual.")
        infoRV$costComInputInfo <- input$costComInputInfo
      }
    }
    if(!is.null(input$densComInputInfo)){
      if(input$densComInputInfo>infoRV$densComInputInfo){
        infoText <- i18n$t("Variable to indicate the species planting density.")
        infoRV$densComInputInfo <- input$densComInputInfo
      }
    }
    if(!is.null(input$stanComParInputInfo)){
      if(input$stanComParInputInfo>infoRV$stanComParInputInfo){
        infoText <- i18n$t("Parameters name to standardization.")
        infoRV$stanComParInputInfo <- input$stanComParInputInfo
      }
    }
    if(!is.null(input$speficyMethodStanInputInfo)){
      if(input$speficyMethodStanInputInfo>infoRV$speficyMethodStanInputInfo){
        infoText <- i18n$t("Standardization method. The method 'maximum' divides the values of each variable by its maximum. The method 'standardise' scales the calculated parameters to zero mean and unit variance in each variable.")
        infoRV$speficyMethodStanInputInfo <- input$speficyMethodStanInputInfo
      }
    }
    if(!is.null(input$testsMultiInputInfo)){
      if(input$testsMultiInputInfo>infoRV$testsMultiInputInfo){
        infoText <- i18n$t("Multifunctionality criteria. The multifunctionality is based on simple logical tests derived from thresholds to multiples calculated parameters.  The sum of individual tests true to a given threshold is the alpha multifunctionality metric. First, select the parameters which will be used to test, then set the sliders as logical tests.")
        infoRV$testsMultiInputInfo <- input$testsMultiInputInfo
      }
    }
    # SelectTab
    if(!is.null(input$testsDetSelInputInfo)){
      if(input$testsDetSelInputInfo>infoRV$testsDetSelInputInfo){
        infoText <- i18n$t("Deterministic selection criteria. Selections of simulated communities are based on simple logical tests derived from thresholds to one or more calculated parameters. In the deterministic selection, only simulations that are true for all tests are returned. First, select the parameters which will be used to test, then set the sliders as logical tests.")
        infoRV$testsDetSelInputInfo <- input$testsDetSelInputInfo
      }
    }
    if(!is.null(input$testsHieSelInputInfo)){
      if(input$testsHieSelInputInfo>infoRV$testsHieSelInputInfo){
        infoText <- i18n$t("Hierarchical selection criteria. Selections of simulated communities are based on simple logical tests derived from thresholds to one or more calculated parameters. In the hierarchical selection, the tests are evaluated hierarchically. If all simulations fail in the first test, the function tries the next test. First, select the parameters which will be used to test, then set the sliders as logical tests. The hierarchical selection is more flexible, given that it includes additional arguments to the selection.")
        infoRV$testsHieSelInputInfo <- input$testsHieSelInputInfo
      }
    }
    if(!is.null(input$speficyGroupsSelInputInfo)){
      if(input$speficyGroupsSelInputInfo>infoRV$speficyGroupsSelInputInfo){
        infoText <- i18n$t("Hierarchical selection allows set site groups. Thus, selection criteria are applied inside a specific site group. Set a variable which determines the site groups for hierarchical selection.")
        infoRV$speficyGroupsSelInputInfo <- input$speficyGroupsSelInputInfo
      }
    }
    if(!is.null(input$singleSelectionInputInfo)){
      if(input$singleSelectionInputInfo>infoRV$singleSelectionInputInfo){
        infoText <- i18n$t("Selection method in hierarchical selection. The method 'single' returns only one simulation or one by site group. In the last step, the function samples only one simulation among those that passed all the tests. The method 'multiple' returns all simulations that passed all the tests.")
        infoRV$singleSelectionInputInfo <- input$singleSelectionInputInfo
      }
    }
    if(!is.null(infoText)){
      # shiny::showModal(shiny::modalDialog(title = NULL,
      # 									infoText, 
      # 									footer = shiny::modalButton(i18n$t("Dismiss")),
      # 									fade = FALSE,
      # 									size = "s",
      # 									easyClose = TRUE), session = session)
      shinyalert::shinyalert(
        title = "",
        text = infoText,
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
    }
    # }
  })
  ## Output ----
  ### Output diagram - diagramOutput ----
  output$diagramOutput <- DiagrammeR::renderGrViz({
    DiagrammeR::grViz(paste0(
      "digraph resbiotaFlowchart {
      # define node aesthetics
      graph[splines = ortho]
      node [fontname = Helvetica, shape = box, width = 5, height = 1, fontsize = 16, style = filled, fillcolor = whitesmoke]
      bgcolor = transparent
      
      # Nodes
      ",
      "loadData[label = <", i18n$t("Load data, at least species trait data<br/><i>Data input tab"), "</i>>]
      simulateCommunities[label = <", i18n$t("Set simulation input parameters<br/><i>Simulate tab"), "</i>>]
      adjustSimulations[label = <", i18n$t("Simulated communities can be adjusted<br/><i>Simulate tab"), "</i>>]
      computeParameters[label = <", i18n$t("Compute basic parameters in each simulated community<br/><i>Compute tab"), "</i>>]
      standardizeParameters[label = <", i18n$t("Calculated parameters can be standardised<br/><i>Compute tab"), "</i>>]
      computeMultifunctionality[label = <", i18n$t("Calculate multiple restoration targets, called multifunctionality<br/><i>Compute tab"), "</i>>]
      selectCommunities[label = <", i18n$t("Selections of simulated communities<br/><i>Select tab"), "</i>>]
      viewResults[label = <", i18n$t("Basic results visualization<br/><i>View tab"), "</i>>]
      viewMultifunctionality[label = <", i18n$t("Multifunctionality visualization<br/><i>View tab"), "</i>>]
      extractResults[label = <", i18n$t("Extract and save the results<br/><i>View tab"), "</i>>]",
      
      "# Edges
      loadData -> simulateCommunities
      simulateCommunities -> computeParameters
      simulateCommunities -> adjustSimulations
      adjustSimulations -> computeParameters
      computeParameters -> standardizeParameters
      computeParameters -> computeMultifunctionality
      computeParameters -> selectCommunities
      standardizeParameters -> selectCommunities
      computeMultifunctionality -> selectCommunities
      standardizeParameters -> computeMultifunctionality
      selectCommunities -> viewResults 
      selectCommunities -> viewMultifunctionality 
      selectCommunities -> extractResults
     }"
    ))
  })
  ### Output aux - outputRankList ----
  shiny::observeEvent(input$testsHieSelInput, ignoreNULL = FALSE, {
    inputParSelRV$auxRankHeiSel <- input$testsHieSelInput
  })
  output$outputRankList <- shiny::renderUI({
    if(length(inputParSelRV$auxRankHeiSel)>1){
      sortable::rank_list(
        input_id = "rankHeiSelInput",
        text = i18n$t("Drag to rank parameters in desired order"),
        labels = inputParSelRV$auxRankHeiSel
      )
    } else {
      NULL
    }
  })
  ### Output aux - showSlidersTestsHieSel ----
  output$showSlidersTestsHieSel <- shiny::reactive({
    !is.null(input$testsHieSelInput)
  })
  outputOptions(output, "showSlidersTestsHieSel", suspendWhenHidden = FALSE)
  ### Output aux - testsDetSelInput ----
  output$showSlidersTestsDetSel <- shiny::reactive({
    !is.null(input$testsDetSelInput)
  })
  outputOptions(output, "showSlidersTestsDetSel", suspendWhenHidden = FALSE)
  ### Output aux - showSlidersMulti ----
  output$showSlidersMulti <- shiny::reactive({
    !is.null(input$testsMultiInput)
  })
  outputOptions(output, "showSlidersMulti", suspendWhenHidden = FALSE)
  
  
  ### Update text - xvar - View tab ----
  shiny::observeEvent(input$xvarViewInput, ignoreNULL = FALSE, {
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
  shiny::observeEvent(input$yvarViewInput, ignoreNULL = FALSE, {
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
  output$showTraitsData <- shiny::reactive({
    !is.null(inputDataRV[["traits"]])
  })
  outputOptions(output, "showTraitsData", suspendWhenHidden = FALSE)
  ### Output aux - showRestComp ----
  output$showRestComp <- shiny::reactive({
    !is.null(inputDataRV[["restComp"]])
  })
  outputOptions(output, "showRestComp", suspendWhenHidden = FALSE)
  ### Output aux - showRestGroup ----
  output$showRestGroup <- shiny::reactive({
    !is.null(inputDataRV[["restGroup"]])
  })
  outputOptions(output, "showRestGroup", suspendWhenHidden = FALSE)
  ### Output aux - showReference ----
  output$showReference <- shiny::reactive({
    !is.null(inputDataRV[["reference"]])
  })
  outputOptions(output, "showReference", suspendWhenHidden = FALSE)
  ### Output aux - showSupplementary ----
  output$showSupplementary <- shiny::reactive({
    !is.null(inputDataRV[["supplementary"]])
  })
  outputOptions(output, "showSupplementary", suspendWhenHidden = FALSE)
  ### Output text - countScenariosText ----
  output$countScenariosText <- shiny::renderText({
    paste0(i18n$t("Simulations scenarios: "),  resultsRV[["nSce"]])
  })
  ### Output text - countSimulationText ----
  output$countSimulationText <- shiny::renderText({
    paste0(i18n$t("Total simulations: "),  resultsRV[["nSim"]])
  })
  ### Output text - countSelectText ----
  output$countSelectText <- shiny::renderText({
    paste0(i18n$t("Selected scenarios: "),  resultsRV[["nSel"]])
  })
  ### Output text - countSimulationSelText ----
  output$countSimulationSelText <- shiny::renderText({
    paste0(i18n$t("Total simulations selected: "),  resultsRV[["nSimSel"]])
  })
  ### Output text - Simulate tab ----
  shiny::observeEvent(input$scenarioSimulateSummaryInput, {
    output$outputSimulateSummaryText <- shiny::renderUI({
      if(!is.null(input$scenarioSimulateSummaryInput)){
        x <- resultsRV$simulate[[input$scenarioSimulateSummaryInput]]
        str1 <- paste0("Pool size: ", ncol(x$simulation$composition))
        str2 <- paste0("Number of simulations: ", nrow(x$simulation$composition))
        str3 <- paste0("Reference communities: ", ifelse(is.null(x$reference), "No", "Yes"))
        str4 <- paste0("Supplementary communities: ", ifelse(is.null(x$supplementary), "No", "Yes"))
        if(!is.null(x$simulation$results)) {
          str5 <- paste0("Parameters: ")
          str6 <- paste0("&emsp;", colnames(x$simulation$results), collapse = "<br/>")
          shiny::HTML(paste(str1, str2, str3, str4, str5, str6, sep = "<br/>"))
        } else{
          str5 <- paste0("Parameters: ", ifelse(is.null(x$simulation$results), "No", "Yes"))
          shiny::HTML(paste(str1, str2, str3, str4, str5, sep = "<br/>"))
        }
      }
    })
  })
  ### Output text - Compute tab ----
  shiny::observeEvent(input$scenarioComputeSummaryInput, {
    output$outputComputeSummaryText <- shiny::renderUI({
      if(!is.null(input$scenarioComputeSummaryInput)){
        x <- resultsRV$simulate[[input$scenarioComputeSummaryInput]]
        str1 <- paste0(i18n$t("Pool size: "), ncol(x$simulation$composition))
        str2 <- paste0(i18n$t("Number of simulations: "), nrow(x$simulation$composition))
        str3 <- paste0(i18n$t("Reference communities: "), ifelse(is.null(x$reference), "No", "Yes"))
        str4 <- paste0(i18n$t("Supplementary communities: "), ifelse(is.null(x$supplementary), "No", "Yes"))
        if(!is.null(x$simulation$results)) {
          str5 <- paste0(i18n$t("Parameters: "))
          str6 <- paste0("&emsp;", colnames(x$simulation$results), collapse = "<br/>")
          shiny::HTML(paste(str1, str2, str3, str4, str5, str6, sep = "<br/>"))
        } else{
          str5 <- paste0(i18n$t("Parameters: "), ifelse(is.null(x$simulation$results), "No", "Yes"))
          shiny::HTML(paste(str1, str2, str3, str4, str5, sep = "<br/>"))
        }
      }
    })
  })
  ### Output text - Select tab ----
  shiny::observeEvent(input$scenarioSelectSummaryInput, {
    output$outputSelectSummaryText <- shiny::renderUI({
      if(!is.null(input$scenarioSelectSummaryInput)){
        x <- resultsRV$select[[input$scenarioSelectSummaryInput]]
        str1 <- paste0(i18n$t("Pool size: "), ncol(x$selection$composition))
        str2 <- paste0(i18n$t("Number of simulations selected: "), nrow(x$selection$composition))
        str3 <- paste0(i18n$t("Reference communities: "), ifelse(is.null(x$reference), "No", "Yes"))
        str4 <- paste0(i18n$t("Supplementary communities: "), ifelse(is.null(x$supplementary), "No", "Yes"))
        if(!is.null(x$selection$results)) {
          str5 <- paste0(i18n$t("Parameters: "))
          str6 <- paste0("&emsp;", colnames(x$selection$results), collapse = "<br/>")
          shiny::HTML(paste(str1, str2, str3, str4, str5, str6, sep = "<br/>"))
        } else{
          str5 <- paste0("Parameters: ", ifelse(is.null(x$selection$results), "No", "Yes"))
          shiny::HTML(paste(str1, str2, str3, str4, str5, sep = "<br/>"))
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
  output$plotParOutput <- shiny::renderPlot({
    resultsRV$plotPar
  })
  ### Output plot - plotMultiOutput ----
  output$plotMultiOutput <- shiny::renderPlot({
    resultsRV$plotMulti
  })
})
# END appServer ----