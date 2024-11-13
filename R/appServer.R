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
  
  ### Reactive Values ----
  inputDataRV <- shiny::reactiveValues(traits = NULL,
                                       restComp = NULL,
                                       restGroup = NULL,
                                       reference = NULL,
                                       supplementary = NULL,
                                       auxTraitsClass = NULL,
                                       auxTraitsVariables = NULL)
  inputParSimRV <- shiny::reactiveValues(# ava = NULL, # Ok
    restComp = NULL,
    restGroup = NULL,
    # und = NULL, # Ok
    it = NULL, # Ok
    # richMin = NULL, # Removed
    # richMax = NULL, # Removed
    rich = NULL, # Ok
    # cwm = NULL, # Ok
    # rao = NULL, # Ok
    prob = NULL,
    # phi = NULL, # Ok
    nInd = NULL, # Ok
    cvAbund = NULL, # Ok
    prefix = NULL, # Ok
    # method = NULL, # Ok
    group = NULL, # Ok
    probGroupRich = NULL, # Ok
    probGroupAbund = NULL # Ok
  )
  inputParComRV <- shiny::reactiveValues(ava = NULL,
                                         cwm = NULL,
                                         cwv = NULL,
                                         rao = NULL,
                                         cost = NULL,
                                         dens = NULL,
                                         stan = NULL)
  inputParSelRV <- shiny::reactiveValues(testsDet = NULL,
                                         testsHie = NULL,
                                         group = NULL,
                                         singleselection = NULL,
                                         auxRankHeiSel = NULL)
  resultsRV <- shiny::reactiveValues(nSce = 0,
                                     nSim = 0,
                                     simulate = list(),
                                     nSel = 0,
                                     select = list())
  
  ### Input file - Traits data ----
  observeEvent(input$traitsInput, {
    # Read file
    inFile <- input$traitsInput
    if (is.null(inFile)){
      inputDataRV$traits <- NULL
    }
    inputDataRV$traits <- read.csv(inFile$datapath, sep = input$fileSep, row.names = 1)
    
    
    # Extract basic data information
    inputDataRV$auxVariables <- colnames(inputDataRV$traits)
    
    
    inputDataRV$auxTraitsClass <- data.frame(t(sapply(inputDataRV$traits, classSimplerVector)), row.names = "Class")
    
    
    ### Update input ----
    updatePickerInput(session, inputId = "avaSimInput", choices = inputDataRV$auxVariables)
    updatePickerInput(session, inputId = "undSimInput", choices = inputDataRV$auxVariables)
    updatePickerInput(session, inputId = "cwmSimInput", choices = inputDataRV$auxVariables)
    updatePickerInput(session, inputId = "raoSimInput", choices = inputDataRV$auxVariables)
    updatePickerInput(session, inputId = "probSimInput", choices = inputDataRV$auxVariables)
    updatePickerInput(session, inputId = "groupSimInput", choices = inputDataRV$auxVariables)
    
    updateNumericInput(session, inputId = "richMinSimInput", value = nrow(inputDataRV$traits))
    updateNumericInput(session, inputId = "richMaxSimInput", value = nrow(inputDataRV$traits))
    
    updateSliderTextInput(session, inputId = "richSliderSimInput",
                          selected = c(1, nrow(inputDataRV$traits)),
                          choices = seq_len(nrow(inputDataRV$traits)))
    
    updatePickerInput(session, inputId = "avaComInput", choices = inputDataRV$auxVariables)
    updatePickerInput(session, inputId = "cwmComInput", choices = inputDataRV$auxVariables)
    updatePickerInput(session, inputId = "cwvComInput", choices = inputDataRV$auxVariables)
    updatePickerInput(session, inputId = "raoComInput", choices = inputDataRV$auxVariables)
    updatePickerInput(session, inputId = "costComInput", choices = inputDataRV$auxVariables)
    updatePickerInput(session, inputId = "densComInput", choices = inputDataRV$auxVariables)
    
  }) # End input file
  
  ### Input file ----
  observeEvent(input$restCompInput, {
    # Read file
    inFile <- input$restCompInput
    if (is.null(inFile)){
      inputDataRV$restComp <- NULL
    }
    inputDataRV$restComp <- read.csv(inFile$datapath, sep = input$fileSep, row.names = 1)
  }) # End input file
  
  
  ### Input file ----
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
  
  
  # CONTINUAR, remover tambem outros elementos da interface
  ### doClear ----
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
  
  observeEvent(input$selectedLanguage, {
    shiny.i18n::update_lang(input$selectedLanguage, session)
  })
  
  # observeEvent(input$doClear2, {
  # 	updateBox(
  # 		"box", 
  # 		action = "update", 
  # 		options = list(
  # 			footer = NULL
  # 		),
  # 		session = session
  # 	)	
  # })
  
  
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
  
  # observeEvent(input$surf-info, {
  # 	shinyBS::bsPopover(
  # 		id = "surf-info",
  # 		# title = "More information",
  # 		title = NULL,
  # 		# content = shiny::HTML(paste0(
  # 		# 	i18n$t("Ribeye steak, grilled jumbo shrimp, butter roasted potato medley grilled asparagus.")
  # 		# )),
  # 		# content = shiny::HTML("Test:"),
  # 		content = shiny::HTML(i18n$t("Test:")),
  # 		# content = as.character(i18n$t("Test:")),
  # 		# content = testRV$t1,
  # 		placement = "right",
  # 		trigger = "hover",
  # 		options = list(container = "body")
  # 	)
  # })
  # AQUI BS ----
  addPopover(session, 
             id = "surf-info", 
             # title = "Data", 
             title = NULL, 
             # content = paste0("Waiting time between "), 
             content = i18n$t("Test:"), 
             placement = "right",
             trigger = "hover")
  
  
  # observeEvent(input$richMinSimInput, {
  # 	if(!is.na(input$richMinSimInput)){
  # 		inputParSimRV$richMin <- input$richMinSimInput
  # 	} else{
  # 		inputParSimRV$richMin <- NA
  # 	}
  # 	inputParSimRV$rich <- c(input$richMinSimInput, input$richMaxSimInput)
  # 	inputParSimRV$rich <- inputParSimRV$rich[!is.na(inputParSimRV$rich)]
  # })
  # observeEvent(input$richMaxSimInput, {
  # 	if(!is.na(input$richMaxSimInput)){
  # 		inputParSimRV$richMax <- input$richMaxSimInput
  # 	} else{
  # 		inputParSimRV$richMax <- NA
  # 	}
  # 	inputParSimRV$rich <- c(input$richMinSimInput, input$richMaxSimInput)
  # 	inputParSimRV$rich <- inputParSimRV$rich[!is.na(inputParSimRV$rich)]
  # })
  observeEvent(input$richSliderSimInput, {
    inputParSimRV$rich <- input$richSliderSimInput
    inputParSimRV$rich <- inputParSimRV$rich[!is.na(inputParSimRV$rich)]
  })
  
  
  toListen2 <- reactive({
    list(input$goalsSimInput, inputDataRV$restComp, inputDataRV$restGroup)
  })
  observeEvent(toListen2(), {
    if(input$goalsSimInput == "New"){
      inputParSimRV$restComp <- NULL
      inputParSimRV$restGroup <- NULL
    } else{ # Ongoing
      inputParSimRV$restComp <- inputDataRV$restComp
      inputParSimRV$restGroup <- inputDataRV$restGroup
    }
  })
  
  
  toListen <- reactive({
    list(input$methodSimInput, input$nIndSimInput, input$cvAbundSimInput)
  })
  observeEvent(toListen(), {
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
  observeEvent(input$prefixSimInput, {
    if(!input$prefixSimInput == ""){
      inputParSimRV$prefix <- input$prefixSimInput
      updateActionButton(session, "doSimulate", disabled = FALSE)
    } else{
      updateActionButton(session, "doSimulate", disabled = TRUE)
    }
  })
  
  
  ### Probs sliders ----
  observeEvent(input$groupSimInput, {
    output$slidersProbRicSim <- renderUI({
      inVars <- unique(inputDataRV$traits[, input$groupSimInput])
      pvars <- length(inVars)
      if (pvars > 0) {
        lapply(seq(pvars), function(i) {
          sliderInput(inputId = paste0("probRichSimGrop", inVars[i]), label = paste0("Probability to draw richness - Group: ", inVars[i]), 
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
          sliderInput(inputId = paste0("probAbunSimGrop", inVars[i]), label = paste0("Probability to draw abundance - Group: ", inVars[i]), 
                      value = 1/length(inVars),
                      min = 0,
                      max = 1)
        })
      }
    })
  })
  
  
  # Check doCompute button 
  observeEvent(input$scenarioComInput, ignoreNULL = FALSE, {
    if(is.null(input$scenarioComInput)){
      updateActionButton(session, "doCompute", disabled = TRUE)
    } else(
      updateActionButton(session, "doCompute", disabled = FALSE)
    )
  })
  
  # AQUI ----
  ### Probs sliders HieSel ----
  observeEvent(input$testsHieSelInput, {
    output$slidersTestsHieSel <- renderUI({
      inVars <- input$testsHieSelInput
      pvars <- length(inVars)
      # print(inVars)
      scenario <- resultsRV$simulate[[input$scenarioSelInput]]
      scenario <- scenario$simulation$results
      
      if (pvars > 0) {
        lapply(seq(pvars), function(i) {
          # Incluir "factor"
          # all(is.na(scenario[,inVars[i]])) # TODOS NA - Class logical
          # all(y == floor(y), na.rm = TRUE) # So inteiros
          # all(is.na(y)) # TODOS NA
          if(classSimplerVector(scenario[,inVars[i]]) == "numeric"){
            # If integers
            if(all(scenario[,inVars[i]] == floor(scenario[,inVars[i]]), na.rm = TRUE)){
              shinyWidgets::sliderTextInput(inputId = paste0("logicalTestHieSelInput", inVars[i]),
                                            label = paste0("Test: ", inVars[i]),
                                            choices = seq(min(scenario[,inVars[i]], na.rm = TRUE), max(scenario[,inVars[i]], na.rm = TRUE)),
                                            selected = c(min(scenario[,inVars[i]], na.rm = TRUE), max(scenario[,inVars[i]], na.rm = TRUE))
              )
            } else { # If reals
              choicesTemp <- round(seq(min(scenario[,inVars[i]], na.rm = TRUE), max(scenario[,inVars[i]], na.rm = TRUE), length.out = 100), digits = 3)
              shinyWidgets::sliderTextInput(inputId = paste0("logicalTestHieSelInput", inVars[i]),
                                            label = paste0("Test: ", inVars[i]),
                                            choices = choicesTemp,
                                            selected = choicesTemp[c(1, 100)]
              )
            }
          } else {
            shinyWidgets::pickerInput(inputId = paste0("logicalTestHieSelInput", inVars[i]),
                                      label = paste0("Test: ", inVars[i]),
                                      choices = unique(scenario[,inVars[i]]),
                                      multiple = TRUE,
                                      options = list(`actions-box` = TRUE),
                                      inline = FALSE
            )
          }
          # updatePickerInput(session, inputId = "testsDetSelInput", choices = colnames(scenario$simulation$results))
        })
      }
    })
  })
  
  
  
  observeEvent(input$testsDetSelInput, {
    output$slidersTestsDetSel <- renderUI({
      inVars <- input$testsDetSelInput
      pvars <- length(inVars)
      # print(inVars)
      scenario <- resultsRV$simulate[[input$scenarioSelInput]]
      scenario <- scenario$simulation$results
      if (pvars > 0) {
        lapply(seq(pvars), function(i) {
          if(classSimplerVector(scenario[,inVars[i]]) == "numeric"){
            # If integers
            if(all(scenario[,inVars[i]] == floor(scenario[,inVars[i]]), na.rm = TRUE)){
              shinyWidgets::sliderTextInput(inputId = paste0("logicalTestDetSelInput", inVars[i]),
                                            label = paste0("Test: ", inVars[i]),
                                            choices = seq(min(scenario[,inVars[i]], na.rm = TRUE), max(scenario[,inVars[i]], na.rm = TRUE)),
                                            selected = c(min(scenario[,inVars[i]], na.rm = TRUE), max(scenario[,inVars[i]], na.rm = TRUE))
              )
            } else { # If reals
              choicesTemp <- round(seq(min(scenario[,inVars[i]], na.rm = TRUE), max(scenario[,inVars[i]], na.rm = TRUE), length.out = 100), digits = 3)
              shinyWidgets::sliderTextInput(inputId = paste0("logicalTestDetSelInput", inVars[i]),
                                            label = paste0("Test: ", inVars[i]),
                                            choices = choicesTemp,
                                            selected = choicesTemp[c(1, 100)]
              )
            }
          } else {
            shinyWidgets::pickerInput(inputId = paste0("logicalTestDetSelInput", inVars[i]),
                                      label = paste0("Test: ", inVars[i]),
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
  
  
  # observeEvent(input$speficyGroupsSelInput, {
  # 	scenario <- resultsRV$simulate[[input$scenarioSelInput]]
  # 	scenario <- scenario$simulation$results
  # 	updatePickerInput(session, inputId = "groupSelInput", choices = colnames(scenario))
  # })
  
  
  output$results_basic <- renderPrint({
    input$rankHeiSelInput # This matches the input_id of the rank list
  })
  
  # output$results_basic2 <- renderPrint({
  # 	input$xxprobRichSimGroprichness # This matches the input_id of the rank list
  # })
  
  
  
  
  ### doSimulate ----
  observeEvent(input$doSimulate, {
    showModal(modalDialog(title = "Running", footer = NULL))
    # Set and update the arguments group, probGroupRich and probGroupAbund
    if(input$speficyGroupsSimInput == "Yes" && !is.null(input$groupSimInput)){
      inputParSimRV$group <- input$groupSimInput
      inVars <- unique(inputDataRV$traits[, input$groupSimInput])
      pvars <- length(inVars)
      # if(input$probGroupTypeSimInput == "Richness"){
      # 	argListRichTemp <- vector("list", length = pvars)
      # 	names(argListRichTemp) <- inVars
      # 	for(i in 1:pvars){
      # 		argListRichTemp[[i]] <- input[[paste0("probRichSimGrop", inVars[i])]]
      # 	}
      # 	inputParSimRV$probGroupRich <- unlist(argListRichTemp, use.names = TRUE)
      # 	inputParSimRV$probGroupAbund <- NULL
      # }
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
                                    rich = inputParSimRV$rich, # Ok
                                    cwm = input$cwmSimInput, # straight input
                                    rao = input$raoSimInput, # straight input
                                    prob = input$probSimInput, # straight input
                                    phi = input$phiSimInput, # straight input
                                    nInd = inputParSimRV$nInd, # Ok
                                    cvAbund = inputParSimRV$cvAbund, # Ok
                                    prefix = inputParSimRV$prefix, # Ok
                                    method = tolower(input$methodSimInput), # straight input
                                    group = inputParSimRV$group, # Ok
                                    probGroupRich = inputParSimRV$probGroupRich, # Ok
                                    probGroupAbund = inputParSimRV$probGroupAbund # Ok
    )
    resultsRV$simulate[[inputParSimRV$prefix]] <- scenario
    resultsRV$nSim <- sum(sapply(resultsRV$simulate, function(x) nrow(x$simulation$composition)))
    resultsRV$nSce <- length(resultsRV$simulate)
    # Update picker
    updatePickerInput(session, inputId = "mergeSimulateInput", choices = names(resultsRV$simulate))
    updatePickerInput(session, inputId = "removeSimulateInput", choices = names(resultsRV$simulate))
    updatePickerInput(session, inputId = "scenarioComInput", choices = names(resultsRV$simulate))
    updatePickerInput(session, inputId = "scenarioViewInput", choices = names(resultsRV$simulate))
    removeModal()
    sendSweetAlert(
      session = session,
      title = "Done!!",
      text = paste0("Simulations scenarios: ", resultsRV$nSce),
      type = "success"
    )
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
    resultsRV$nSim <- sum(sapply(resultsRV$simulate, function(x) nrow(x$simulation$composition)))
    resultsRV$nSce <- length(resultsRV$simulate)
    # Update picker
    updatePickerInput(session, inputId = "mergeSimulateInput", choices = c(names(resultsRV$simulate)))
    updatePickerInput(session, inputId = "removeSimulateInput", choices = c(names(resultsRV$simulate)))
    updatePickerInput(session, inputId = "scenarioComInput", choices = names(resultsRV$simulate))
    updatePickerInput(session, inputId = "scenarioViewInput", choices = names(resultsRV$simulate))
    sendSweetAlert(
      session = session,
      title = "Done!!",
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
    resultsRV$nSce <- length(resultsRV$simulate)
    if(resultsRV$nSce>0){
      resultsRV$nSim <- sum(sapply(resultsRV$simulate, function(x) nrow(x$simulation$composition)))	
    } else{
      resultsRV$nSim <- 0
    }
    # Update picker
    updatePickerInput(session, inputId = "mergeSimulateInput", choices = c(names(resultsRV$simulate)))
    updatePickerInput(session, inputId = "removeSimulateInput", choices = c(names(resultsRV$simulate)))
    updatePickerInput(session, inputId = "scenarioComInput", choices = names(resultsRV$simulate))
    updatePickerInput(session, inputId = "scenarioViewInput", choices = names(resultsRV$simulate))
    sendSweetAlert(
      session = session,
      title = "Done!!",
      text = paste0("Simulations scenarios: ", resultsRV$nSce),
      type = "success"
    )
  })
  
  
  
  
  ### doCompute ----
  observeEvent(input$doCompute, {
    showModal(modalDialog(title = "Running", footer = NULL))
    scenario <- computeParameters(x = resultsRV$simulate[[input$scenarioComInput]],
                                  trait = inputDataRV$traits,
                                  ava = input$avaComInput, # straight input
                                  cwm = input$cwmComInput, # straight input
                                  cwv = input$cwvComInput, # straight input
                                  rao = input$raoComInput, # straight input
                                  cost = input$costComInput, # straight input
                                  dens = input$densComInput, # straight input
                                  reference = NULL,
                                  supplementary = NULL
    )
    resultsRV$simulate[[input$scenarioComInput]] <- scenario
    # Update picker
    
    
    updatePickerInput(session, inputId = "scenarioSelInput", choices = names(resultsRV$simulate))
    
    
    removeModal()
    sendSweetAlert(
      session = session,
      title = "Done!!",
      # text = paste0("Simulations scenarios: ", resultsRV$nSce),
      type = "success"
    )
  })
  
  
  
  ### doSelect ----
  observeEvent(input$doSelect, {
    # showModal(modalDialog(title = "Running", footer = NULL))
    # Set and update the arguments group, probGroupRich and probGroupAbund
    
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
        
        if(classSimplerVector(scenario[,inVars[i]]) == "numeric"){
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
        if(classSimplerVector(scenario[,inVars[i]]) == "numeric"){
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
    
    # resultsRV$simulate[[inputParSimRV$prefix]] <- scenario
    # resultsRV$nSim <- sum(sapply(resultsRV$simulate, function(x) nrow(x$simulation$composition)))
    # resultsRV$nSce <- length(resultsRV$simulate)
    # # Update picker
    # updatePickerInput(session, inputId = "mergeSimulateInput", choices = names(resultsRV$simulate))
    # updatePickerInput(session, inputId = "removeSimulateInput", choices = names(resultsRV$simulate))
    # updatePickerInput(session, inputId = "scenarioComInput", choices = names(resultsRV$simulate))
    # updatePickerInput(session, inputId = "scenarioViewInput", choices = names(resultsRV$simulate))
    # removeModal()
    sendSweetAlert(
      session = session,
      title = "Done!!",
      # text = paste0("Simulations scenarios: ", resultsRV$nSce),
      type = "success"
    )
  })
  
  
  
  observeEvent(input$scenarioSelInput, {
    scenario <- resultsRV$simulate[[input$scenarioSelInput]]
    updatePickerInput(session, inputId = "testsDetSelInput", choices = colnames(scenario$simulation$results))
    updatePickerInput(session, inputId = "testsHieSelInput", choices = colnames(scenario$simulation$results))
    updatePickerInput(session, inputId = "groupSelInput", choices = colnames(scenario$simulation$results))
  })
  
  
  ### Output - countScenariosText ----
  output$countScenariosText <- shiny::renderText({
    paste0("Simulations scenarios: ",  resultsRV[["nSce"]])
  })
  
  output$countSimulationText <- shiny::renderText({
    paste0("Total simulations: ",  resultsRV[["nSim"]])
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
  
  
  # TESTE ----
  
  observeEvent(input$testsHieSelInput, ignoreNULL = FALSE, {
    inputParSelRV$auxRankHeiSel <- input$testsHieSelInput
    print(input$testsHieSelInput)
  })
  
  output$outputRankList <- renderUI({
    if(length(inputParSelRV$auxRankHeiSel)>1){
      sortable::rank_list(
        input_id = "rankHeiSelInput",
        text = "Drag Curves in desired order",
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
  
  output$showSlidersTestsDetSel <- reactive({
    !is.null(input$testsDetSelInput)
  })
  outputOptions(output, "showSlidersTestsDetSel", suspendWhenHidden = FALSE)
  
  
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
  
  # AQUI ----
  # output$TESTE <- renderPrint(input$probAbunSimGrop2777)
  # output$TextTESTE <- renderText({
  # paste0(" Pool size: ")
  # x <- resultsRV[["simulate"]][[1]]
  # paste0(ncol(x$simulation$composition), "\n")
  # cat(" Number of simulations: ")
  # cat(nrow(x$simulation$composition), "\n")
  # cat(" Reference communities: ")
  # cat(ifelse(is.null(x$reference), "No", "Yes"), "\n")
  # cat(" Supplementary communities: ")
  # cat(ifelse(is.null(x$supplementary), "No", "Yes"), "\n")
  # cat(" Parameters: ")
  # if(!is.null(x$simulation$results)) {
  # 	cat("\n")
  # 	cat(paste("   ",colnames(x$simulation$results)), sep = "\n")
  # 	cat("\n")
  # } else{
  # 	cat(ifelse(is.null(x$simulation$results), "No", "Yes"), "\n\n")
  # }
  # 
  # })
  output$TextTESTE <- renderUI({
    # x <- resultsRV[["simulate"]][[1]]
    # x <- resultsRV$simulate[[input$scenarioViewInput]]
    # print(is.null(input$scenarioViewInput))
    if(!is.null(input$scenarioViewInput)){
      x <- resultsRV$simulate[[input$scenarioViewInput]]
      str1 <- paste0("Pool size: ", ncol(x$simulation$composition))
      str2 <- paste0("Number of simulations: ", nrow(x$simulation$composition))
      str3 <- paste0("Reference communities: ", ifelse(is.null(x$reference), "No", "Yes"))
      str4 <- paste0("Supplementary communities: ", ifelse(is.null(x$supplementary), "No", "Yes"))
      if(!is.null(x$simulation$results)) {
        # str5 <- paste0("Parameters: ", colnames(x$simulation$results), sep = '<br/>')
        str5 <- paste0("Parameters: ")
        # x <- scenarioB
        str6 <- paste0("&emsp;", colnames(x$simulation$results), collapse = '<br/>')
        HTML(paste(str1, str2, str3, str4, str5, str6, sep = '<br/>'))
      } else{
        str5 <- paste0("Parameters: ", ifelse(is.null(x$simulation$results), "No", "Yes"))
        HTML(paste(str1, str2, str3, str4, str5, sep = '<br/>'))
      }
    } else{
      HTML(paste("NULL"))
    }
  })
  
  
  
  
  
  ### Output table - Traits data ----
  output$outputTableTraitsData <- rhandsontable::renderRHandsontable({
    if(is.null(inputDataRV$traits)){
      return(NULL)
    }
    rhandsontable::rhandsontable(inputDataRV$traits, contextMenu =  FALSE, readOnly = TRUE, height = 550, stretchH = "all", rowHeaderWidth = 200)
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
  ## END appServer ----
})
