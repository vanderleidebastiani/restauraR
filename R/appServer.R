#' @rdname app
#' @export
appServer <- shiny::shinyServer(function(input, output, session) {
  # Input parameters to simulateCommunities
  ### Reactive Values ----
  inputDataRV <- shiny::reactiveValues(traits = NULL,
                                       restComp = NULL,
                                       restGroup = NULL,
                                       reference = NULL,
                                       supplementary = NULL,
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
                                         singleselection = NULL)
  resultsRV <- shiny::reactiveValues(nSce = 0,
                                     nSim = 0,
                                     simulate = list(),
                                     nSel = 0,
                                     select = list())
  ### Input file ----
  observeEvent(input$traitsInput, {
    # Read file
    inFile <- input$traitsInput
    if (is.null(inFile)){
      inputDataRV$traits <- NULL
    }
    inputDataRV$traits <- read.csv(inFile$datapath, sep = input$fileSep, row.names = 1)
    # Extract basic data information
    inputDataRV$auxVariables <- colnames(inputDataRV$traits)
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
  }) # End input file
  
  ### Input file ----
  observeEvent(input$restCompInput, {
    # Read file
    inFile <- input$restCompInput
    if (is.null(inFile)){
      inputDataRV$restComp <- NULL
    }
    inputDataRV$restComp <- read.csv(inFile$datapath, sep = input$fileSep, row.names = 1)
    print(rownames(inputDataRV$restComp))
  }) # End input file
  
  
  ### Input file ----
  observeEvent(input$restGroupInput, {
    # Read file
    inFile <- input$restGroupInput
    if (is.null(inFile)){
      inputDataRV$restGroup <- NULL
    }
    inputDataRV$restGroup <- read.csv(inFile$datapath, sep = input$fileSep)
  }) # End input file
  
  
  # CONTINUAR, remover tambem outros elementos da interface
  ### doClear ----
  observeEvent(input$doClear, {
    inputDataRV$traits <- NULL
    inputDataRV$restComp <- NULL
    inputDataRV$restGroup <- NULL
    inputDataRV$reference <- NULL
    inputDataRV$supplementary <- NULL
    inputDataRV$auxTraitsVariables <- NULL
  })
  
  
  
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
                                    # restGroup = inputParSimRV$restGroup, # Ok
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
    sendSweetAlert(
      session = session,
      title = "Done!!",
      text = paste0("Simulations scenarios: ", resultsRV$nSce),
      type = "success"
    )
  })
  ### doRemove ----
  observeEvent(input$doRemove, {
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
    sendSweetAlert(
      session = session,
      title = "Done!!",
      text = paste0("Simulations scenarios: ", resultsRV$nSce),
      type = "success"
    )
  })
  ### Output - countScenariosText ----
  output$countScenariosText <- shiny::renderText({
    paste0("Simulations scenarios: ",  resultsRV[["nSce"]])
  })
  
  output$countSimulationText <- shiny::renderText({
    paste0("Total simulations: ",  resultsRV[["nSim"]])
  })
  
  
  # TESTE
  # output$TESTE <- renderPrint(input$probAbunSimGrop2777)
  output$TESTE <- renderPrint(input$methodSimInput)
  
  ### Output - Traits data ----
  output$traitsData <- rhandsontable::renderRHandsontable({
    if(is.null(inputDataRV$traits)){
      return(NULL)
    }
    rhandsontable::rhandsontable(inputDataRV$traits, contextMenu =  FALSE, readOnly = TRUE, height = 400, stretchH = "all", rowHeaderWidth = 50) #%>%
    # hot_table(stretchH = 'all', rowHeaderWidth = 50) # %>%
    # hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    # hot_context_menu(rhandsontable::rhandsontable(inputDataRV$traits, readOnly = TRUE, height = 400, stretchH = 'all', rowHeaderWidth = 50), allowRowEdit = FALSE, allowColEdit = FALSE)
  })
})

