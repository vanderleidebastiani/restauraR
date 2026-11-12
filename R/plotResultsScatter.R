#' @rdname viewResults2
#' @include viewResults2.R
#' @encoding UTF-8
#' @keywords MainFunction
#' @export
plotResultsScatter <- function(x, parameters, showReference = TRUE, showSupplementary = FALSE, showMultisite = FALSE, testsFilter = NULL, showAvailability = FALSE){
  # Check object class
  if(!c(inherits(x, "simRest") || inherits(x, "simRestSelect"))){
    stop("The x argument must be of class simRest or simRestSelect")
  }
  if(!showMultisite){
    if(inherits(x, "simRest")){
      resResults <- x$simulation$results
      if(!is.null(testsFilter)){
        # pal <- c("#A6CEE4", "#227AB5", "#b5b5b5","#000000", "#BD0026", "#C37614")
        pal <- paletteRestauraR()[c("LightBlue", "DarkBlue", "LightGray", "DarkGray", "Magenta", "Orange")]
      } else{
        # pal <- c("#b5b5b5","#000000", "#BD0026", "#C37614")  
        pal <- paletteRestauraR()[c("LightGray", "DarkGray", "Magenta", "Orange")]
      }
    } else{
      resResults <- x$selection$results
      # pal <- c("#A6CEE4", "#227AB5", "#BD0026", "#C37614")
      pal <- paletteRestauraR()[c("LightBlue", "DarkBlue", "Magenta", "Orange")]
    }
    if(is.null(resResults)){
      stop("The x argument must contain the parameters calculated")
    }
    if(!is.null(testsFilter) && inherits(x, "simRest")){
      # Adjust string to test
      completeString <- adjString("resResults", testsFilter)
      # Evaluate test
      exprs <- lapply(completeString, function(x) parse(text = x, keep.source = FALSE))
      testsEval <- sapply(exprs, function(x) eval(x, envir = list(xPar = resResults)))
      selTemp <- apply(testsEval, 1, all)
      
      varNames <- colnames(resResults)
      if(!inherits(parameters, "character") || !all(parameters %in% varNames)){
        stop("The xvar and yvar arguments must be a character vector specifying column names from the results")
      }
      # Check parameters
      if (length(parameters) != 2) {
        stop("The argument parameters must be length of 2")
      }
      varNamesTestsEval <- extractNamesLogicTest(parameters[1], parameters[2], completeString)
      valuesTestsEval <- extractValuesLogicTest(completeString)
      if(showAvailability && !is.null(resResults$unavailable)){
        # Set names in the palette
        names(pal) <- c("Simulation selected - unavailable", "Simulation selected - available", "Simulation not selected - unavailable", "Simulation not selected - available", "Reference", "Supplementary")
        resResults$Legend <- ifelse(resResults$unavailable == 0 & selTemp, 
                                    "Simulation selected - available",
                                    ifelse(resResults$unavailable > 0 & selTemp,
                                           "Simulation selected - unavailable",
                                           ifelse(resResults$unavailable == 0 & !selTemp,
                                                  "Simulation not selected - available",
                                                  "Simulation not selected - unavailable")))
        resResults$Legend <- factor(resResults$Legend, c("Simulation selected - available", "Simulation selected - unavailable", 
                                                         "Simulation not selected - available", "Simulation not selected - unavailable", 
                                                         "Reference", "Supplementary"))
      } else{
        # Set names in the palette
        pal <- pal[c(2, 4:6)]
        names(pal) <- c("Simulation selected", "Simulation not selected", "Reference", "Supplementary")
        resResults$Legend <- ifelse(selTemp, "Simulation selected", "Simulation not selected")
        resResults$Legend <- factor(resResults$Legend, levels = c("Simulation selected", "Simulation not selected", "Reference", "Supplementary"))
      }
    } else{
      # Set legend to unavailable and available species
      if(showAvailability && !is.null(resResults$unavailable)){
        # Set names in the palette
        names(pal) <- c("Simulation - unavailable", "Simulation - available", "Reference", "Supplementary")
        resResults$Legend <- ifelse(resResults$unavailable == 0, "Simulation - available", "Simulation - unavailable")
        resResults$Legend <- factor(resResults$Legend, levels = c("Simulation - available", "Simulation - unavailable", "Reference", "Supplementary"))
      } else{
        # Set names in the palette
        pal <- pal[2:4]
        names(pal) <- c("Simulation", "Reference", "Supplementary")
        resResults$Legend <- "Simulation"
        resResults$Legend <- factor(resResults$Legend, levels = c("Simulation", "Reference", "Supplementary"))
      }
    }
  } else{
    if(inherits(x, "simRest")){
      resResults <- x$simulation$multisite$results
      # pal <- c("#000000")
      pal <- paletteRestauraR()[["DarkGray"]]
      
    } else{
      resResults <- x$selection$multisite$results
      # pal <- c("#1d4b61")
      pal <- paletteRestauraR()[["DarkBlue"]]
    }
    if(is.null(resResults)){
      stop("The x argument must contain the multisite results")
    }
    # Force hide reference 
    showReference <- FALSE
  }
  # Get reference results
  # ref <- x$reference$results 
  # Check names
  # parameters <- c(xvar, yvar)
  varNames <- colnames(resResults)
  if(!inherits(parameters, "character") || !all(parameters %in% varNames)){
    stop("The xvar and yvar arguments must be a character vector specifying column names from the results")
  }
  # Check parameters
  if (length(parameters) != 2) {
    stop("The argument parameters must be lenght of 2")
  }
  xvar <- parameters[1]
  yvar <- parameters[2]
  # Force hide reference if column names not found in reference results
  if(showReference){
    ref <- x$reference$results
    varNamesRef <- colnames(ref)
    if(!all(parameters %in% varNamesRef)){
      # Force hide reference 
      showReference <- FALSE
    }
  }
  if(showSupplementary){
    suple <- x$supplementary$results
    varNamesSuple <- colnames(suple)
    if(!all(parameters %in% varNamesSuple)){
      # Force hide reference 
      showSupplementary <- FALSE
    }  
  }
  if(showReference && !is.null(ref)){
    ref$Legend <- "Reference"
    ref$Legend <- factor(ref$Legend, levels = levels(resResults$Legend))
  }
  if(showSupplementary && !is.null(suple)){
    suple$Legend <- "Supplementary"
    suple$Legend <- factor(suple$Legend, levels = levels(resResults$Legend))
  }
  if(!showMultisite){
    p <- ggplot2::ggplot() +
      ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]], col = .data[["Legend"]]) +
      suppressWarnings(ggplot2::geom_point(data = resResults, size = 1.6, mapping = ggplot2::aes(customdata = .data[["Simulation"]]))) +
      themeRestauraR(baseSize = 15)
    if(showReference || showSupplementary){
      if(showReference){
        p <- p + 
          ggplot2::geom_point(data = ref, size = 1.7)
      } 
      if(showSupplementary){
        p <- p + 
          ggplot2::geom_point(data = suple, size = 1.7)
      }
    }
    p <- p +
      ggplot2::scale_colour_manual(values = pal) +
      ggplot2::labs(colour = "Legend")
    if(!is.null(testsFilter) && inherits(x, "simRest")){
      p <- p +
        ggplot2::geom_vline(xintercept =  valuesTestsEval[[which(varNamesTestsEval %in% xvar)]]) +
        ggplot2::geom_hline(yintercept =  valuesTestsEval[[which(varNamesTestsEval %in% yvar)]])
    }
  } else{
    # Else multisite results
    p <- ggplot2::ggplot() +
      ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]]) +
      suppressWarnings(ggplot2::geom_point(data = resResults, size = 1.6, col = pal, mapping = ggplot2::aes(customdata = .data[["Combination"]]))) +
      themeRestauraR(baseSize = 15)
  }
  return(p)
}