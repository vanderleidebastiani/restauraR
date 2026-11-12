#' @rdname viewResults2
#' @include viewResults2.R
#' @encoding UTF-8
#' @keywords MainFunction
#' @export
plotResultsHistogram <- function(x, parameters, showReference = TRUE, showSupplementary = FALSE, showMultisite = FALSE){
  # Check object class
  if(!c(inherits(x, "simRest") || inherits(x, "simRestSelect"))){
    stop("The x argument must be of class simRest or simRestSelect")
  }
  if(!showMultisite){
    if(inherits(x, "simRest")){
      tableResults <- x$simulation$results
    } else{
      tableResults <- x$selection$results
    }
    if(is.null(tableResults)){
      stop("The x argument must contain the parameters calculated")
    }
  } else{
    if(inherits(x, "simRest")){
      tableResults <- x$simulation$multisite$results
    } else{
      tableResults <- x$selection$multisite$results
    }
    if(is.null(tableResults)){
      stop("The x argument must contain the multisite results")
    }
    # Force hide reference and supplementary
    showReference <- FALSE
    showSupplementary <- FALSE
  }
  # Check names
  varNames <- colnames(tableResults)
  if(!inherits(parameters, "character") || !all(parameters %in% varNames)){
    stop("The parameters argument must be a character vector specifying column names from the results")
  }
  # Check parameters
  if (length(parameters) > 1) {
    stop("Only one parameter can be specified") 
  }
  # Get reference and supplementary results
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
  if(inherits(tableResults[, parameters], "factor") || inherits(tableResults[, parameters], "character")){
    p <- ggplot2::ggplot() +
      ggplot2::aes(x = .data[[parameters]]) +
      ggplot2::geom_bar(data = tableResults, fill = paletteRestauraR()[["DarkBlue"]], col = paletteRestauraR()[["White"]]) +
      ggplot2::labs(y = "Count") +
      themeRestauraR(baseSize = 15)
  } else{
    # Freedman-Diaconis method
    if(nrow(tableResults)>1){
      if(length(unique(tableResults[, parameters]))<2){
        binsTemp <- 1
      } else{
        binsTemp <- grDevices::nclass.FD(tableResults[, parameters])
      }
    } else{
      binsTemp <- 1
    }
    p <- ggplot2::ggplot() +
      ggplot2::aes(x = .data[[parameters]]) +
      ggplot2::geom_histogram(data = tableResults, bins = binsTemp, fill = paletteRestauraR()[["DarkBlue"]], col = paletteRestauraR()[["White"]]) +
      ggplot2::labs(y = "Count") +
      themeRestauraR(baseSize = 15)
  }
  if(showReference || showSupplementary){
    # pal <- c("#BD0026", "#C37614")
    pal <- paletteRestauraR()[c("Magenta", "Orange")]
    names(pal) <- c("Reference", "Supplementary")
    if(showReference){
        p <- p + 
          ggplot2::geom_rug(data = ref, mapping = ggplot2::aes(x = .data[[parameters]], col = "Reference"), linewidth = 1.2, inherit.aes = FALSE)
    } 
    if(showSupplementary){
      p <- p + 
        ggplot2::geom_rug(data = suple, mapping = ggplot2::aes(x = .data[[parameters]], col = "Supplementary"), linewidth = 1.2, inherit.aes = FALSE)
    }
    p <- p +
      ggplot2::scale_colour_manual(values = pal) +
      ggplot2::labs(colour = "Legend")
  } 
  return(p)
}