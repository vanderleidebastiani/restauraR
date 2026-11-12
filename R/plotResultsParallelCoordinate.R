#' @rdname viewResults2
#' @include viewResults2.R
#' @encoding UTF-8
#' @keywords MainFunction
#' @export
plotResultsParallelCoordinate <- function(x, parameters, highlight = c("ResultType", "Site", "Simulation"), showReference = TRUE, showSupplementary = FALSE){
  # Check object class
  if(!inherits(x, "simRestSelect")){
    stop("The x argument must be of class simRestSelect")
  }
  # Check parameters
  if(length(parameters)<2){
    stop("Parallel coordinate plots require at least two parameters")
  }
  # Check names
  varNames <- colnames(x$selection$results)
  if(!inherits(parameters, "character") || !all(parameters %in% varNames)){
    stop("The parameters argument must be a character vector specifying column names from the results")
  }
  HIGHLIGHT <- c("ResultType", "Site", "Simulation")
  highlight <- match.arg(highlight, HIGHLIGHT)
  varClass <- sapply(x$selection$results[, parameters, drop = FALSE], vectorClass)
  if(any(!varClass %in% c("numeric"))){
    stop("All parameters must be numeric")
  }
  # Extract parameters data
  res <- extractParameters(x, parameters, showSimulation = TRUE, showReference = showReference, showSupplementary = showSupplementary)  
  res <- data.table::data.table(res)
  # Reshape data, attribute not preserved
  dataPlot <- data.table::melt(res, id.vars = 1:3, measure.vars = parameters, value.name = "Value", variable.name = "Variable") 
  dataPlot <- data.frame(dataPlot)
  # Define unique names for drawing lines
  dataPlot$lineGroup <- paste0(dataPlot$ResultType, dataPlot$Simulation, dataPlot$Site)
  if(highlight == "ResultType"){
    # pal <- c("#1d4b61", "#BD0026", "#C37614")
    pal <- paletteRestauraR()[c("DarkBlue", "Magenta", "Orange")]
    names(pal) <- c("Simulation", "Reference", "Supplementary")
    # Reorder levels
    dataPlot$ResultType <- factor(dataPlot$ResultType, levels = c("Simulation", "Reference", "Supplementary"))
  }
  if(highlight == "Site"){
    simNames <- unique(dataPlot$Site[dataPlot$ResultType == "Simulation"])
    refNames <- unique(dataPlot$Site[dataPlot$ResultType == "Reference"])
    supNames <- unique(dataPlot$Site[dataPlot$ResultType == "Supplementary"])
    if(any(supNames %in% simNames) && showSupplementary){
      stop("Supplementary and simulation sites share names. Highlighting by sites is not possible")
    }
    # Set colours
    blues <- if(length(simNames) == 0) character(0) else if (length(simNames) == 1) paletteRestauraR()[["DarkBlue"]] else scales::pal_hue(h = c(195, 217), c = 100, l = 10)(length(simNames))
    reds <- if(length(refNames) == 0) character(0) else if (length(refNames) == 1) paletteRestauraR()[["Magenta"]] else scales::pal_hue(h = c(326, 346), c = 100, l = 10)(length(refNames))
    yellows <- if(length(supNames) == 0) character(0) else if (length(supNames) == 1) paletteRestauraR()[["Orange"]] else scales::pal_hue(h = c(45, 75), c = 100, l = 60)(length(supNames))
    pal <- c(blues, reds, yellows)
    names(pal) <- c(simNames, refNames, supNames)
    # Reorder levels
    dataPlot$Site<- factor(dataPlot$Site, levels = c(simNames, refNames, supNames))
  }
  if(highlight == "Simulation"){
    simNames <- unique(dataPlot$Simulation)
    simNames <- simNames[!is.na(simNames)]
    blues <- if(length(simNames) == 0) character(0) else if (length(simNames) == 1) paletteRestauraR()[["DarkBlue"]] else scales::pal_hue(h = c(195, 217), c = 100, l = 10)(length(simNames))
    # pal <- c("#BD0026", "#C37614", blues)
    pal <- c(paletteRestauraR()[c("Magenta", "Orange")], blues)
    names(pal) <- c("Reference", "Supplementary", simNames)
    dataPlot$Simulation <- ifelse(is.na(dataPlot$Simulation), dataPlot$ResultType, dataPlot$Simulation)
    dataPlot$Simulation <- factor(dataPlot$Simulation, levels = c(simNames, "Reference", "Supplementary"))
  }
  p <- ggplot2::ggplot() +
    ggplot2::aes(x = .data[["Variable"]], y = .data[["Value"]], col = .data[[highlight]]) +
    suppressWarnings(ggplot2::geom_point(data = dataPlot, size = 1.6, mapping = ggplot2::aes(customdata = .data[["Simulation"]]))) +
    ggplot2::geom_line(data = dataPlot, mapping = ggplot2::aes(group = .data[["lineGroup"]]), linewidth	= 0.6) +
    ggplot2::scale_colour_manual(values = pal) +
    ggplot2::labs(colour = "Legend") +
    themeRestauraR(baseSize = 15)
  return(p)
}