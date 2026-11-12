#' @rdname viewResults2
#' @include viewResults2.R
#' @encoding UTF-8
#' @keywords MainFunction
#' @export
plotResultsConvexHull <- function(x, parameters, traits, showReference = TRUE, showSupplementary = FALSE){
  # Check object class
  if(!inherits(x, "simRestSelect")){
    stop("The x argument must be of class simRestSelect")
  }
  # Check parameters
  if (length(parameters) != 2) {
    stop("The argument parameters must be length of 2")
  }
  xvar <- parameters[1]
  yvar <- parameters[2]
  # Extract species names
  sppSim <- colSums(x$selection$composition)
  sppSim <- names(sppSim)[sppSim>0]
  if(showReference && !is.null(x$reference)){
    sppRef <- colSums(x$reference$composition)
    sppRef <- names(sppRef)[sppRef>0]
  } else{
    sppRef <- NULL
  }
  if(showSupplementary && !is.null(x$supplementary)){
    sppSuple <- colSums(x$supplementary$composition)
    sppSuple <- names(sppSuple)[sppSuple>0]
  } else{
    sppSuple <- NULL
  }
  # Check names
  traitNames <- colnames(traits)
  if(!inherits(parameters, "character") || !all(parameters %in% traitNames)){
    stop("The parameters argument must be a character vector specifying column names from the traits")
  }
  if(length(sppSim) < 3){
    stop("Convex hull requires at least 3 species in simulation composition")
  }
  chullSim <- grDevices::chull(traits[sppSim, parameters, drop = FALSE])
  dataPlot <- traits[sppSim[chullSim], , drop = FALSE]
  dataPlot$Legend <- "Simulation"
  if(!is.null(sppRef)){
    if(length(sppRef) < 3){
      stop("Convex hull requires at least 3 species in reference composition")
    }
    chullRef <- grDevices::chull(traits[sppRef, parameters, drop = FALSE])
    dataPlotRef <- traits[sppRef[chullRef], , drop = FALSE]
    dataPlotRef$Legend <- "Reference"
    dataPlot <- rbind.data.frame(dataPlot, dataPlotRef)
  } 
  if(!is.null(sppSuple)){
    if(length(sppSuple) < 3){
      stop("Convex hull requires at least 3 species in supplementary composition")
    }
    chullSuple <- grDevices::chull(traits[sppSuple, parameters, drop = FALSE])
    dataPlotSuple <- traits[sppSuple[chullSuple], , drop = FALSE]
    dataPlotSuple$Legend <- "Supplementary"
    dataPlot <- rbind.data.frame(dataPlot, dataPlotSuple)
  }
  dataPlot$Legend <- factor(dataPlot$Legend, levels = c("Simulation", "Reference", "Supplementary"))
  traits <- data.frame(Species = rownames(traits), traits)
  # pal <- c("#1d4b61", "#BD0026", "#C37614")
  pal <- paletteRestauraR()[c("DarkBlue", "Magenta", "Orange")]
  names(pal) <- c("Simulation", "Reference", "Supplementary")
  p <- ggplot2::ggplot() +
    ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]]) +
    ggplot2::geom_polygon(data = dataPlot, mapping = ggplot2::aes(group = .data[["Legend"]], fill = .data[["Legend"]]), alpha = 0.5) +
    suppressWarnings(ggplot2::geom_point(data = traits, size = 1.6, mapping = ggplot2::aes(customdata = .data[["Species"]]))) +
    ggplot2::scale_fill_manual(values = pal) +
    themeRestauraR(baseSize = 15)
  return(p)
}