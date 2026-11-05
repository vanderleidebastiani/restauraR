#' @rdname viewResults2
#' @include viewResults2.R
#' @encoding UTF-8
#' @keywords MainFunction
#' @export
plotResultsRidges <- function(x, parameters, showReference = TRUE, showSupplementary = FALSE){
  # Check object class
  if(!c(inherits(x, "simRest") || inherits(x, "simRestSelect"))){
    stop("The x argument must be of class simRest or simRestSelect")
  }
  # Check parameters
  if (length(parameters) > 1) {
    stop("Only one parameter can be specified")
  }
  # Check names
  if(inherits(x, "simRest")){
    tableResults <- x$simulation$results
  } else{
    tableResults <- x$selection$results
  }
  varNames <- colnames(tableResults)
  if(!inherits(parameters, "character") || !all(parameters %in% varNames)){
    stop("The parameters argument must be a character vector specifying column names from the results")
  }
  dataPlot <- extractParameters(x, parameters, showReference = FALSE, showSupplementary = FALSE)
  if(min(table(dataPlot$Site))<4){
    stop("Results must include multiple simulations per site in order to calculate the density plots")
  }
  if(showReference){
    ref <- extractParameters(x, parameters, showReference = TRUE, showSupplementary = FALSE, showSimulation = FALSE)
  }
  if(showSupplementary){
    suple <- extractParameters(x, parameters, showSupplementary = TRUE, showReference = FALSE, showSimulation = FALSE)
  }
  p <- ggplot2::ggplot() +
    ggplot2::aes(x = .data[[parameters]], y = .data[["Site"]]) +
    ggridges::geom_density_ridges(data = dataPlot, fill = "#227AB5", col = "#1d4b61") +
    themeRestauraR(baseSize = 15)
  if(showReference || showSupplementary){
    pal <- c("#BD0026", "#C37614")
    names(pal) <- c("Reference", "Supplementary")
    if(showReference){
      p <- p +
        ggplot2::geom_rug(data = ref, mapping = ggplot2::aes(x = .data[[parameters]], col = "Reference"), linewidth = 1, inherit.aes = FALSE)
    }
    if(showSupplementary){
      p <- p +
        ggplot2::geom_rug(data = suple, mapping = ggplot2::aes(x = .data[[parameters]], col = "Supplementary"), linewidth = 1, inherit.aes = FALSE)
    }
    p <- p +
      ggplot2::scale_colour_manual(values = pal) +
      ggplot2::labs(colour = "Legend")
  }
  return(p)
}