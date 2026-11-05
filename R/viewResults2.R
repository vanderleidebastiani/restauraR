#' @title Visualise simulation results
#' @description Generate graphical representations of simulated and selected communities, including histograms for parameter distributions, scatter plots for trade-off analyses, and upset plots for multifunctionality results. 
#' Reference sites are highlighted as distinct points in scatter plots or as rug lines along the axes in histograms.
#' @encoding UTF-8
#' @importFrom ggplot2 ggplot aes geom_point geom_bar geom_histogram geom_line geom_rug geom_polygon geom_linerange geom_vline geom_hline theme element_text scale_color_manual scale_fill_manual labs
#' @importFrom ComplexUpset upset intersection_size upset_set_size
#' @importFrom grDevices nclass.FD chull
#' @importFrom scales pal_hue
#' @importFrom data.table data.table melt
#' @importFrom ggridges geom_density_ridges
#' @importFrom stats ave
#' @param x An object of class "simRest" or "simRestSelect" to visualise results.
#' @param type Plot type, "Histogram", "ScatterPlot", "RidgesPlot", "ParallelCoordinate", "SpeciesRank", "DumbbellPlot", "ConvexHull" or "UpsetPlot". See details.
#' @param parameters Character vector specifying the variable name for the histogram or the x-axis in scatter plots. y-axis
#' @param highlight Colour highlight, "ResultType", "Site" or "Simulation". See details.
#' @param traits Data frame or matrix with species traits. Traits as columns and species as rows.
#' @param showReference Logical indicating whether to include reference sites from visualisation (default showReference = TRUE).
#' @param showSupplementary Logical indicating whether to include supplementary sites from visualisation (default showSupplementary = TRUE).
#' @param showMultisite Logical indicating whether to plot results from the multisite analysis; otherwise, the plot shows the basic results (default showMultisite = FALSE).
#' @param testsFilter Character vector of logical tests for selection.
#' @param ... Arguments passed to the upset function of the package ComplexUpset.
#' @returns A ggplot plot object that can be further customised using standard ggplot2 syntax.
#' @author See \code{\link{restauraR-package}}.
#' @seealso \code{\link{simulateCommunities}}, \code{\link{computeParameters}}, \code{\link{selectCommunities}}, \code{\link{extractResults}}
#' @references
#' Coutinho, A. G., Carlucci, M. B., & Cianciaruso, M. V. (2023). A framework to apply trait-based ecological 
#' restoration at large scales. Journal of Applied Ecology, 60, 1562–1571. https://doi.org/10.1111/1365-2664.14439
#' 
#' Coutinho, A. G., Nunes, A., Branquinho, C., Debastiani, V. J., Carlucci, M. B., & Cianciaruso, M. V. (2026). Boosting 
#' multifunctionality through adaptive trait-based species addition in ongoing restoration projects. 
#' Ecological Applications, 36, e70197. https://doi.org/10.1002/eap.70197
#' @keywords MainFunction
#' @export
viewResults2 <- function(x, type, parameters, highlight = c("ResultType", "Site", "Simulation"), traits = NULL, showReference = TRUE, showSupplementary = FALSE, showMultisite = FALSE, testsFilter = NULL){
  # Check object class
  if(!c(inherits(x, "simRest") || inherits(x, "simRestSelect"))){
    stop("The x argument must be of class simRest or simRestSelect")
  }
  TYPE <- c("Histogram", "ScatterPlot", "RidgesPlot", "ParallelCoordinate", "SpeciesRank", "DumbbellPlot", "ConvexHull", "UpsetPlot")
  typeTest <- pmatch(type, TYPE)
  typeTest <- match.arg(type, TYPE)
  p <- switch(typeTest,
    "Histogram" = plotResultsHistogram(x = x, parameters = parameters, showReference = showReference, showSupplementary = showSupplementary, showMultisite = showMultisite),
    "ScatterPlot" = plotResultsScatter(x = x, parameters = parameters, showReference = showReference, showSupplementary = showSupplementary, showMultisite = showMultisite, testsFilter = testsFilter),
    "RidgesPlot" = plotResultsRidges(x = x, parameters = parameters, showReference = showReference, showSupplementary = showSupplementary),
    "ParallelCoordinate" = plotResultsParallelCoordinate(x = x, parameters = parameters, highlight = highlight, showReference = showReference, showSupplementary = showSupplementary),
    "SpeciesRank" = plotResultsSpeciesRank(x = x, highlight = highlight, showReference = showReference, showSupplementary = showSupplementary),
    "DumbbellPlot" = plotResultsDumbbell(x = x, parameters = parameters, showReference = showReference),
    "ConvexHull" = plotResultsConvexHull(x = x, parameters = parameters, traits = traits, showReference = showReference, showSupplementary = showSupplementary),
    "UpsetPlot" = plotResultsUpset(x = x, showReference = showReference)
  )
  if(is.null(p)){
    p <- ggplot2::ggplot() +
      themeRestauraR(baseSize = 15)
  }
  return(p)
}