#' @title Visualise simulation results
#' @description Visualise simulated results using histograms, ridge plots, scatter plots, parallel coordinates, rank-abundance plots, convex hulls, before-and-after comparisons, and UpSet plots.
#' @details 
#' \strong{Overview}
#' 
#' Generate graphical representations of simulated results, including histograms and ridge plots for parameter distributions, scatter plots for 
#' trade-off analyses, parallel coordinates plots for multi-parameter visualisation, rank-abundance plots, convex hulls to visualise the functional trait space, 
#' before-and-after restoration target plots, and UpSet plots for multifunctionality results. Reference and supplementary sites can be highlighted as distinct points
#' in scatter plots or as rug lines along the x-axis in the other plots.
#' 
#' \strong{Plot types}
#' 
#' The available plot types are:
#' 
#' \strong{"Histogram"}
#' 
#' Draws a histogram of the distribution of a single parameter across all simulated or selected communities, or across multisite results. 
#' Reference and supplementary sites can be added as rug lines along the x-axis. Exactly one parameter must be supplied.
#' 
#' \strong{"ScatterPlot"}
#' 
#' Draws a scatter plot of two parameters (one per axis) to explore trade-offs and bivariate relationships among parameters of simulated 
#' or selected communities, or of multisite results. Reference and supplementary sites can be added as distinct points. The argument testsFilter can be supplied to 
#' classify simulations based on logical thresholds, highlighting selected versus non-selected simulations, and drawing vertical and horizontal threshold lines. Exactly
#' two parameters, one for each axis, must be supplied.
#' 
#' \strong{"RidgesPlot"}
#' 
#' Draws ridge density plots of a single parameter for each site, showing how the distribution of simulation outcomes varies across sites. 
#' This requires several simulations per site so that a density can be estimated. Reference and supplementary sites can be added as rug lines along the x-axis. Only one 
#' parameter may be supplied.
#' 
#' \strong{"ParallelCoordinate"}
#' 
#' Draws a parallel coordinate plot with each selected simulation as a line connecting its values across two or more parameters. 
#' This is useful for visualising multivariate trade-offs and identifying simulation profiles that perform well or poorly across several objectives simultaneously. 
#' Reference and supplementary sites can also be included. Both points and lines are coloured according to the grouping variable chosen in the highlight argument, which 
#' must be one of "ResultType", "Site" or "Simulation". At least two parameters must be supplied.
#' 
#' \strong{"SpeciesRank"}
#' 
#' Draws rank–abundance (Whittaker) plots, in which species are ranked from most to least abundant to visualise dominance and evenness 
#' in the simulated, reference and supplementary communities. Reference and supplementary sites can also be included. Both points and lines are coloured according to 
#' the grouping variable chosen in the highlight argument, which must be one of "ResultType", "Site" or "Simulation".
#' 
#' \strong{"DumbbellPlot"}
#' 
#' Draws a before-and-after comparison for a single parameter, from the pre-restoration value to the value obtained after the selected 
#' restoration solution. Reference sites can be added as rug lines along the x-axis. This plot requires that the object contains supplementary sites and only one selected 
#' simulation per site. Only one parameter may be supplied. To use this plot, supply the same species-composition matrix for restoration sites via the restComp argument 
#' of \code{simulateCommunities} and via the supplementary argument of \code{computeParameters}.
#' 
#' \strong{"ConvexHull"}
#' 
#' Draws species traits in a two-dimensional space and draws convex hulls enclosing the species present at the simulated, reference and 
#' supplementary sites. The hull area provides a visual proxy for the functional space each community occupies. The traits data frame must be supplied. Exactly two trait 
#' names, one for each axis, must be supplied in the parameters argument.
#' 
#' \strong{"UpsetPlot"}
#' 
#' Draws an UpSet plot of the multifunctionality results, showing the number of simulations that simultaneously achieve each combination of 
#' functions. Intersection sizes are shown as bars above the matrix, and set sizes are shown on the left. When the showReference argument is TRUE, the reference 
#' multifunctionality results are appended to the matrix.
#' 
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
#' @param parameters Character vector specifying the variable names for plots: length 1 for "Histogram"/"RidgesPlot"/"DumbbellPlot", length 2 for "ScatterPlot"/"ConvexHull", and length greater than or equal to 2 for "ParallelCoordinate". See details.
#' @param highlight Character vector used to colour points and lines, one of "ResultType", "Site" or "Simulation". See details.
#' @param traits Data frame or matrix with species traits. Traits as columns and species as rows.
#' @param showReference Logical indicating whether to include reference sites from visualisation (default showReference = TRUE).
#' @param showSupplementary Logical indicating whether to include supplementary sites from visualisation (default showSupplementary = FALSE).
#' @param showMultisite Logical indicating whether to plot results from the multisite analysis; otherwise, the plot shows the basic results (default showMultisite = FALSE).
#' @param testsFilter Character vector of logical tests used to highlight selected simulations in scatter plot. See details.
#' @param ... Arguments passed to the upset function of the package ComplexUpset.
#' @returns A ggplot plot object (or an object from the ComplexUpset package for UpsetPlot) that can be further customised using standard ggplot2 syntax.
#' @author See \code{\link{restauraR-package}}.
#' @seealso \code{\link{simulateCommunities}}, \code{\link{computeParameters}}, 
#' \code{\link{selectCommunities}}, \code{\link{extractResults}}
#' @references
#' Coutinho, A. G., Carlucci, M. B., & Cianciaruso, M. V. (2023). A framework to apply trait-based ecological 
#' restoration at large scales. Journal of Applied Ecology, 60, 1562–1571. https://doi.org/10.1111/1365-2664.14439
#' 
#' Coutinho, A. G., Nunes, A., Branquinho, C., Debastiani, V. J., Carlucci, M. B., & Cianciaruso, M. V. (2026). Boosting 
#' multifunctionality through adaptive trait-based species addition in ongoing restoration projects. 
#' Ecological Applications, 36, e70197. https://doi.org/10.1002/eap.70197
#' @keywords MainFunction
#' @examples
#' data("cerrado")
#' head(cerrado$traits)
#' # Simulation
#' scenario <- simulateCommunities(traits = cerrado$traits,
#'                                 restComp = cerrado$restoration,
#'                                 maxDiver = c("SLA", "Height", "Seed"),
#'                                 constCWM = "BT",
#'                                 rich = c(10, 15),
#'                                 it = 100)
#' scenario
#' # Compute functional parameters
#' scenario <- computeParameters(x = scenario,
#'                               traits = cerrado$traits,
#'                               ava = "Available",
#'                               cwm = "BT",
#'                               rao = c("SLA", "Height", "Seed"),
#'                               reference = cerrado$reference,
#'                               supplementary = cerrado$restoration)
#' scenario
#' # Compute multifunctionality
#' scenario <- computeMultifunctionality(scenario,
#'                                       tests = c("CWM_BT > 9",
#'                                                 "rao > 0.2"))
#' scenario
#' # Select communities - Priority selection - Multiple selection
#' scenarioSelectedMultiple <- selectCommunities(x = scenario,
#'                                               testsPriority = c("simpson > 0.98",
#'                                                                 "CWM_BT > 9"),
#'                                               siteGroup = "Site",
#'                                               singleSelection = FALSE)
#' scenarioSelectedMultiple
#' # Select communities - Priority selection - Single selection
#' scenarioSelectedSingle <- selectCommunities(x = scenario,
#'                                             testsPriority = c("simpson > 0.98",
#'                                                               "CWM_BT > 9"),
#'                                             siteGroup = "Site",
#'                                             singleSelection = TRUE)
#' scenarioSelectedSingle
#' # Optimise selection
#' scenarioOptimiseMultisite <- optimiseSelection(scenarioSelectedMultiple,
#'                                                siteGroup = "Site",
#'                                                traits = cerrado$traits,
#'                                                beta = c("SLA", "Height", "Seed"),
#'                                                calcTaxonomicBeta = TRUE)
#' scenarioOptimiseMultisite
#' # Select comunities - Optimise selection
#' scenarioOptimiseMultisiteSelected <- selectCommunities(x = scenarioOptimiseMultisite,
#'                                                        testsMultisite = c("betaDiversity == 'MAX'"))
#' scenarioOptimiseMultisiteSelected
#' # Histogram
#' viewResults2(scenario, 
#'              type = "Histogram", 
#'              parameters = "simpson", 
#'              showReference = TRUE, showSupplementary = TRUE)
#' viewResults2(scenarioOptimiseMultisite, 
#'              type = "Histogram", 
#'              parameters = "betaDiversity", 
#'              showMultisite = TRUE)
#' # ScatterPlot
#' viewResults2(scenario, 
#'              type = "ScatterPlot", 
#'              parameters = c("simpson", "rao"), 
#'              showReference = TRUE, showSupplementary = TRUE)
#' viewResults2(scenario, 
#'              type = "ScatterPlot", 
#'              parameters = c("simpson", "rao"), 
#'              testsFilter = c("simpson >= 0.8", 
#'                              "rao > 0.2"), 
#'              showSupplementary = FALSE)
#' viewResults2(scenarioOptimiseMultisite, 
#'              type = "ScatterPlot", 
#'              parameters = c("averageFunctions", "betaDiversity"), 
#'              showMultisite = TRUE)
#' # RidgesPlot
#' viewResults2(scenario, 
#'              type = "RidgesPlot", 
#'              parameters = "simpson", 
#'              showReference = TRUE, showSupplementary = TRUE)
#' # ParallelCoordinate - Only selected scenarios
#' viewResults2(scenarioSelectedSingle, 
#'              type = "ParallelCoordinate", 
#'              parameters = c("simpson", "dissimilarity", "rao"), 
#'              highlight = "ResultType", 
#'              showSupplementary = FALSE)
#' # SpeciesRank - Only selected scenarios
#' viewResults2(scenarioSelectedSingle, 
#'              type = "SpeciesRank", 
#'              highlight = "Simulation", 
#'              showSupplementary = FALSE)
#' # UpsetPlot - Multifunctionality results
#' viewResults2(scenario, 
#'              type = "UpsetPlot", 
#'              showReference = FALSE)
#' # ConvexHull - Traits - Only selected scenarios
#' viewResults2(scenarioSelectedSingle, 
#'              type = "ConvexHull", 
#'              parameters = c("BT", "Height"), 
#'              traits = cerrado$traits, 
#'              showSupplementary = TRUE)
#' # DumbbellPlot - Before-after - Only single selection
#' # The supplementary sites represent the "before" state
#' viewResults2(scenarioSelectedSingle, 
#'              type = "DumbbellPlot", 
#'              parameters = "rao", 
#'              showReference = TRUE)
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