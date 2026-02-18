#' @title Visualise simulation results
#' @description Generate graphical representations of simulated and selected communities, including histograms for parameter distributions, scatter plots for trade-off analyses, and upset plots for multifunctionality results. 
#' Reference sites are highlighted as distinct points in scatter plots or as rug lines along the axes in histograms.
#' @encoding UTF-8
#' @importFrom ggplot2 ggplot aes geom_point scale_color_manual theme element_text geom_bar geom_histogram
#' @importFrom ComplexUpset upset intersection_size
#' @importFrom grDevices nclass.FD
#' @aliases viewMultifunctionality
#' @param x An object of class "simRest" or "simRestSelect" to visualise results.
#' @param xvar Character vector specifying the variable name for the histogram or the x-axis in scatter plots.
#' @param yvar Character vector specifying the variable name for the y-axis in scatter plots (default yvar = NULL).
#' @param showReference Logical indicating whether to include reference sites from visualisation (default showReference = TRUE).
#' @param showMultisite Logical indicating whether to plot results from the multisite analysis; otherwise, the plot shows the basic results (default showMultisite = FALSE).
#' @param ... Arguments passed to the upset function of the package ComplexUpset.
#' @returns A ggplot plot object that can be further customised using standard ggplot2 syntax.
#' @author See \code{\link{resbiota-package}}.
#' @seealso \code{\link{simulateCommunities}}, \code{\link{computeParameters}}, \code{\link{selectCommunities}}, \code{\link{extractResults}}
#' @references
#' Coutinho, A. G., Carlucci, M. B., & Cianciaruso, M. V. (2023). A framework to apply trait-based ecological 
#' restoration at large scales. Journal of Applied Ecology, 60, 1562–1571. https://doi.org/10.1111/1365-2664.14439
#' 
#' Coutinho, A. G., Nunes, A., Branquinho, C., Carlucci, M. B., & Cianciaruso, M. V. (2024). Natural regeneration 
#' enhances ecosystem multifunctionality but species addition can increase it during restoration monitoring. Manuscript 
#' in preparation.
#' @keywords MainFunction
#' @examples
#' data("cerrado.mini")
#' head(cerrado.mini$traits)
#' # Simulation
#' scenario <- simulateCommunities(traits = cerrado.mini$traits,
#'                                 ava = "Available",
#'                                 maxDiver = c("SLA", "Height", "Seed"),
#'                                 constCWM = "BT",
#'                                 rich = c(10, 15),
#'                                 it = 100)
#' scenario
#' # Compute functional parameters
#' scenario <- computeParameters(x = scenario,
#'                               traits = cerrado.mini$traits,
#'                               ava = "Available",
#'                               cwm = "BT",
#'                               rao = c("SLA", "Height", "Seed"),
#'                               cost = "Cost",
#'                               dens = "Density",
#'                               dissimilarity = c("SLA", "Height", "Seed"),
#'                               reference = cerrado.mini$reference,
#'                               supplementary = cerrado.mini$supplementary)
#' scenario
#' # Standardise parameters
#' scenario <- standardiseParameters(x = scenario,
#'                                   parameters = "dissimilarity",
#'                                   method = "max")
#' scenario
#' # Compute multifunctionality
#' scenario <- computeMultifunctionality(x = scenario,
#'                                       tests = c("CWM_BT > 5.9",
#'                                                 "rao > 0.2"))
#' scenario
#' # View results - histogram
#' viewResults(scenario, xvar = "CWM_BT")
#' # View results - scatter plot
#' viewResults(scenario, xvar = "CWM_BT", yvar = "rao")
#' # View multifunctionality - upset plot
#' viewMultifunctionality(scenario)
#' @export
viewResults <- function(x, xvar, yvar = NULL, showReference = TRUE, showMultisite = FALSE){
  # Check object class
  if(!c(inherits(x, "simRest") || inherits(x, "simRestSelect"))){
    stop("The x argument must be of class simRest or simRestSelect")
  }
  if(!showMultisite){
    if(inherits(x, "simRest")){
      resResults <- x$simulation$results
      pal <- c("#b5b5b5","#000000", "#BD0026")
    } else{
      resResults <- x$selection$results
      pal <- c("#45a1d4", "#1d4b61", "#BD0026")
    }
    if(is.null(resResults)){
      stop("The x argument must contain the parameters calculated")
    }
    # Set legend to unavailable and available species
    if(!is.null(resResults$unavailable)){
      # Set names in the palette
      names(pal) <- c("Unavailable", "Available", "References")
      resResults$Legend <- ifelse(resResults$unavailable == 0, "Available", "Unavailable")
      resResults$Legend <- factor(resResults$Legend, levels = c("Available", "Unavailable", "References"))
    } else{
      # Set names in the palette
      pal <- pal[2:3]
      names(pal) <- c("Simulation", "References")
      resResults$Legend <- "Simulation"
      resResults$Legend <- factor(resResults$Legend, levels = c("Simulation", "References"))
    }
  } else{
    if(inherits(x, "simRest")){
      resResults <- x$simulation$multisite$results
      pal <- c("#000000")
    } else{
      resResults <- x$selection$multisite$results
      pal <- c("#1d4b61")
    }
    if(is.null(resResults)){
      stop("The x argument must contain the multisite results")
    }
    # Force hide reference 
    showReference <- FALSE
  }
  # Get reference results
  ref <- x$reference$results 
  # Check names
  parameters <- c(xvar, yvar)
  varNames <- colnames(resResults)
  if(!inherits(parameters, "character") || !all(parameters %in% varNames)){
    stop("The xvar and yvar arguments must be a character vector specifying column names from the results")
  }
  # Force hide reference if column names not found in reference results
  if(showReference){
    varNamesRef <- colnames(ref)
    if(!all(parameters %in% varNamesRef)){
      # Force hide reference 
      showReference <- FALSE
    }  
  }
  # If yvar null plot bar chart or histogram
  if(is.null(yvar)){
    if(showReference && !is.null(ref)){
      if(inherits(resResults[,xvar], "factor") || inherits(resResults[,xvar], "character")){
        p <- ggplot2::ggplot() +
          ggplot2::aes(x = .data[[xvar]]) +
          ggplot2::geom_bar(data = resResults, fill = "#1d4b61", col = "#ffffff") +
          ggplot2::geom_rug(data = ref, col = "#BD0026")+
          themeResbiota(baseSize = 15)
      } else{
        # Freedman-Diaconis method
        p <- ggplot2::ggplot() +
          ggplot2::aes(x = .data[[xvar]]) +
          ggplot2::geom_histogram(data = resResults, bins = grDevices::nclass.FD(resResults[,xvar]), fill = "#1d4b61", col = "#ffffff") +
          ggplot2::geom_rug(data = ref, col = "#BD0026")+
          themeResbiota(baseSize = 15)
      }
    } else{
      if(inherits(resResults[,xvar], "factor") || inherits(resResults[,xvar], "character")){
        p <- ggplot2::ggplot() +
          ggplot2::aes(x = .data[[xvar]]) +
          ggplot2::geom_bar(data = resResults, fill = "#1d4b61", col = "#ffffff") +
          themeResbiota(baseSize = 15)
      } else{
        # Freedman-Diaconis method
        p <- ggplot2::ggplot() +
          ggplot2::aes(x = .data[[xvar]]) +
          ggplot2::geom_histogram(data = resResults, bins = grDevices::nclass.FD(resResults[,xvar]), fill = "#1d4b61", col = "#ffffff") +
          themeResbiota(baseSize = 15)
      }
    }
  } else {
    # Else plot scatter plot
    if(showReference && !is.null(ref)){
      ref$Legend <- "References"
      ref$Legend <- factor(ref$Legend, levels = levels(resResults$Legend))
      p <- ggplot2::ggplot() +
        ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]], col = .data[["Legend"]]) +
        ggplot2::geom_point(data = resResults, size = 1.2) +
        ggplot2::geom_point(data = ref, size = 1.7) +
        ggplot2::scale_color_manual(values = pal) +
        themeResbiota(baseSize = 15)
    } else{
      if(nrow(resResults)>0){
        if(!showMultisite){
          p <- ggplot2::ggplot() +
            ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]], col = .data[["Legend"]]) +
            ggplot2::geom_point(data = resResults, size = 1.2) +
            ggplot2::scale_color_manual(values = pal) +
            themeResbiota(baseSize = 15)
        } else{
          # Else multisite results
          p <- ggplot2::ggplot() +
            ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]]) +
            ggplot2::geom_point(data = resResults, size = 1.2, col = pal) +
            themeResbiota(baseSize = 15) 
        }
      } else{
        p <- ggplot2::ggplot() +
          ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]], col = .data[["Legend"]]) +
          ggplot2::geom_point(data = resResults, size = 1.2) +
          themeResbiota(baseSize = 15)
      }
    }
  }
  return(p)
}