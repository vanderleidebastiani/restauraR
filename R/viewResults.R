#' @title Visualize results
#' @description Visualize parameters of simulated and selected communities and reference sites.
#' @encoding UTF-8
#' @importFrom ggplot2 ggplot aes geom_point scale_color_manual theme element_text geom_bar
#' @importFrom ComplexUpset upset intersection_size
#' @aliases viewMultifunctionality
#' @param x A object of class "simRest" or "simRestSelect" to visualize results.
#' @param xvar Name of the variable (parameter) in x axis.
#' @param yvar Name of the variable (parameter) in y axis.
#' @param hideref Logical argument (TRUE or FALSE) to specify if hide reference sites.
#' @param ... Arguments passed to upset function.
#' @returns A ggplot plot object.
#' @author See \code{\link{resbiota-package}}.
#' @seealso \code{\link{simulateCommunities}},\code{\link{computeParameters}}, \code{\link{selectCommunities}}
#' \code{\link{extractResults}}
#' @references
#' Coutinho, A. G., Carlucci, M. B., & Cianciaruso, M. V. (2023). A framework to apply trait-based ecological 
#' restoration at large scales. Journal of Applied Ecology, 60, 1562–1571. https://doi.org/10.1111/1365-2664.14439
#' 
#' Coutinho, A. G., Nunes, A., Branquinho, C., Carlucci, M. B., & Cianciaruso, M. V. (2024). Natural regeneration 
#' enhances ecosystem multifunctionality but species addition can increase it during restoration monitoring. Manuscript 
#' in preparation.
#' @keywords MainFunction
#' @export
viewResults <- function(x, xvar, yvar, hideref = FALSE){
  # Check object class
  if(!c(inherits(x, "simRest") || inherits(x, "simRestSelect"))){
    stop("x must be of the simRest or simRestSelect class")
  }
  if(inherits(x, "simRest")){
    resResults <- x$simulation$results
    pal <- c("#b5b5b5","#000000", "#BD0026")
  } else{
    resResults <- x$selection$results
    pal <- c("#45a1d4", "#1d4b61", "#BD0026")
  }
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
  # Get reference results
  ref <- x$reference$results
  if(!hideref && !is.null(ref)){
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
      p <- ggplot2::ggplot() +
        ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]], col = .data[["Legend"]]) +
        ggplot2::geom_point(data = resResults, size = 1.2) +
        ggplot2::scale_color_manual(values = pal) +
        themeResbiota(baseSize = 15)
    } else{
      p <- ggplot2::ggplot() +
        ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]], col = .data[["Legend"]]) +
        ggplot2::geom_point(data = resResults, size = 1.2) +
        themeResbiota(baseSize = 15)
    }
  }
  return(p)
}