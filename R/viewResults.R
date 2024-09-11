#' @title Visualize results
#' @description Visualize parameters of simulated and selected communities and reference sites
#' @details
#' @encoding UTF-8
#' @importFrom ggplot2 ggplot aes geom_point scale_color_manual
#' @importFrom ComplexUpset upset
#' @aliases viewMultifunctionality
#' @param x A object of class "simRest" or "simRestSelect" to visualize results
#' @param xvar Name of the variable (parameter) in x axis.
#' @param yvar Name of the variable (parameter) in y axis.
#' @param hideref Logical argument to specify if hide reference sites
#' @param ... Arguments passed to \code{\link{ComplexUpset::upset}} function.
#' @returns A ggplot plot object.
#' @author 
#' @seealso
#' @references
#' @keywords MainFunction
#' @examples
#' @export
viewResults <- function(x, xvar, yvar, hideref = FALSE){
  if(inherits(x, "simRest")){
    resResults <- x$simulation$results
  } else{
    resResults <- x$selection$results
  }
  # all <- resResults$simulation$results
  all <- resResults
  # sel <- resResults$selection$results
  all$PLOTCOL <- ifelse(all$unavailable == 0, "Available", "Unavailable")
  # sel$PLOTCOL <- ifelse(sel$unavailable == 0, "Available - selected", "Unavailable - selected")
  pal <- c('grey', 'black', '#A6CEE3', '#1F78B4', '#B2DF8A')
  names(pal) <- c("Unavailable", "Available", "Unavailable - selected", "Available - selected", "References")
  if(!hideref){
    ref <- x$reference$results
    ref$PLOTCOL <- "References"
    p <- ggplot2::ggplot() +
      ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]], col = .data[["PLOTCOL"]]) +
      ggplot2::geom_point(data = all) +
      # ggplot2::geom_point(data = sel) +
      ggplot2::geom_point(data = ref) +
      ggplot2::scale_color_manual(values = pal)
  } else{
    p <- ggplot2::ggplot() +
      ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]], col = .data[["PLOTCOL"]]) +
      ggplot2::geom_point(data = all) +
      # ggplot2::geom_point(data = sel) +
      # ggplot2::geom_point(data = ref) +
      ggplot2::scale_color_manual(values = pal)
  }
  return(p)
}
