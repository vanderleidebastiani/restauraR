#' @title function to visualize results
#' @description Visualize parameters of simulated and selected communities and reference sites
#' @details
#' @encoding UTF-8
#' @importFrom ggplot2 ggplot aes geom_point scale_color_manual
#' @aliases
#' @param x 
#' @return 
#' @note 
#' @author 
#' @seealso
#' @references
#' @keywords
#' @examples
#' @export
viewResults <- function(x, xvar, yvar){
  if(inherits(x, "simRest")){
    resResults <- x$simulation$results
  } else{
    resResults <- x$selection$results
  }
  # all <- resResults$simulation$results
  all <- resResults
  ref <- x$reference$results
  # sel <- resResults$selection$results
  all$PLOTCOL <- ifelse(all$unavailable == 0, "Available", "Unavailable")
  # sel$PLOTCOL <- ifelse(sel$unavailable == 0, "Available - selected", "Unavailable - selected")
  ref$PLOTCOL <- "References"
  pal <- c('grey', 'black', '#A6CEE3', '#1F78B4', '#B2DF8A')
  names(pal) <- c("Unavailable", "Available", "Unavailable - selected", "Available - selected", "References")
  p <- ggplot2::ggplot() +
    ggplot2::aes(x = .data[[xvar]], y = .data[[yvar]], col = .data[["PLOTCOL"]]) +
    ggplot2::geom_point(data = all) +
    # ggplot2::geom_point(data = sel) +
    ggplot2::geom_point(data = ref) +
    ggplot2::scale_color_manual(values = pal)
  return(p)
}
