#' @title Internal function. A custom palette to use in plots
#' @encoding UTF-8
#' @returns A character vector with default palette.
#' @author See \code{\link{restauraR-package}}.
#' @seealso \code{\link{simulateCommunities}}
#' @keywords InternalFunction
paletteRestauraR <- function(){
  # Colour blind safe
  pal <- c("#56B4E9", "#0072B2", "#E0E0E0","#4D4D4D", "#CC79A7", "#D55E00", "#ffffff")
  names(pal) <- c("LightBlue", "DarkBlue", "LightGray", "DarkGray", "Magenta", "Orange","White")
  return(pal)
}