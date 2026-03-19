#' @title Internal function. A custom theme to use in ggplot2 objects
#' @encoding UTF-8
#' @importFrom ggplot2 theme element_text element_line element_blank element_rect
#' @param baseSize Base font size for text elements (default baseSize = 10).
#' @param legendPosition Position of the legends (default legendPosition = "left").
#' @returns A ggplot theme object.
#' @author See \code{\link{restauraR-package}}.
#' @seealso \code{\link{simulateCommunities}}
#' @keywords InternalFunction
themeRestauraR <- function(baseSize = 10, legendPosition = "right"){
  ggplot2::theme(
    # Titles
    plot.tag = ggplot2::element_text(size = baseSize),
    plot.title = ggplot2::element_text(size = baseSize, face = "bold"),
    plot.subtitle = ggplot2::element_text(size = baseSize*0.9),
    plot.caption = ggplot2::element_text(size = baseSize*0.9),
    # Axis
    axis.title = ggplot2::element_text(size = baseSize*0.9, face = "bold"),
    axis.text.y = ggplot2::element_text(size = baseSize*0.8, hjust = 1),
    axis.text.x = ggplot2::element_text(size = baseSize*0.8, angle = 0, vjust = 1),
    axis.line = ggplot2::element_line(linewidth = 0.5),
    # Legend
    legend.key = ggplot2::element_blank(),
    legend.position = legendPosition,
    legend.justification = "center",
    legend.title = ggplot2::element_text(size = baseSize*0.8, face = "bold"),
    legend.text = ggplot2::element_text(size = baseSize*0.7),
    # Background
    plot.background = ggplot2::element_rect(fill = "#ffffff", color = NA),
    panel.background = ggplot2::element_rect(fill  = "#ffffff", color = NA),
    legend.background = ggplot2::element_rect(fill = "#ffffff", color = NA),
    strip.background = ggplot2::element_rect(fill = "#dbdbdb", color = NA)
  )
}
