#' @title Create report 
#' @description Creates a data profiling report 
#' @details
#' @encoding UTF-8
#' @importFrom ggplot2 ggplot aes geom_bar geom_histogram ggsave
#' @importFrom tableHTML tableHTML add_css_table
#' @import mcmcplots
#' @aliases
#' @param x Input data
#' @param props Numeric vector of probabilities with values in between 0 and 1 to produces sample quantiles corresponding to the given probabilities (default props = NULL).
#' @returns
#' @note 
#' @author 
#' @seealso
#' @references
#' @keywords
#' @examples
#' @export
createReport <- function(x, props = NULL){
  # Based on the mcmcplots package
  # Get basic parameters
  ncall <- deparse(substitute(x))
  traitsNames <- colnames(x)
  dir <- tempdir()
  ## Turn off graphics device if interrupted in the middle of plotting
  current.devices <- dev.list()
  on.exit( sapply(dev.list(), function(dev) if(!(dev %in% current.devices)) dev.off(dev)) )
  # Load css file in mcmcplots package
  css.file <- system.file("MCMCoutput.css", package = "mcmcplots")
  css.file <- paste("file:///", css.file, sep = "")
  # Start html file
  htmlfile <- mcmcplots:::.html.begin(dir, filename = "CCCoutput", extension = "html", title = "Descriptive statistics", cssfile = css.file)
  cat('\n<div id="outer">\n', file = htmlfile, append = TRUE)
  # Title
  cat(sprintf('<h1>Descriptive statistics: %s</h1>', ncall), sep = "", file = htmlfile, append = TRUE)
  cat('<div id="toc">\n', file = htmlfile, append = TRUE)
  # Table of Contents
  cat('\n<h2>Table of Contents</h2>', file = htmlfile, append = TRUE)
  cat('<ul id="toc_items">\n', file = htmlfile, append = TRUE)
  cat(sprintf('<li class="toc_item"><a href="#%s">%s</a></li>\n', "Summary table", "Summary table"), file = htmlfile, append = TRUE)
  for (group.name in traitsNames) {
    cat(sprintf('<li class="toc_item"><a href="#%s">%s</a></li>\n', group.name, group.name), file = htmlfile, append = TRUE)
  }
  cat('</ul></div>\n', file = htmlfile, append = TRUE)
  cat('<div class="main">\n', file = htmlfile, append = TRUE)
  # Global summary table
  cat(sprintf('<h2><a name="%s">Summary table</a></h2>\n', "Summary table"), file = htmlfile, append = TRUE)
  dfInfo <- data.frame(Name = c("Species pool", "Number of traits"),Value = c(nrow(x), ncol(x)))
  cat(tableHTML::add_css_table(tableHTML::tableHTML(dfInfo, rownames = FALSE), css = list('text-align', 'center')), file = htmlfile, append = TRUE)
  # Plots
  for (group.name in traitsNames) {
    cat(sprintf('<h2><a name="%s">Plots for %s</a></h2>\n', group.name, group.name), file = htmlfile, append = TRUE)
    gname <- paste(group.name, ".png", sep="")
    if(inherits(x[[group.name]], "factor") || inherits(x[[group.name]], "character")){
      p1 <- ggplot2::ggplot(data = x) +
        ggplot2::aes(x = .data[[group.name]]) +
        ggplot2::geom_bar()
    } else{
      # Freedman-Diaconis method
      p1 <- ggplot2::ggplot(data = x) +
        ggplot2::aes(x = .data[[group.name]]) +
        ggplot2::geom_histogram(bins = nclass.FD(x[[group.name]]), col = "white")
    }
    # Export temp plot
    ggplot2::ggsave(file.path(dir, gname), 
                    plot = p1,
                    width = 6, height = 5,
                    units = "in",
                    dpi = 300)
    # Include summary table
    df <- t(resSummary(x[,group.name], props = props))
    cat(tableHTML::add_css_table(tableHTML::tableHTML(df, rownames = FALSE, round = 3), css = list('text-align', 'center')), file = htmlfile, append = TRUE)
    # New line
    cat('\n<br>\n', file = htmlfile, append = TRUE)
    # Include plot
    mcmcplots:::.html.img(file = htmlfile, class = "mcmcplot", src = gname, width = 640, height = 530)
    # New line
    cat('\n<br>\n', file = htmlfile, append = TRUE)
  }
  cat('\n</div>\n</div>\n', file = htmlfile, append = TRUE)
  # End html file
  mcmcplots:::.html.end(htmlfile)
  full.name.path <- paste("file://", htmlfile, sep = "")
  browseURL(full.name.path)
  invisible(full.name.path)
}