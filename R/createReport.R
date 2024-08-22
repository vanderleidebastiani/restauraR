#' @title Create report 
#' @description Creates a data profiling report 
#' @details
#' @encoding UTF-8
#' @import ggplot2
#' @import mcmcplots
#' @import patchwork
#' @import ggpubr
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
  
  ncall <- deparse(substitute(x))
  
  traitsNames <- colnames(x)
  dir <- tempdir()
  
  ## Turn off graphics device if interrupted in the middle of plotting
  current.devices <- dev.list()
  on.exit( sapply(dev.list(), function(dev) if(!(dev %in% current.devices)) dev.off(dev)) )
  
  
  css.file <- system.file("MCMCoutput.css", package = "mcmcplots")
  css.file <- paste("file:///", css.file, sep = "")
  # html file
  htmlfile <- mcmcplots:::.html.begin(dir, filename = "CCCoutput", extension = "html", title = "Descriptive statistics", cssfile = css.file)
  
  cat('\n<div id="outer">\n', file = htmlfile, append = TRUE)
  # Title
  # cat('<h1>Descriptive statistics</h1>', sep = "", file = htmlfile, append = TRUE)
  cat(sprintf('<h1>Descriptive statistics: %s</h1>', ncall), sep = "", file = htmlfile, append = TRUE)
  # cat(sprintf('<h2>Data set: %s</h2>', ncall), sep = "", file = htmlfile, append = TRUE)
  cat('<div id="toc">\n', file = htmlfile, append = TRUE)
  # Table of Contents
  cat('\n<h2>Table of Contents</h2>', file = htmlfile, append = TRUE)
  cat('<ul id="toc_items">\n', file = htmlfile, append = TRUE)
  cat(sprintf('<li class="toc_item"><a href="#%s">%s</a></li>\n', "Summary table", "Summary table"), file = htmlfile, append = TRUE)
  
  # cat('<ul id="toc_items">\n', file = htmlfile, append = TRUE)
  # cat(sprintf('<li class="toc_item"><a href="#%s">%s</a></li>\n', "Summary table", "Summary table"), file = htmlfile, append = TRUE)
  # cat('</ul></div>\n', file = htmlfile, append = TRUE)
  
  for (group.name in traitsNames) {
    cat(sprintf('<li class="toc_item"><a href="#%s">%s</a></li>\n', group.name, group.name), file = htmlfile, append = TRUE)
  }
  cat('</ul></div>\n', file = htmlfile, append = TRUE)
  cat('<div class="main">\n', file = htmlfile, append = TRUE)
  
  cat(sprintf('<h2><a name="%s">Summary table</a></h2>\n', "Summary table"), file=htmlfile, append=TRUE)
  cat("\r", rep(" ", getOption("width")), sep="")
  
  
  # Global summary table
  # cat(sprintf('
  # <table>
  #  <thead>
  #    <tr>
  #     <th style="text-align: center" >Species pool</th>
  #     <th style="text-align: center" >Number of traits</th>
  #    <tr>
  #  </thead>
  #  <tbody>
  #   <tr>
  #    <td style="text-align: center">%s</td>
  #    <td style="text-align: center">%s</td>
  #   </tr>
  #  </tbody>
  # </table>\n\n', nrow(x), ncol(x)), file = htmlfile, append = TRUE)
  
  cat(sprintf('
  <table>
  <thead>
   <tr class="header">
    <th align="left">Name</th>
    <th align="left">Value</th>
   </tr>
  </thead>
  <tbody>
   <tr>
    <td align="left">Species pool</td>
    <td align="center">%s</td>
   </tr>
   <tr>
    <td align="left">Number of traits</td>
    <td align="center">%s</td>
   </tr>
  </tbody>
  </table>\n\n', nrow(x), ncol(x)), file = htmlfile, append = TRUE)
  # Plots
  for (group.name in traitsNames) {
    cat(sprintf('<h2><a name="%s">Plots for %s</a></h2>\n', group.name, group.name), file=htmlfile, append=TRUE)
    cat("\r", rep(" ", getOption("width")), sep="")
    
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
    
    
    df <- resSummary(x[,group.name], props = props)
    df <- round(df, 3)
    p2 <- ggpubr::ggtexttable(df, rows = NULL)
    # p2 <- tab_add_title(p2, text = group.name, face = "bold")
    ggplot2::ggsave(file.path(dir, gname), 
                    plot = p2/p1 + patchwork::plot_layout(heights = c(0.2, 0.8)), 
                    width = 6, height = 6,
                    units = "in",
                    dpi = 300)
    
    
    # if (inherits(plot_err, "error")) {
    #   cat(sprintf('<p class="plot_err">%s. %s</p>', p, plot_err),
    #       file=htmlfile, append=TRUE)
    # } else {
    mcmcplots:::.html.img(file=htmlfile, class="mcmcplot", src = gname,
                          width = 640, height = 640)
    # }
  }
  cat("\r", rep(" ", getOption("width")), "\r", sep="")
  cat('\n</div>\n</div>\n', file=htmlfile, append=TRUE)
  mcmcplots:::.html.end(htmlfile)
  full.name.path <- paste("file://", htmlfile, sep="")
  browseURL(full.name.path)
  invisible(full.name.path)
}