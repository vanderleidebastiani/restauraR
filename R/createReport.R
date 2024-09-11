#' @title Create report 
#' @description Creates a data profiling report Based on the mcmcplots package
#' @details
#' @encoding UTF-8
#' @importFrom ggplot2 ggplot aes geom_bar geom_histogram ggsave
#' @importFrom tableHTML tableHTML add_css_table
#' @importFrom R2HTML HTMLInitFile HTMLCSS HTML.title HTMLli HTMLhr HTML HTMLInsertGraph HTMLbr
#' @aliases
#' @param x Input data
#' @param props Numeric vector of probabilities with values in between 0 and 1 to produces sample quantiles corresponding to the given probabilities (default props = NULL).
#' @returns
#' @note 
#' @author 
#' @seealso
#' @references
#' @keywords MainFunction
#' @examples
#' @export
createReport <- function(x, props = NULL){
  # Get basic parameters
  ncall <- deparse(substitute(x))
  traitsNames <- colnames(x)
  dirTemp <- tempdir()
  ## Turn off graphics device if interrupted in the middle of plotting
  current.devices <- dev.list()
  on.exit( sapply(dev.list(), function(dev) if(!(dev %in% current.devices)) dev.off(dev)) )
  # Load css file in mcmcplots package
  css.file <- system.file("style.css", package = "CCC")
  # css.file <- paste0(getwd(), "/inst/style.css")
  # Start html file
  htmlfile <- R2HTML::HTMLInitFile(outdir = dirTemp, filename = "CCCoutput", CSSFile = css.file, useLaTeX = FALSE, useGrid = FALSE, HTMLframe = FALSE)
  # Include css file
  R2HTML::HTMLCSS(file = htmlfile, append = TRUE, CSSfile = css.file)
  # Title
  # cat(sprintf('<h1>Descriptive statistics: %s</h1>', ncall), sep = "", file = htmlfile, append = TRUE)
  R2HTML::HTML.title(sprintf("Descriptive statistics: %s", ncall), HR = 1, CSSclass = "title", file = htmlfile, append = TRUE)
  # Table of Contents
  # cat('\n<h2>Table of Contents</h2>', file = htmlfile, append = TRUE)
  R2HTML::HTML.title("Table of Contents", HR = 2, CSSclass = "title", file = htmlfile, append = TRUE)
  # cat(sprintf('<li class="toc_item"><a href="#%s">%s</a></li>\n', "Summary table", "Summary table"), file = htmlfile, append = TRUE)
  R2HTML::HTMLli(txt = sprintf('<a href="#%s">%s</a>', "Summary table", "Summary table"), file = htmlfile, append = TRUE)
  for (group.name in traitsNames) {
    # cat(sprintf('<li class="toc_item"><a href="#%s">%s</a></li>\n', group.name, group.name), file = htmlfile, append = TRUE)
    R2HTML::HTMLli(txt = sprintf('<a href="#%s">%s</a>', group.name, group.name), file = htmlfile, append = TRUE)
  }
  R2HTML::HTMLhr(file = htmlfile, append = TRUE)
  # Global summary table
  # cat(sprintf('<h2><a name="%s">Summary table</a></h2>\n', "Summary table"), file = htmlfile, append = TRUE)
  R2HTML::HTML.title(sprintf('<a name="%s">Summary table</a>', "Summary table"), HR = 2, CSSclass = "title", file = htmlfile, append = TRUE)
  dfInfo <- data.frame(Name = c("Species pool", "Number of traits"), Value = c(nrow(x), ncol(x)))
  cat(tableHTML::add_css_table(tableHTML::tableHTML(dfInfo, rownames = FALSE), css = list('text-align', 'center')), file = htmlfile, append = TRUE)
  # R2HTML::HTML(dfInfo, row.names = FALSE, file = htmlfile, append = TRUE)
  # R2HTML::HTML(dfInfo, row.names = FALSE, file = htmlfile, append = TRUE, classtable = "table", Border = -1, innerBorder = 1)
  # Plots
  for (group.name in traitsNames) {
    R2HTML::HTMLhr(file = htmlfile, append = TRUE)
    # cat(sprintf('<h2><a name="%s">Plots for %s</a></h2>\n', group.name, group.name), file = htmlfile, append = TRUE)
    R2HTML::HTML.title(sprintf('<a name="%s">Plots for %s</a>', group.name, group.name), HR = 2, CSSclass = "title", file = htmlfile, append = TRUE)
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
    ggplot2::ggsave(file.path(dirTemp, gname),
                    plot = p1,
                    width = 6, height = 5,
                    units = "in",
                    # width = 500, height = 500,
                    # units = "px",
                    dpi = 300)
    # Include summary table
    df <- t(resSummary(x[,group.name], props = props))
    df <- as.data.frame(df)
    cat(tableHTML::add_css_table(tableHTML::tableHTML(df, rownames = FALSE, round = 3), css = list('text-align', 'center')), file = htmlfile, append = TRUE)
    # R2HTML::HTML(df, row.names = FALSE, digits = 3, file = htmlfile, append = TRUE)
    # Include plot
    R2HTML::HTMLInsertGraph(GraphFileName = gname, Caption="", GraphBorder = 0,
                    Align = "left", WidthHTML = 600, HeightHTML = 500,
                    file = htmlfile, append = TRUE)
    # New line
    R2HTML::HTMLbr(file = htmlfile, append = TRUE)
  }
  # End html file
  cat("\n<hr size=1>\n<font size=-1>\n\t Generated on: <i>", date(), "</i> - <b>CCC</b> \n<hr size=1>\n\t</body>\n</html>", sep = "", append = TRUE, file = htmlfile)
  full.name.path <- paste("file://", htmlfile, sep = "")
  # TEMP REMOVe
  print(css.file)
  print(full.name.path)
  browseURL(full.name.path)
  invisible(full.name.path)
}