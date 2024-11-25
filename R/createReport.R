#' @title Creates a data profiling report.
#' @description Creates a data profiling report with descriptive statistics and univariate graphs.
#' @encoding UTF-8
#' @importFrom ggplot2 ggplot aes geom_bar geom_histogram ggsave
#' @importFrom tableHTML tableHTML add_css_table
#' @importFrom R2HTML HTMLInitFile HTMLCSS HTML.title HTMLli HTMLhr HTML HTMLInsertGraph HTMLbr
#' @importFrom utils browseURL
#' @importFrom grDevices dev.list dev.off nclass.FD
#' @param x A data.frame or matrix with the input data.
#' @param props Numeric vector of probabilities with values in between 0 and 1 to produces sample quantiles corresponding to the given probabilities (default props = NULL).
#' @returns The report in html format.
#' @author See \code{\link{resbiota-package}}.
#' @seealso \code{\link{simulateCommunities}}, \code{\link{computeParameters}}, \code{\link{selectCommunities}},
#' \code{\link{extractResults}}, \code{\link{viewResults}}
#' @keywords MainFunction
#' @export
createReport <- function(x, props = NULL){
  # Get basic parameters
  ncall <- deparse(substitute(x))
  colNamesX <- colnames(x)
  dirTemp <- tempdir()
  ## Turn off graphics device if interrupted in the middle of plotting
  currentDevices <- grDevices::dev.list()
  on.exit( sapply(grDevices::dev.list(), function(dev) if(!(dev %in% currentDevices)) grDevices::dev.off(dev)) )
  # Load css file
  cssFile <- system.file("style.css", package = "resbiota")
  # Start html file
  htmlFile <- R2HTML::HTMLInitFile(outdir = dirTemp, filename = "resbiotaoutput", CSSFile = cssFile, useLaTeX = FALSE, useGrid = FALSE, HTMLframe = FALSE)
  # Include css file
  R2HTML::HTMLCSS(file = htmlFile, append = TRUE, CSSfile = cssFile)
  # Title
  R2HTML::HTML.title(sprintf("Descriptive statistics: %s", ncall), HR = 1, CSSclass = "title", file = htmlFile, append = TRUE)
  # Table of Contents
  R2HTML::HTML.title("Table of Contents", HR = 2, CSSclass = "title", file = htmlFile, append = TRUE)
  R2HTML::HTMLli(txt = sprintf('<a href="#%s">%s</a>', "Summary table", "Summary table"), file = htmlFile, append = TRUE)
  for (varName in colNamesX) {
    R2HTML::HTMLli(txt = sprintf('<a href="#%s">%s</a>', varName, varName), file = htmlFile, append = TRUE)
  }
  # New line
  R2HTML::HTMLhr(file = htmlFile, append = TRUE)
  # Global summary table
  R2HTML::HTML.title(sprintf('<a name="%s">Summary table</a>', "Summary table"), HR = 2, CSSclass = "title", file = htmlFile, append = TRUE)
  dfInfo <- data.frame(Name = c("Number of rows", "Number of columns"), Value = c(nrow(x), ncol(x)))
  cat(tableHTML::add_css_table(tableHTML::tableHTML(dfInfo, rownames = FALSE), css = list('text-align', 'center')), file = htmlFile, append = TRUE)
  # Plots
  for (varName in colNamesX) {
    R2HTML::HTMLhr(file = htmlFile, append = TRUE)
    R2HTML::HTML.title(sprintf('<a name="%s"> %s</a>', varName, paste0(varName, " - ", vectorClass(x[,varName]))), HR = 2, CSSclass = "title", file = htmlFile, append = TRUE)
    pName <- paste(varName, ".png", sep = "")
    if(inherits(x[,varName], "factor") || inherits(x[,varName], "character")){
      p1 <- ggplot2::ggplot(data = x) +
        ggplot2::aes(x = .data[[varName]]) +
        ggplot2::geom_bar(fill = "#1F78B4", col = "#ffffff") +
        themeResbiota(baseSize = 12)
    } else{
      # Freedman-Diaconis method
      p1 <- ggplot2::ggplot(data = x) +
        ggplot2::aes(x = .data[[varName]]) +
        ggplot2::geom_histogram(bins = grDevices::nclass.FD(x[,varName]), fill = "#1F78B4", col = "#ffffff") +
        themeResbiota(baseSize = 12)
    }
    # Export temp plot
    ggplot2::ggsave(file.path(dirTemp, pName),
                    plot = p1,
                    width = 6, height = 5,
                    units = "in",
                    dpi = 300)
    # Include summary table
    dfSummary <- t(resSummary(x[, varName], props = props))
    dfSummary <- as.data.frame(dfSummary)
    cat(tableHTML::add_css_table(tableHTML::tableHTML(dfSummary, rownames = FALSE, round = 3), css = list('text-align', 'center')), file = htmlFile, append = TRUE)
    # Include plot
    R2HTML::HTMLInsertGraph(GraphFileName = pName, Caption = "", GraphBorder = 1,
                    Align = "left", WidthHTML = 600, HeightHTML = 500,
                    file = htmlFile, append = TRUE)
    # New line
    R2HTML::HTMLbr(file = htmlFile, append = TRUE)
  }
  # End html file
  cat("\n<hr size=1>\n<font size=-1>\n\t Generated on: <i>", date(), "</i> - <b>resbiota</b> \n<hr size=1>\n\t</body>\n</html>", sep = "", append = TRUE, file = htmlFile)
  fullPath <- paste("file://", htmlFile, sep = "")
  utils::browseURL(fullPath)
  invisible(fullPath)
}