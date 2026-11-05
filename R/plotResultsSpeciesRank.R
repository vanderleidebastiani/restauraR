#' @rdname viewResults2
#' @include viewResults2.R
#' @encoding UTF-8
#' @keywords MainFunction
#' @export
plotResultsSpeciesRank <- function(x, highlight = c("ResultType", "Site", "Simulation"), showReference = TRUE, showSupplementary = FALSE){
  # Check object class
  if(!inherits(x, "simRestSelect")){
    stop("The x argument must be of class simRestSelect")
  }
  HIGHLIGHT <- c("ResultType", "Site", "Simulation")
  highlightTest <- pmatch(highlight, HIGHLIGHT)
  if (length(highlightTest) > 1) {
    stop("Only one option can be specified in highlight argument")
  }
  if (is.na(highlightTest)) {
    stop("Invalid highlight argument. Choose either ResultType, Site ou Simulation")
  }
  dataPlot <- extractResults(x, type = "simComposition", dbFormat = TRUE)
  dataPlot$Rank <- stats::ave(-dataPlot$Abundance, dataPlot$Site, FUN = rank)
  dataPlot$ResultType <- "Simulation"
  simGroup <- x$selection$group
  groupNames <- colnames(simGroup)
  if(!"Site" %in% groupNames){
    simGroup$Site <- NA
  }
  # Change names, Site to Simulation
  dataPlot$Simulation <- as.character(dataPlot$Site)
  dataPlot$Site <- NULL
  # Merge
  dataPlot <- merge(x = dataPlot, y = simGroup, by = "Simulation", all.x = TRUE)
  dataPlot <- dataPlot[, c("ResultType", "Simulation", "Site", "Species", "Abundance", "Rank")]
  # "ResultType", "Simulation", "Site"
  if(showReference && !is.null(x$reference)){
    resRef <- extractResults(x, type = "refComposition", dbFormat = TRUE)
    resRef$Rank <- stats::ave(-resRef$Abundance, resRef$Site, FUN = rank)
    resRef$ResultType <- "Reference"
    resRef$Simulation <- NA
    resRef <- resRef[, c("ResultType", "Simulation", "Site", "Species", "Abundance", "Rank")]
    dataPlot <- rbind.data.frame(dataPlot, resRef)
  }
  if(showSupplementary && !is.null(x$supplementary)){
    resSup <- extractResults(x, type = "supComposition", dbFormat = TRUE)
    resSup$Rank <- stats::ave(-resSup$Abundance, resSup$Site, FUN = rank)
    resSup$ResultType <- "Supplementary"
    resSup$Simulation <- NA
    resSup <- resSup[, c("ResultType", "Simulation", "Site", "Species", "Abundance", "Rank")]
    dataPlot <- rbind.data.frame(dataPlot, resSup)
  }
  # Define unique names for drawing lines
  dataPlot$lineGroup <- paste0(dataPlot$ResultType, dataPlot$Simulation, dataPlot$Site)
  if(highlight == "ResultType"){
    pal <- c("#1d4b61", "#BD0026", "#C37614")
    names(pal) <- c("Simulation", "Reference", "Supplementary")
    # Reorder levels
    dataPlot$ResultType <- factor(dataPlot$ResultType, levels = c("Simulation", "Reference", "Supplementary"))
  }
  if(highlight == "Site"){
    simNames <- unique(dataPlot$Site[dataPlot$ResultType == "Simulation"])
    refNames <- unique(dataPlot$Site[dataPlot$ResultType == "Reference"])
    supNames <- unique(dataPlot$Site[dataPlot$ResultType == "Supplementary"])
    if(any(supNames %in% simNames) && showSupplementary){
      stop("The supplementary sites have the same names as the Simulation sites. Highlighting individual sites is not permitted")
    }
    # Set colours
    blues <- if(length(simNames) == 0) character(0) else if (length(simNames) == 1) "#1d4b61" else scales::pal_hue(h = c(195, 217), c = 100, l = 10)(length(simNames))
    reds <- if(length(refNames) == 0) character(0) else if (length(refNames) == 1) "#BD0026" else scales::pal_hue(h = c(218, 255), c = 100, l = 10)(length(refNames))
    yellows <- if(length(supNames) == 0) character(0) else if (length(supNames) == 1) "#C37614" else scales::pal_hue(h = c(45, 75), c = 100, l = 60)(length(supNames))
    pal <- c(blues, reds, yellows)
    names(pal) <- c(simNames, refNames, supNames)
    # Reorder levels
    dataPlot$Site<- factor(dataPlot$Site, levels = c(simNames, refNames, supNames))
  }
  if(highlight == "Simulation"){
    simNames <- unique(dataPlot$Simulation)
    simNames <- simNames[!is.na(simNames)]
    blues <- if(length(simNames) == 0) character(0) else if (length(simNames) == 1) "#1d4b61" else scales::pal_hue(h = c(195, 217), c = 100, l = 10)(length(simNames))
    pal <- c("#BD0026", "#C37614", blues)
    names(pal) <- c("Reference", "Supplementary", simNames)
    dataPlot$Simulation <- ifelse(is.na(dataPlot$Simulation), dataPlot$ResultType, dataPlot$Simulation)
    dataPlot$Simulation <- factor(dataPlot$Simulation, levels = c(simNames, "Reference", "Supplementary"))
  }
  p <- ggplot2::ggplot() +
    ggplot2::aes(x = .data[["Rank"]], y = .data[["Abundance"]], col = .data[[highlight]]) +
    suppressWarnings(ggplot2::geom_point(data = dataPlot, size = 1.2, mapping = ggplot2::aes(customdata = .data[["Species"]]))) +
    suppressWarnings(ggplot2::geom_line(data = dataPlot, mapping = ggplot2::aes(group = .data[["lineGroup"]]), linewidth	= 0.4)) +
    ggplot2::scale_color_manual(values = pal) +
    ggplot2::labs(colour = "Legend") +
    themeRestauraR(baseSize = 15)
  return(p)
}