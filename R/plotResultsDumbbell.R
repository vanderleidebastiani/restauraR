#' @rdname viewResults2
#' @include viewResults2.R
#' @encoding UTF-8
#' @keywords MainFunction
#' @export
plotResultsDumbbell <- function(x, parameters, showReference = TRUE){
  # Check object class
  if(!inherits(x, "simRestSelect")){
    stop("The x argument must be of class simRestSelect")
  }
  # Check names
  varNames <- colnames(x$selection$results)
  if(!inherits(parameters, "character") || !all(parameters %in% varNames)){
    stop("The parameters argument must be a character vector specifying column names from the results")
  }
  if (length(parameters) > 1) {
    stop("Only one parameter can be specified")
  }
  if(is.null(x$supplementary)){
    stop("The results must include supplementary sites, with the sites as they were prior to restoration")
  }
  # Extract parameters data
  resu <- extractParameters(x, parameters, showSimulation = TRUE, showSupplementary = FALSE, showReference = FALSE)
  supp <- extractParameters(x, parameters, showSimulation = FALSE, showSupplementary = TRUE, showReference = FALSE)
  if(showReference){
    ref <- extractParameters(x, parameters, showSimulation = FALSE, showSupplementary = FALSE, showReference = TRUE)  
  }
  # Maximum one simulation per site
  if(max(table(resu$Site))>1){
    stop("The selection results must contain only a single simulation selected per site")
  }
  if(!all(resu$Site %in% supp$Site) || !all(supp$Site %in% resu$Site)){
    stop("The supplementary sites names must match with selection sites names")
  }
  resu <- data.table::data.table(resu)
  supp <- data.table::data.table(supp)
  # Reshape data
  supp <- data.table::melt(supp, id.vars = 1:3, measure.vars = parameters, value.name = "value0")
  resu <- data.table::melt(resu, id.vars = 1:3, measure.vars = parameters, value.name = "value1")
  dataPlot <- resu[supp, on = c("Site", "variable")]
  dataPlot <- as.data.frame(dataPlot)
  # Re-extract parameters data
  dataPlot0 <- extractParameters(x, parameters, showSimulation = TRUE, showSupplementary = TRUE, showReference = FALSE)
  dataPlot0$ResultType <- ifelse(dataPlot0$ResultType == "Supplementary", "Before", "After")
  dataPlot0$ResultType <- factor(dataPlot0$ResultType, levels = c("Before", "After"))
  pal <- c("#C37614", "#1d4b61", "#BD0026")
  names(pal) <- c("Before", "After", "Reference")
  if(showReference){
    p <- ggplot2::ggplot()+
      ggplot2::aes(xmin = .data[["value0"]], xmax = .data[["value1"]], y = .data[["Site"]]) +
      ggplot2::geom_linerange(data = dataPlot, linewidth = 1.5, col = "grey75") +
      suppressWarnings(ggplot2::geom_point(data = dataPlot0, mapping = ggplot2::aes(x = .data[[parameters]], y = .data[["Site"]], col = .data[["ResultType"]], customdata = .data[["Simulation"]]), size = 6, inherit.aes = FALSE)) +
      ggplot2::geom_rug(data = ref, mapping = ggplot2::aes(x = .data[[parameters]], col = "Reference"), linewidth = 1, inherit.aes = FALSE) +
      ggplot2::scale_colour_manual(values = pal) +
      ggplot2::labs(x = parameters, colour = "Legend") +
      themeRestauraR(baseSize = 15)
  } else{
    p <- ggplot2::ggplot()+
      ggplot2::aes(xmin = .data[["value0"]], xmax = .data[["value1"]], y = .data[["Site"]]) +
      ggplot2::geom_linerange(data = dataPlot, linewidth = 1.5, col = "grey75") +
      suppressWarnings(ggplot2::geom_point(data = dataPlot0, mapping = ggplot2::aes(x = .data[[parameters]], y = .data[["Site"]], col = .data[["ResultType"]], customdata = .data[["Simulation"]]), size = 6, inherit.aes = FALSE)) +
      ggplot2::scale_colour_manual(values = pal) +
      ggplot2::labs(x = parameters, colour = "Legend") +
      themeRestauraR(baseSize = 15)
  }
  # Multivar?
  # ggplot()+
  #   ggplot2::aes(xmin = value0, xmax = value1, y = interaction(Site, variable)) +
  #   geom_linerange(data = dataPlot, linewidth = 1.5, position = "nudge") +
  #   geom_point(data = dataPlot, mapping = aes(x = value0, col = "Antes"), size = 6) +
  #   geom_point(data = dataPlot, mapping = aes(x = value1, col = "Depois"), size = 6)
  return(p)
}