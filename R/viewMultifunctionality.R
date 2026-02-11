#' @rdname viewResults
#' @include viewResults.R
#' @encoding UTF-8
#' @export
viewMultifunctionality <- function(x, hideref = FALSE, ...){
  # Check object class
  if(!c(inherits(x, "simRest") || inherits(x, "simRestSelect"))){
    stop("The x argument must be of class simRest or simRestSelect")
  }
  if(inherits(x, "simRest")){
    resMulti <- x$simulation$multifunctionality
  } else{
    resMulti <- x$selection$multifunctionality
  }
  if(is.null(resMulti)){
    stop("The x argument must contain multifunctionality results")
  }
  # Remove first name (SIM)
  groupsMulti <- colnames(resMulti)[-1]
  resMulti <- resMulti[, -1, drop =  FALSE]
  if(!hideref){
    resMultiRef <- x$reference$multifunctionality[, -1, drop =  FALSE]
    template0 <- makeMatrixTemplate(resMulti, resMultiRef)
    resMulti <- rearrangementMatrix(template = template0, resMulti, fillNA = FALSE)
    resMultiRef <- rearrangementMatrix(template = template0, resMultiRef, fillNA = FALSE)
    resMulti <- rbind.data.frame(resMulti, resMultiRef)
  }
  if(nrow(resMulti)>0){
    # p <- ComplexUpset::upset(resMulti, intersect = groupsMulti,
    #                          keep_empty_groups = TRUE,
    #                          name = "Groups",
    #                          base_annotations = list("Intersection size" = ComplexUpset::intersection_size(counts = FALSE, fill = "#1d4b61") +
    #                                                    ggplot2::theme(axis.title = ggplot2::element_text(size = 12*0.9, face = "bold"))),
    #                          set_sizes = ComplexUpset::upset_set_size(geom = ggplot2::geom_bar(width = 0.6, fill = "#1d4b61")) + 
    #                            ggplot2::theme(axis.title = ggplot2::element_text(size = 12*0.9, face = "bold")),
    #                          sort_intersections_by = "degree",
    #                          width_ratio = 0.2,
    #                          height_ratio = 0.8, 
    #                          ...) + 
    #   ggplot2::theme(axis.title = ggplot2::element_text(size = 15*0.9, face = "bold"))
    p <- upset(resMulti, intersect = groupsMulti,
                             keep_empty_groups = TRUE,
                             name = "Groups",
                             base_annotations = list("Intersection size" = ComplexUpset::intersection_size(counts = FALSE, fill = "#1d4b61") +
                                                       ggplot2::theme(axis.title = ggplot2::element_text(size = 12*0.9, face = "bold"))),
                             set_sizes = ComplexUpset::upset_set_size(geom = ggplot2::geom_bar(width = 0.6, fill = "#1d4b61")) + 
                               ggplot2::theme(axis.title = ggplot2::element_text(size = 12*0.9, face = "bold")),
                             sort_intersections_by = "degree",
                             width_ratio = 0.2,
                             height_ratio = 0.8, 
                             ...) + 
      ggplot2::theme(axis.title = ggplot2::element_text(size = 15*0.9, face = "bold"))
  } else{
    p <- ggplot2::ggplot() +
      themeResbiota(baseSize = 15)
  }
  return(p)
}