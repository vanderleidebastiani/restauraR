#' @rdname viewResults
#' @include viewResults.R
#' @encoding UTF-8
#' @export
viewMultifunctionality <- function(x, ...){
  if(inherits(x, "simRest")){
    resMulti <- x$simulation$multifunctionality
  } else{
    resMulti <- x$selection$multifunctionality
  }
  # Remove first name (SIM)
  groupsMulti <- colnames(resMulti)[-1]
  p <- ComplexUpset::upset(resMulti, intersect = groupsMulti,
                           keep_empty_groups = TRUE,
                           name = "Groups",
                           base_annotations = list("Intersection size" = ComplexUpset::intersection_size(counts = FALSE, fill = "#1F78B4") +
                                                     ggplot2::theme(axis.title = ggplot2::element_text(size = 12*0.9, face = "bold"))),
                           set_sizes = ComplexUpset::upset_set_size(geom = ggplot2::geom_bar(width = 0.6, fill = "#1F78B4")) + 
                             ggplot2::theme(axis.title = ggplot2::element_text(size = 12*0.9, face = "bold")),
                           sort_intersections_by = "degree",
                           width_ratio = 0.2,
                           height_ratio = 0.8, 
                           ...) + 
    ggplot2::theme(axis.title = ggplot2::element_text(size = 12*0.9, face = "bold"))
  return(p)
}
