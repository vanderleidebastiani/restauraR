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
  groupsMulti <- colnames(resMulti)
  p <- ComplexUpset::upset(resMulti, intersect = groupsMulti,
                      keep_empty_groups = TRUE,
                      name = "Groups",
                      base_annotations = list("Intersection size" = ComplexUpset::intersection_size(counts = FALSE)),
                      sort_intersections_by = "degree",
                      width_ratio = 0.2,
                      height_ratio = 0.5, 
                      ...)
  return(p)
}
