classSimplerVector <- function(x){
  resClass <- class(x)
  resClass <- ifelse(resClass == "integer", "numeric", resClass)
  resClass <- ifelse(any(resClass == "ordered"), "factor", resClass)
  return(resClass)
}