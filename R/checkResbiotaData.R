#' @title Internal function to check data to resbiota Shiny Application
#' @encoding UTF-8
#' @param traits Data frame or matrix with species traits. Traits as columns and species as rows.
#' @param restComp A matrix with species proportions in the restoration sites. NAs not accepted.
#' @param restGroup Data frame or matrix with complementary information for restoration sites.
#' @param reference A matrix with species proportions in the reference sites.
#' @param supplementary A matrix with species proportions in the supplementary sites.
#' @returns A list with the elements:
#' \item{checkStatus}{The global status with the type of alert.}
#' \item{checkMessage}{A vector with error messages.}
#' \item{checkNA}{A vector with warning messages.}
#' @author See \code{\link{resbiota-package}}.
#' @seealso \code{\link{selectCommunities}}
#' @keywords Auxiliary
#' @export
checkResbiotaData <- function (traits = NULL, 
                               restComp = NULL, 
                               restGroup = NULL,
                               reference = NULL, 
                               supplementary = NULL) 
{
  RES <- list()
  checkStatus <- "success"
  checkMessage <- NULL
  checkNA <- NULL
  fCheckBasic <- function(x, objName = "obj"){
    res <- NULL
    if (!inherits(x, c("data.frame", "matrix"))) {
      # stop(paste0(objName, " must be a matrix or a data.frame"), call. = FALSE)
      res <- c(res, paste0("Error: ", objName, " must be a matrix or a data.frame"))
    }
    if (is.null(colnames(x))) {
      # stop(paste0(objName, " without column names"), call. = FALSE)
      res <- c(res, paste0("Error: ", objName, " without column names"))
    }
    if (is.null(rownames(x))) {
      # stop(paste0(objName, " without row names"), call. = FALSE)
      res <- c(res, paste0("Error: ", objName, " without row names"))
    }
    # if (any(is.na(x))) {
    #   # warning(paste0(objName, " with NA values"), call. = FALSE)
    #   res <- c(res, paste0("Warning: ", objName, " with NA values"))
    # }
    return(res)
  }
  fCheckNA <- function(x, objName = "obj"){
    res <- NULL
    if (any(is.na(x))) {
      # warning(paste0(objName, " with NA values"), call. = FALSE)
      res <- paste0("Warning: ", objName, " with NA values")
    }
    return(res)
  }
  if(!is.null(traits)){
    # fCheckBasic(traits, objName = "traits")
    checkMessage <- c(checkMessage, fCheckBasic(traits, objName = "traits"))
    checkNA <- c(checkNA, fCheckNA(traits, objName = "traits"))
    # traitsType <- var.type(traits)
    traitsType <- sapply(traits, vectorClass)
    if (any(traitsType == "character")) {
      # stop("traits must contain only numeric, binary, factor or ordinal variables")
      checkMessage <- c(checkMessage, "Error: traits must contain only numeric, binary, factor or ordinal variables")
    }
    sppTraits <- rownames(traits)
  } else{
    checkMessage <- c(checkMessage, "Error: traits data is mandatory")
  }
  sppComp <- c()
  if(!is.null(restComp)){
    # fCheckBasic(restComp, objName = "restComp")
    checkMessage <- c(checkMessage, fCheckBasic(restComp, objName = "restComp"))
    checkNA <- c(checkNA, fCheckNA(restComp, objName = "restComp"))
    # varType <- SYNCSA::var.type(restComp)
    varType <- sapply(restComp, vectorClass)
    if (any(varType == "character") | any(varType == "factor")) {
      # stop("restComp data must contain only numeric or binary variables")
      checkMessage <- c(checkMessage, "Error: restComp data must contain only numeric or binary variables")
    }
    sppComp <- c(sppComp, colnames(restComp))
  }
  if(!is.null(restGroup)){
    # fCheckBasic(restGroup, objName = "restGroup")
    checkMessage <- c(checkMessage, fCheckBasic(restGroup, objName = "restGroup"))
    checkNA <- c(checkNA, fCheckNA(restGroup, objName = "restGroup"))
  }
  if(!is.null(reference)){
    # fCheckBasic(reference, objName = "reference")
    checkMessage <- c(checkMessage, fCheckBasic(reference, objName = "reference"))
    checkNA <- c(checkNA, fCheckNA(reference, obj1Name = "reference"))
    # varType <- SYNCSA::var.type(reference)
    varType <- sapply(reference, vectorClass)
    if (any(varType == "character") | any(varType == "factor")) {
      # stop("reference data must contain only numeric or binary variables")
      checkMessage <- c(checkMessage, "Error: reference data must contain only numeric or binary variables")
    }
    sppComp <- c(sppComp, colnames(reference))
  }
  if(!is.null(supplementary)){
    # fCheckBasic(supplementary, objName = "supplementary")
    checkMessage <- c(checkMessage,  fCheckBasic(supplementary, objName = "supplementary"))
    checkNA <- c(checkNA, fCheckNA(supplementary, objName = "supplementary"))
    # varType <- SYNCSA::var.type(supplementary)
    varType <- sapply(supplementary, vectorClass)
    if (any(varType == "character") | any(varType == "factor")) {
      # stop("supplementary data must contain only numeric or binary variables")
      checkMessage <- c(checkMessage, "Error: supplementary data must contain only numeric or binary variables")
    }
    sppComp <- c(sppComp, colnames(supplementary))
  }
  if(!is.null(traits)){
    sppComp <- unique(sppComp)
    # sppTraits
    matchNames <- match(sppComp, sppTraits)
    if (sum(is.na(matchNames)) > 0) {
      # data.frame(species.missing.on.traits.data = setdiff(sppComp, sppTraits))
      # warning(c("Species missing on traits data", "\n", paste0(setdiff(sppComp, sppTraits), "\n")), call. = FALSE)
      # List difference species
      diffSpp <- setdiff(sppComp, sppTraits)
      nDiffSpp <- length(diffSpp)
      if(nDiffSpp>10){
        diffSpp <- c(diffSpp[1:5], "...", diffSpp[(nDiffSpp-4):nDiffSpp])
      }
      checkMessage <- c(checkMessage, paste0("Error: There are ", nDiffSpp, " species missing in the traits data: ", paste0(diffSpp, collapse = "; "), collapse = " "))
    }
  }
  if(!is.null(checkMessage)){
    checkStatus <- "error" 
  } else{
    if(!is.null(checkNA)){
      checkStatus <- "warning" 
    } 
  }
  RES <- list(checkStatus = checkStatus, 
              checkMessage = checkMessage,
              checkNA = checkNA)
  return(RES)
}