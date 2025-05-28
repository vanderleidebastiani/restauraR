#' @title Internal function to check data to resbiota package
#' @encoding UTF-8
#' @param traits Data frame or matrix with species traits. Traits as columns and species as rows.
#' @param restComp A matrix with species proportions in the restoration sites. NAs not accepted.
#' @param restGroup Data frame or matrix with complementary information for restoration sites.
#' @param reference A matrix with species proportions in the reference sites.
#' @param supplementary A matrix with species proportions in the supplementary sites.
#' @param traitsDist A distance matrix between species, based on functional trait values.
#' @param asList A logical argument to return the results in as list format.
#' @returns A list with the elements:
#' \item{checkStatus}{The global status with the type of alert.}
#' \item{checkMessage}{A vector with error messages.}
#' \item{checkWarning}{A vector with warning messages.}
#' @author See \code{\link{resbiota-package}}.
#' @seealso \code{\link{selectCommunities}}
#' @keywords Auxiliary
#' @export
checkResbiotaData <- function (traits = NULL, 
                               restComp = NULL, 
                               restGroup = NULL,
                               reference = NULL, 
                               supplementary = NULL,
                               traitsDist = NULL,
                               asList = TRUE)
{
  RES <- list()
  checkStatus <- "success"
  checkMessage <- NULL
  checkWarning <- NULL
  fCheckBasic <- function(x, objName = "obj", asList = TRUE){
    res <- NULL
    if (!inherits(x, c("data.frame", "matrix"))) {
      if(asList){
        res <- c(res, paste0("Error: ", objName, " must be a matrix or a data.frame"))  
      } else{
        stop(paste0(objName, " must be a matrix or a data.frame"), call. = FALSE)
      }
    }
    if (is.null(colnames(x))) {
      if(asList){
        res <- c(res, paste0("Error: ", objName, " without column names"))
      } else{
        stop(paste0(objName, " without column names"), call. = FALSE)
      }
    }
    if (is.null(rownames(x))) {
      if(asList){
        res <- c(res, paste0("Error: ", objName, " without row names"))
      } else{
        stop(paste0(objName, " without row names"), call. = FALSE)
      }
    }
    if(asList){
      return(res)
    }
  }
  fCheckDist <- function(x, objName = "obj", asList = TRUE){
    res <- NULL
    if(inherits(x, c("data.frame", "dist"))){
      x <- as.matrix(x)
    }
    # Check only if x is a matrix, data.frame or distance matrix
    if(inherits(x, "matrix")){
      nRow <- nrow(x)
      nCol <- ncol(x)
      if (nRow!=nCol){
        if(asList){
          res <- c(res, paste0("Error: ", objName, " is not a distance matrix or a square matrix"))  
        } else{
          stop(paste0(objName, " is not a distance matrix or a square matrix"), call. = FALSE)
        }
      }
      if (is.null(colnames(x))) {
        if(asList){
          res <- c(res, paste0("Error: ", objName, " without column names"))
        } else{
          stop(paste0(objName, " without column names"), call. = FALSE)
        }
      }
      if (is.null(rownames(x))) {
        if(asList){
          res <- c(res, paste0("Error: ", objName, " without row names"))
        } else{
          stop(paste0(objName, " without row names"), call. = FALSE)
        }
      }
      if(asList){
        return(res)
      }
    }
  }
  fCheckNA <- function(x, objName = "obj", asList = TRUE){
    res <- NULL
    if (any(is.na(x))) {
      if(asList){
        res <- paste0("Warning: ", objName, " with NA values")  
      } else{
        warning(paste0(objName, " with NA values"), call. = FALSE)
      }
    }
    if(asList){
      return(res)
    }
  }
  if(!is.null(traits)){
    if(asList){
      checkMessage <- c(checkMessage, fCheckBasic(traits, objName = "traits"))
      checkWarning <- c(checkWarning, fCheckNA(traits, objName = "traits"))  
    } else{
      fCheckBasic(traits, objName = "traits", asList = FALSE)
      fCheckNA(traits, objName = "traits", asList = FALSE)
    }
    traitsType <- sapply(traits, vectorClass)
    if (any(traitsType == "character")) {
      if(asList){
        checkMessage <- c(checkMessage, "Error: traits must contain only numeric, binary, factor or ordinal variables")  
      } else{
        stop("traits must contain only numeric, binary, factor or ordinal variables", call. = FALSE)
      }
    }
    sppTraits <- rownames(traits)
  } else{
    if(asList){
      checkMessage <- c(checkMessage, "Error: traits data is mandatory")  
    } else{
      stop("traits data is mandatory", call. = FALSE)
    }
  }
  sppComp <- c()
  if(!is.null(restComp)){
    if(asList){
      checkMessage <- c(checkMessage, fCheckBasic(restComp, objName = "restComp"))
      checkWarning <- c(checkWarning, fCheckNA(restComp, objName = "restComp"))  
    } else{
      fCheckBasic(restComp, objName = "restComp", asList = FALSE)
      fCheckNA(restComp, objName = "restComp", asList = FALSE)
    }
    varType <- sapply(restComp, vectorClass)
    if (any(varType == "character") | any(varType == "factor")) {
      if(asList){
        checkMessage <- c(checkMessage, "Error: restComp data must contain only numeric or binary variables")  
      } else{
        stop("restComp data must contain only numeric or binary variables", call. = FALSE)
      }
    }
    sppComp <- c(sppComp, colnames(restComp))
  }
  if(!is.null(restGroup)){
    if(asList){
      checkMessage <- c(checkMessage, fCheckBasic(restGroup, objName = "restGroup"))
      checkWarning <- c(checkWarning, fCheckNA(restGroup, objName = "restGroup"))  
    } else{
      fCheckBasic(restGroup, objName = "restGroup", asList = FALSE)
      fCheckNA(restGroup, objName = "restGroup", asList = FALSE)
    }
  }
  if(!is.null(reference)){
    if(asList){
      checkMessage <- c(checkMessage, fCheckBasic(reference, objName = "reference"))
      checkWarning <- c(checkWarning, fCheckNA(reference, objName = "reference"))
    } else{
      fCheckBasic(reference, objName = "reference", asList = FALSE)
      fCheckNA(reference, objName = "reference", asList = FALSE)
    }
    varType <- sapply(reference, vectorClass)
    if (any(varType == "character") | any(varType == "factor")) {
      if(asList){
        checkMessage <- c(checkMessage, "Error: reference data must contain only numeric or binary variables")  
      } else{
        stop("reference data must contain only numeric or binary variables", call. = FALSE)
      }
    }
    sppComp <- c(sppComp, colnames(reference))
  }
  if(!is.null(supplementary)){
    if(asList){
      checkMessage <- c(checkMessage,  fCheckBasic(supplementary, objName = "supplementary"))
      checkWarning <- c(checkWarning, fCheckNA(supplementary, objName = "supplementary"))  
    } else{
      fCheckBasic(supplementary, objName = "supplementary", asList = FALSE)
      fCheckNA(supplementary, objName = "supplementary", asList = FALSE)
    }
    varType <- sapply(supplementary, vectorClass)
    if (any(varType == "character") | any(varType == "factor")) {
      if(asList){
        checkMessage <- c(checkMessage, "Error: supplementary data must contain only numeric or binary variables")  
      } else{
        stop("supplementary data must contain only numeric or binary variables", call. = FALSE)
      }
    }
    sppComp <- c(sppComp, colnames(supplementary))
  }
  if(!is.null(traitsDist)){
    # Check only if traitsDist is a matrix or distance matrix
    if(asList){
      checkMessage <- c(checkMessage,  fCheckDist(traitsDist, objName = "traitsDist"))
      checkWarning <- c(checkWarning, fCheckNA(traitsDist, objName = "traitsDist"))  
    } else{
      fCheckDist(traitsDist, objName = "traitsDist", asList = FALSE)
      fCheckNA(traitsDist, objName = "traitsDist", asList = FALSE)
    }
    if(inherits(traitsDist, c("data.frame", "dist", "matrix"))){
      traitsDist <- as.matrix(traitsDist)
      sppDist <- colnames(traitsDist)
    } else{
      sppDist <- NULL
    }
  }
  if(!is.null(traits)){
    sppComp <- unique(sppComp)
    matchNames <- match(sppComp, sppTraits)
    if (sum(is.na(matchNames)) > 0) {
      # List difference species
      diffSpp <- setdiff(sppComp, sppTraits)
      nDiffSpp <- length(diffSpp)
      # Return only 10 species names
      if(nDiffSpp>10){
        diffSpp <- c(diffSpp[1:5], "...", diffSpp[(nDiffSpp-4):nDiffSpp])
      }
      if(asList){
        checkMessage <- c(checkMessage, paste0("Error: There are ", nDiffSpp, " species missing in the traits data: ", paste0(diffSpp, collapse = "; "), collapse = " "))  
      } else{
        stop(paste0("There are ", nDiffSpp, " species missing in the traits data: ", paste0(diffSpp, collapse = "; "), collapse = " "), call. = FALSE)
      }
    }
    if(!is.null(traitsDist) && !is.null(sppDist)){
      matchNamesDist <- match(sppTraits, sppDist)
      if (sum(is.na(matchNamesDist)) > 0) {
        # List difference species
        diffSpp <- setdiff(sppTraits, sppDist)
        nDiffSpp <- length(diffSpp)
        # Return only 10 species names
        if(nDiffSpp>10){
          diffSpp <- c(diffSpp[1:5], "...", diffSpp[(nDiffSpp-4):nDiffSpp])
        }
        if(asList){
          checkMessage <- c(checkMessage, paste0("Error: There are ", nDiffSpp, " species missing in distance matrix between species: ", paste0(diffSpp, collapse = "; "), collapse = " "))  
        } else{
          stop(paste0("There are ", nDiffSpp, " species missing in the distance matrix between species: ", paste0(diffSpp, collapse = "; "), collapse = " "), call. = FALSE)
        }
      }
    }
  }
  if(asList){
    # Change checkStatus
    if(!is.null(checkMessage)){
      checkStatus <- "error" 
    } else{
      if(!is.null(checkWarning)){
        checkStatus <- "warning" 
      } 
    }
    # Return list with the check results (only if asList is TRUE)
    RES <- list(checkStatus = checkStatus, 
                checkMessage = checkMessage,
                checkWarning = checkWarning)
    return(RES)  
  }
}