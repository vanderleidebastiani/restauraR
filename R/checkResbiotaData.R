#' @title Internal function to data validation for resbiota package inputs
#' @encoding UTF-8
#' @param traits Data frame or matrix with species traits. Traits as columns and species as rows.
#' @param restComp Community matrix with species composition in the restoration sites. NAs not accepted.
#' @param restGroup Data frame or matrix with complementary information for restoration sites.
#' @param reference Community matrix with species composition in the reference sites.
#' @param supplementary Community matrix with species composition in the supplementary sites.
#' @param traitsDist Distance matrix between species, based on functional trait values.
#' @param cooccur A matrix with co-occurrence probabilities between species.
#' @param asList A logical argument to specify if return the results in as list format or trigger errors/warnings directly.
#' @returns A list with the elements when `asList = TRUE`:
#' \item{checkStatus}{The global validation status: "success", "warning", or "error".}
#' \item{checkMessage}{Character vector containing error messages.}
#' \item{checkWarning}{Character vector containing warning messages.}
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
                               cooccur = NULL,
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
        res <- c(res, paste0("Error: The ", objName, " must be a matrix or data.frame"))  
      } else{
        stop(paste0("The ", objName, " must be a matrix or data.frame"), call. = FALSE)
      }
    }
    if (is.null(colnames(x))) {
      if(asList){
        res <- c(res, paste0("Error: The ", objName, " must have column names"))
      } else{
        stop(paste0("The ", objName, " must have column names"), call. = FALSE)
      }
    }
    if (is.null(rownames(x))) {
      if(asList){
        res <- c(res, paste0("Error: The ", objName, " must have row names"))
      } else{
        stop(paste0("The ", objName, " must have row names"), call. = FALSE)
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
          res <- c(res, paste0("Error: The ", objName, " must be a square matrix or distance matrix"))  
        } else{
          stop(paste0("The ", objName, " must be a square matrix or distance matrix"), call. = FALSE)
        }
      }
      if (is.null(colnames(x))) {
        if(asList){
          res <- c(res, paste0("Error: The ", objName, " must have column names"))
        } else{
          stop(paste0("The ", objName, " must have column names"), call. = FALSE)
        }
      }
      if (is.null(rownames(x))) {
        if(asList){
          res <- c(res, paste0("Error: The ", objName, " must have row names"))
        } else{
          stop(paste0("The ", objName, " must have row names"), call. = FALSE)
        }
      }
      if (!any(rownames(x) == colnames(x))) {
        if(asList){
          res <- c(res, paste0("Error: The ", objName, " must be a distance matrix or square matrix with matching row and column names in the same order"))
        } else{
          stop(paste0("The ", objName, " must be a distance matrix or square matrix with matching row and column names in the same order"), call. = FALSE)
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
        checkMessage <- c(checkMessage, "Error: The traits data must contain only numeric, binary, factor or ordinal variables")  
      } else{
        stop("The traits data must contain only numeric, binary, factor or ordinal variables", call. = FALSE)
      }
    }
    sppTraits <- rownames(traits)
  } else{
    if(asList){
      checkMessage <- c(checkMessage, "Error: The traits data is requerid")  
    } else{
      stop("The traits data is requerid", call. = FALSE)
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
        checkMessage <- c(checkMessage, "Error: The restComp data must contain only numeric or binary variables")  
      } else{
        stop("The restComp data must contain only numeric or binary variables", call. = FALSE)
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
        checkMessage <- c(checkMessage, "Error: The reference data must contain only numeric or binary variables")  
      } else{
        stop("The reference data must contain only numeric or binary variables", call. = FALSE)
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
        checkMessage <- c(checkMessage, "Error: The supplementary data must contain only numeric or binary variables")  
      } else{
        stop("The supplementary data must contain only numeric or binary variables", call. = FALSE)
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
  if(!is.null(cooccur)){
    # Check only if cooccur is a matrix or distance matrix
    if(asList){
      checkMessage <- c(checkMessage,  fCheckDist(cooccur, objName = "cooccur"))
      checkWarning <- c(checkWarning, fCheckNA(cooccur, objName = "cooccur"))  
    } else{
      fCheckDist(cooccur, objName = "cooccur", asList = FALSE)
      fCheckNA(cooccur, objName = "cooccur", asList = FALSE)
    }
    if(inherits(cooccur, c("data.frame", "dist", "matrix"))){
      cooccur <- as.matrix(cooccur)
      sppCooc <- colnames(cooccur)
    } else{
      sppCooc <- NULL
    }
  } else{
    sppCooc <- NULL
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
        checkMessage <- c(checkMessage, paste0("Error: The following ", nDiffSpp, " species are missing from the traits data: ", paste0(diffSpp, collapse = "; "), collapse = " "))  
      } else{
        stop(paste0("The following ", nDiffSpp, " species are missing from the traits data: ", paste0(diffSpp, collapse = "; "), collapse = " "), call. = FALSE)
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
          checkMessage <- c(checkMessage, paste0("Error: The following ", nDiffSpp, " species are missing from the distance matrix: ", paste0(diffSpp, collapse = "; "), collapse = " "))  
        } else{
          stop(paste0("The following ", nDiffSpp, " species are missing from the distance matrix: ", paste0(diffSpp, collapse = "; "), collapse = " "), call. = FALSE)
        }
      }
    }
    if(!is.null(sppCooc)){
      matchNamesCooc <- match(sppTraits, sppCooc)
      if (sum(is.na(matchNamesCooc)) > 0) {
        # List difference species
        diffSppCooc <- setdiff(sppTraits, sppCooc)
        nDiffSppCooc <- length(diffSppCooc)
        # Return only 10 species names
        if(nDiffSppCooc>10){
          diffSppCooc <- c(diffSppCooc[1:5], "...", diffSppCooc[(nDiffSppCooc-4):nDiffSppCooc])
        }
        if(asList){
          checkMessage <- c(checkMessage, paste0("Error: The following ", nDiffSppCooc, " species are missing from the co-occurrence matrix: ", paste0(diffSppCooc, collapse = "; "), collapse = " "))
        } else{
          stop(paste0("The following ", nDiffSppCooc, " species are missing from the co-occurrence matrix: ", paste0(diffSppCooc, collapse = "; "), collapse = " "), call. = FALSE)
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