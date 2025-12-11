optimalResBiota <- function(x, group = NULL, includeReference = TRUE){
  # Check object class
  if(!inherits(x, "simRestSelect")){
    stop("The x argument must be of class simRestSelect")
  }
  composition <- x$selection$composition
  nSim <- nrow(composition)
  xGroup <- x$selection$group
  # Set groups
  if(!is.null(group)){
    groupNames <- xGroup[, group]
  } else{
    groupNames <- rep("sim", nSim)
  }
  # Merge compositions - simulations and reference
  if(!is.null(x$reference) && includeReference){
    reference <- x$reference$composition
    nRef <- nrow(reference)
    template0 <- makeMatrixTemplate(composition, reference)
    composition <- reorganizeMatrix(template = template0, composition, fillNA = TRUE)
    reference <- reorganizeMatrix(template = template0, reference, fillNA = TRUE)
    # This sequence is important for split the results
    composition <- rbind(reference, composition)
    # x$reference$composition <- reference
    dbCombinations <- data.frame(v1 = c(rownames(composition)), v2 = c(rownames(reference), groupNames))
  } else{
    dbCombinations <- data.frame(v1 = c(rownames(composition)), v2 = groupNames)
  }
  # Make groups combinations
  dbCombinations <- makeCombinations(dbCombinations$v1, dbCombinations$v2)
  resCombinations <- matrix(NA, nrow(dbCombinations), ncol = 4)
  # resCombinations <- matrix(NA, nrow(dbCombinations), ncol = 3)
  for(i in 1:nrow(dbCombinations)){
    whichComb <- names(which(dbCombinations[i,]==1))
    resCombinations[i,] <- unlist(calcRAO(composition[whichComb,], averages = TRUE))
    # beta.multi(composition[whichComb,], index.family="sor")
    # compTemp <- composition[whichComb,]
    # compTemp[compTemp>0] <- 1
    # resCombinations[i,] <- unlist(calcRAO2(compTemp))
    # resCombinations[i,] <- unlist(beta.multi(compTemp, index.family="sor"))
    # beta.multi(compTemp, index.family="sor")
    # functional.beta.multi(compTemp, traits = cerrado.mini$traits[,1:4], index.family = "sor")
  }
  rownames(resCombinations) <- rownames(dbCombinations)
  colnames(resCombinations) <- c("total", "alpha", "beta", "Fst")
  # Set results
  x$selection$beta$combinations <- dbCombinations
  x$selection$beta$results <- resCombinations
  return(x)
}