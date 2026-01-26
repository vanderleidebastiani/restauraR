# data("cerrado.mini")
# head(cerrado.mini$traits)
# # Simulation
# scenario <- simulateCommunities(traits = cerrado.mini$traits,
#                          ava = "Available",
#                          maxDiver = c("SLA", "Height", "Seed"),
#                          constCWM = "BT",
#                          rich = c(10, 15),
#                          it = 100)
# scenario
# # Compute functional parameters
# scenario <- computeParameters(x = scenario,
#                               traits = cerrado.mini$traits,
#                     ava = "Available",
#                     cwm = "BT",
#                     rao = c("SLA", "Height", "Seed"),
#                     cost = "Cost",
#                     dens = "Density",
#                     reference = cerrado.mini$reference,
#                     supplementary = cerrado.mini$supplementary)
# scenario
# # Select communities - Deterministic selection
# scenarioSelected <- selectCommunities(x = scenario,
#                                       testsFilter = c("CWM_BT > 6",
#                                                 "rao > 2.5"))
# scenarioSelected
# # Select communities - Hierarchical selection
# scenarioSelected <- selectCommunities(x = scenario,
#                                       testsPriority = c("CWM_BT > 6",
#                                                 "rao > 2.5"),
#                                       singleselection = FALSE)
# scenarioSelected
# 
# 
# a <- optimalResBiota(scenarioSelected)
# a$selection$results
# a$selection$beta$combinations
# a$selection$beta$results
# # plot(a$selection$beta$results[,1])
# x <- scenarioSelected
# nrow(a$selection$beta$results)
# 
# which(a$selection$beta$results[,"total"] == max(a$selection$beta$results[,"total"]))
# a$selection$beta$results[57,]
# a$selection$beta$combinations[57,]
#' @export
optimizeSelection <- function(x, group = NULL, includeReference = TRUE, maxComb = 1000){
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
  dbCombinations <- makeCombinations(dbCombinations$v1, dbCombinations$v2, minSubset = 1, maxSubset = 1, maxComb = maxComb)
  resCombinations <- matrix(NA, nrow(dbCombinations), ncol = 4)
  # resCombinations <- matrix(NA, nrow(dbCombinations), ncol = 3)
  for(i in 1:nrow(dbCombinations)){
    whichComb <- names(which(dbCombinations[i,]==1))
    # VER PARTE FUNCIONAL
    resCombinations[i,] <- unlist(calcRAO(composition[whichComb,, drop = FALSE], averages = TRUE))
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