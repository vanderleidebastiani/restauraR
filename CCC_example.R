# Packages ----
require(CCC)
require(magrittr)
require(data.table)

# Print data.frame as data.table class
# getOption("datatable.print.class")
# options("datatable.print.class" = TRUE)
# getOption("datatable.print.nrows")
# options("datatable.print.nrows" = 15)
# print.data.frame <- function(x){
#   print(data.table::data.table(x, keep.rownames = TRUE))
# }

# Data ----
data("dados")
ls(dados)

# Initial check of the reference community ----
head(dados$trait)
resCheck <- checkReference(dados$ref,
                           trait = dados$trait,
                           cwm = dados$cwm,
                           rao = dados$cwm,
                           supplementary = dados$supplementary
)
str(resCheck, 1)
resCheck$reference$results 
resCheck$reference$summary
resCheck$supplementary$results
resCheck$supplementary$summary

# Simulation ----

## Simulate communities starting without species ----
resSIM0 <- simulateCommunities(dados$trait[70:110,], 
                               ava = dados$ava, 
                               it = dados$it, 
                               rich = dados$rich, 
                               cwm = dados$cwm, 
                               rao = dados$cwm,
                               phi = 1,
                               prefix = "New"
)
class(resSIM0)
str(resSIM0)
resSIM0$simulation$group %>% class()
resSIM0$simulation$composition %>% class()
resSIM0$simulation$composition
resSIM0$simulation$composition %>% head
resSIM0$simulation$group
resSIM0$simulation$group %>% head
resSIM0$simulation$group %>% dim


## Simulate communities starting with existing species ----
resSIM1 <- simulateCommunities(trait = dados$trait[80:120,],
                               ava = dados$ava,
                               und = dados$und,
                               it = dados$it,
                               rich = c(10, 12),
                               cwm = dados$cwm,
                               rao = dados$cwm,
                               rest = dados$rest,
                               restGroup = dados$restGroup,
                               max_add = dados$max_add, 
                               min_p = dados$min_p,
                               phi = 1, 
                               prefix = "Ongoing"
)

class(resSIM1)
str(resSIM1)
resSIM1$simulation$group %>% class()
resSIM1$simulation$composition %>% class()
resSIM1$simulation$composition
resSIM1$simulation$composition %>% head
resSIM1$simulation$group
resSIM1$simulation$group %>% head
resSIM1$simulation$group %>% dim

## Merge simulation ----
allSIM <- mergeSimulations(resSIM0, resSIM1)

class(allSIM)
str(allSIM)
allSIM$simulation$composition %>% dim
allSIM$simulation$group %>% head
allSIM$simulation$group %>% dim

# Calculate parameters ----
## Basic parameters ----

### Merged simulation ----
resParAllSIM <- calculateParameters(allSIM, 
                                    trait = dados$trait, 
                                    cwm = dados$cwm,
                                    rao = dados$cwm,
                                    # cwv = dados$cwm,
                                    ava = dados$ava,
                                    ref = dados$ref[1:10,],
                                    supplementary = dados$ref[11:19,]
)
class(resParAllSIM)
str(resParAllSIM, 2)

### Non merged simulation ----
resParSIM0 <- calculateParameters(resSIM0, 
                   trait = dados$trait, 
                   cwm = dados$cwm,
                   rao = dados$cwm,
                   cwv = dados$cwm,
                   ava = dados$ava,
                   ref = dados$ref[1:10,],
                   supplementary = dados$ref[11:19,]
)


class(resParSIM0)
str(resParSIM0, 2)
resParAllSIM$simulation$composition
resParAllSIM$simulation$group
resParAllSIM$simulation$results

resParAllSIM$reference$composition
resParAllSIM$reference$results

resParAllSIM$supplementary$composition
resParAllSIM$supplementary$results


## Multifunctionality ----
resCheck$reference$summary

target <- c("CWM_LMA > 0.105", "rao > 2.9", "CWM_Resprouter < 0.76")

resParAllSIM <- calculateMultifunctionality(resParAllSIM,
                            tests = target
                            # where = "global"
                            )
resParAllSIM$simulation$multifunctionality
resParAllSIM$simulation$results

## Dissimilarity ----
resParAllSIM <- calculateDissimilarity(resParAllSIM, 
                                       dados$trait[,1:2] 
                                       # where = "global"
                                       )
resParAllSIM$simulation$results


# Select communities ----

## Global selection (first step) ----
targetSelect1 <- c("CWM_LMA > 0.08", "rao > 2.9", "CWM_Resprouter > 0.5")
resSelectSim <- selectCommunities(resParAllSIM, 
                                  tests = targetSelect1)
class(resSelectSim)
resSelectSim$selection$results %>% dim
resSelectSim$selection$results
resSelectSim$selection$N
resSelectSim$selection$thresholds

## Additional selection (second step) ----
targetSelect2 <- c("CWM_LMA > 0.08", "rao < 3", "CWM_Resprouter > 0.5")
resSelectSim <- selectCommunities(resSelectSim, 
                                  tests = targetSelect2)
str(resSelectSim,1)
resSelectSim$selection$group %>% dim
resSelectSim$selection$composition
resSelectSim$selection$results
resSelectSim$selection$N
resSelectSim$selection$thresholds

## Global selection with dissimilarity and/or multifunctionality ----
# targetSelect3 <- c("multifunctionality >= 2", "dissimilarity < 0.1")
targetSelect3 <- c("multifunctionality >= 2")
resSelectSim3 <- selectCommunities(resParAllSIM, 
                                   tests = targetSelect3)
resSelectSim3$selection$results
resSelectSim3$selection$N
resSelectSim3$selection$thresholds

# Merge selections ----
## Selections ----
targetSelect4 <- c("PREFIX == 'Ongoing'", "restGroup == 'nonEdge'", "rao > 3.6")
resSelectSimPart1 <- selectCommunities(resParAllSIM, 
                                  tests = targetSelect4)

resSelectSimPart1$selection$results
resSelectSimPart1$selection$N

targetSelect5 <- c("PREFIX == 'Ongoing'", "restGroup == 'edge'", "rao > 3.6")
resSelectSimPart2 <- selectCommunities(resParAllSIM, 
                                   tests = targetSelect5)
resSelectSimPart2$selection$results
resSelectSimPart2$selection$N

## Merge ----
resSelectSimMerged <- mergeSelection(resSelectSimPart1, resSelectSimPart2)

resSelectSimMerged$selection$group %>% dim
resSelectSimMerged$selection$composition %>% dim
resSelectSimMerged$selection$results %>% dim

# Extras ----

## Selection then calculate dissimilarity and multifunctionality ----

### First selection ----
targetSelect6 <- c("rao > 5")
resParSelectExtra <- selectCommunities(resParSIM0, 
                                   tests = targetSelect6)
resParSelectExtra$selection$N
resParSelectExtra$selection$results

### Dissimilarity ----
resParSelectExtra <- calculateDissimilarity(resParSelectExtra, dados$trait[,1:2])
resParSelectExtra$selection$results

### Multifunctionality ----
targetMulti <- c("CWM_Height > 50", "CWM_Resprouter > 0.05")
resParSelectExtra <- calculateMultifunctionality(resParSelectExtra, tests = targetMulti)
resParSelectExtra$selection$multifunctionality
resParSelectExtra$selection$results

### Additional selection (second step) ----
targetSelect7 <- c("multifunctionality >= 2")
resParSelectExtra <- selectCommunities(resParSelectExtra, tests = targetSelect7)
resParSelectExtra$selection$results
resParSelectExtra$reference$results
resParSelectExtra$supplementary$results