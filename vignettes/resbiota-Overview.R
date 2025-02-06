## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  eval = TRUE, 
  echo = TRUE,
  collapse = TRUE,
  results = 'hold',
  fig.height = 5, 
  fig.width = 6, 
  fig.align = 'center',
  comment = "#>"
)
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())

## ----message = FALSE, echo = FALSE, fig.cap="Flowchart with the main steps of the framework", fig.width = 6.5----
require(DiagrammeR)
DiagrammeR::grViz(
  "digraph resbiotaFlowchart {
      # define node aesthetics
      graph[splines = ortho]
      node [fontname = Helvetica, shape = box, width = 5, height = 1, fontsize = 16, style = filled, fillcolor = whitesmoke]
      bgcolor = transparent
      
      # Nodes
      dataSpeciesTraits[label = <Species traits>, fillcolor = Gold]
      dataRestorationComp[label = <Species composition to restoration sites>, fillcolor = PaleGoldenrod]
      dataRefenceComp[label = <Species composition to reference sites>, fillcolor = PaleGoldenrod]
      dataSupplementaryComp[label = <Species composition to supplementary sites>, fillcolor = PaleGoldenrod]
      simulateCommunities[label = <Set simulation input parameters<br/><i>simulateCommunities</i>>, fillcolor = SeaGreen, fontcolor = snow]
      computeParameters[label = <Compute basic parameters in each simulated community<br/><i>computeParameters</i>>, fillcolor = SeaGreen, fontcolor = snow]
      standardizeParameters[label = <Calculated parameters can be standardised<br/><i>standardizeParameters</i>>, fillcolor = MediumSeaGreen]
      computeMultifunctionality[label = <Calculate multiple restoration targets, called multifunctionality<br/><i>computeMultifunctionality</i>>, fillcolor = MediumSeaGreen]
      selectCommunities[label = <Selections of simulated communities<br/><i>selectCommunities</i>>, fillcolor = SeaGreen, fontcolor = snow]
      viewResults[label = <Basic results visualization<br/><i>viewResults</i>>, fillcolor = Goldenrod]
      viewMultifunctionality[label = <Multifunctionality visualization<br/><i>viewMultifunctionality</i>>, fillcolor = Goldenrod]
      extractResults[label = <Extract and save the results<br/><i>extractResults</i>>, fillcolor = Goldenrod]
      mergeSelection[label = <Merge selections scenarios<br/><i>mergeSelection</i>>, fillcolor = Plum]
      mergeSimulations[label = <Merge simulations scenarios<br/><i>mergeSimulations</i>>, fillcolor = Plum]
      
      # Edges
      dataSpeciesTraits -> simulateCommunities
      dataSpeciesTraits -> dataRestorationComp
      dataSpeciesTraits -> dataSupplementaryComp
      dataSpeciesTraits -> dataRefenceComp
      dataRestorationComp -> simulateCommunities
      dataRefenceComp -> computeParameters
      dataSupplementaryComp -> computeParameters
      simulateCommunities -> computeParameters
      simulateCommunities -> mergeSimulations
      mergeSimulations -> simulateCommunities
      computeParameters -> standardizeParameters
      computeParameters -> computeMultifunctionality
      computeParameters -> selectCommunities
      standardizeParameters -> selectCommunities
      computeMultifunctionality -> selectCommunities
      standardizeParameters -> computeMultifunctionality
      selectCommunities -> mergeSelection
      mergeSelection -> selectCommunities
      selectCommunities -> viewResults
      selectCommunities -> viewMultifunctionality
      selectCommunities -> extractResults
      
      # Rank
      {rank = same; computeParameters dataSupplementaryComp}
      {rank = same; simulateCommunities mergeSimulations dataRefenceComp}
      {rank = same; selectCommunities mergeSelection}
      
     }"
)

## ----message = FALSE----------------------------------------------------------
# Set seed
set.seed(2024)
# Package load
require(resbiota)

## -----------------------------------------------------------------------------
# Load cerrado data
data(cerrado.mini)
# Traits data
traits <- cerrado.mini$traits
# Species composition in reference sites
referenceSites <- cerrado.mini$reference
# Species composition in restoration sites
restorationSites <- cerrado.mini$restoration
# Species composition in supplementary sites
supplementarySites <- cerrado.mini$supplementary

## ----echo = FALSE-------------------------------------------------------------
knitr::kable(head(traits), digits = 3, row.names = TRUE, caption = "Traits data (first 6 rows only)")
knitr::kable(head(referenceSites, n = c(6, 3)), digits = 3, row.names = TRUE, caption = "Reference sites (first 3 columns only)")
knitr::kable(head(restorationSites, n = c(6, 3)), digits = 3, row.names = TRUE, caption = "Restoration sites (first 3 columns only)")
knitr::kable(head(supplementarySites, n = c(6, 3)), digits = 3, row.names = TRUE, caption = "Supplementary sites (first 3 columns only)")

## -----------------------------------------------------------------------------
# Initial check of the reference community
resCheck <- checkReference(reference = referenceSites,
                           trait = traits,
                           cwm = "BT", 
                           rao = c("BT", "SLA", "Height", "Seed"),
                           supplementary = supplementarySites,
                           props = c(0.75)
)
resCheck

## -----------------------------------------------------------------------------
# First scenario
scenarioA <- simulateCommunities(trait = traits, 
                                 ava = "Available", 
                                 it = 100, 
                                 rich = c(10, 15), 
                                 cwm = "BT", 
                                 rao = c("BT", "SLA", "Height", "Seed"),
                                 phi = 1,
                                 prefix = "New"
)
scenarioA

## -----------------------------------------------------------------------------
# Second scenario
scenarioB <- simulateCommunities(trait = traits, 
                                 restComp = restorationSites,
                                 ava = "Available", 
                                 it = 100, 
                                 rich = c(10, 15), 
                                 cwm = "BT", 
                                 rao = c("BT", "SLA", "Height", "Seed"),
                                 phi = 1,
                                 prefix = "Ongoing"
)
scenarioB

## -----------------------------------------------------------------------------
# Merge all scenarios
allScenarios <- mergeSimulations(scenarioA, scenarioB)
allScenarios 

## -----------------------------------------------------------------------------
# Compute functional parameters
allScenarios <- computeParameters(x = allScenarios,
                                  trait = traits,
                                  ava = "Available",
                                  cwm = "BT",
                                  rao = c("SLA", "Height", "Seed"),
                                  cost = "Cost",
                                  dens = "Density",
                                  dissimilarity = c("SLA", "Height", "Seed"),
                                  reference = referenceSites,
                                  supplementary = supplementarySites)
allScenarios

## -----------------------------------------------------------------------------
# Compute functional parameters using list in Rao
# Make list
list2rao <- list(c("BT", "SLA", "Height", "Seed"), 
                 c("SLA", "Height", "Seed"))
# The list can be named
names(list2rao) <- c("allTraits", 
                     "chosenTraits")
list2rao

## -----------------------------------------------------------------------------
# Compute functional parameters
allScenariosAlternative <- computeParameters(x = allScenarios,
                                             trait = traits,
                                             ava = "Available",
                                             cwm = "BT",
                                             rao = list2rao,
                                             cost = "Cost",
                                             dens = "Density",
                                             reference = referenceSites,
                                             supplementary = supplementarySites)
allScenariosAlternative

## -----------------------------------------------------------------------------
# Compute multifunctionality
allScenarios <- computeMultifunctionality(x = allScenarios, 
                                 tests = c("CWM_BT > 6",
                                           "rao > 2.5"))
# All parameters calculated
allScenarios

## -----------------------------------------------------------------------------
# Global selection (first step)
targetSelect1 <- c("CWM_BT > 6", "rao > 2.5")
resSelectSim <- selectCommunities(allScenarios, 
                                  testsDet = targetSelect1)

