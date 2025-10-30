# resbiota Example ----

## Packages ----
require(resbiota)
require(magrittr)
# require(data.table)

# Print data.frame as data.table class
# getOption("datatable.print.class")
# options("datatable.print.class" = TRUE)
# getOption("datatable.print.nrows")
# options("datatable.print.nrows" = 15)
# print.data.frame <- function(x){
#   print(data.table::data.table(x, keep.rownames = TRUE))
# }

## Data ----
load(file = "data/dados.rda")
ls(dados)
?checkReference

## Initial check of the reference community ----
head(dados$trait)
resCheck <- checkReference(reference = dados$ref,
                           trait = dados$trait,
                           cwm = dados$cwm,
                           rao = dados$cwm,
                           supplementary = dados$supplementary,
                           props = c(0.75)
)
resCheck
str(resCheck, 1)
resCheck$pool$results
resCheck$pool$summary
resCheck$reference$results
resCheck$reference$summary
resCheck$supplementary$results
resCheck$supplementary$summary

## Exploratory report (html)
createReport(dados$trait, props = c(0.1, 0.5, 0.75))

# FINALIZAR ----
# funcao para definir os target (percentil?)
# referencia, pool, ...
# testar se tem atributos apartir da literatura
# etapa exploratoria
# pensar em percentil na funcao da multi
# incluir os graficos exploratorios
## Simulation ----

### Simulate communities starting without species ----
resSIM0 <- simulateCommunities(dados$trait[70:110,], 
                               ava = dados$ava, 
                               it = 500, #dados$it, 
                               rich = dados$rich, 
                               cwm = dados$cwm, 
                               rao = dados$cwm,
                               phi = 1,
                               prefix = "New"
)
resSIM0
class(resSIM0)
str(resSIM0)
resSIM0$simulation$group %>% class()
resSIM0$simulation$composition %>% class()
resSIM0$simulation$composition
resSIM0$simulation$composition %>% head
resSIM0$simulation$group
resSIM0$simulation$group %>% head
resSIM0$simulation$group %>% dim
resSIM0$simulation$baseline


### Simulate communities starting with existing species ----
resSIM1 <- simulateCommunities(trait = dados$trait,
                               ava = dados$ava,
                               und = dados$und,
                               it = 500, # dados$it,
                               rich = c(10, 12),
                               cwm = dados$cwm,
                               rao = dados$cwm,
                               restComp = dados$rest,
                               restGroup = dados$restGroup,
                               # max_add = dados$max_add, 
                               # min_p = dados$min_p,
                               method = "proportions",
                               phi = 1, 
                               prefix = "Ongoing"
)

rowSums(dados$rest)
resSIM1
class(resSIM1)
str(resSIM1)
resSIM1$simulation$group %>% class()
resSIM1$simulation$composition %>% class()
resSIM1$simulation$composition
resSIM1$simulation$composition %>% head
resSIM1$simulation$group
resSIM1$simulation$group %>% head
resSIM1$simulation$group %>% dim

### Merge simulation ----
allSIM <- mergeSimulations(resSIM0, resSIM1)
allSIM 
class(allSIM)
str(allSIM)
allSIM$simulation$composition %>% dim
allSIM$simulation$group %>% head
allSIM$simulation$group %>% dim

## Calculate parameters ----
### Basic parameters ----

#### Merged simulation ----
resParAllSIM <- computeParameters(allSIM, 
                                    trait = dados$trait, 
                                    cwm = dados$cwm,
                                    rao = dados$cwm,
                                    # cwv = dados$cwm,
                                    ava = dados$ava,
                                    # ref = dados$ref[1:10,],
                                    supplementary = dados$ref[11:19,]
)
resParAllSIM
class(resParAllSIM)
str(resParAllSIM, 2)

#### Non merged simulation ----
resParSIM0 <- computeParameters(resSIM0, 
                   trait = dados$trait, 
                   cwm = dados$cwm,
                   rao = dados$cwm,
                   cwv = dados$cwm,
                   ava = dados$ava,
                   ref = dados$ref[1:10,],
                   supplementary = dados$ref[11:19,]
)
resParSIM0
class(resParSIM0)
str(resParSIM0, 2)
resParAllSIM$simulation$composition
resParAllSIM$simulation$group
resParAllSIM$simulation$results

resParAllSIM$reference$composition
resParAllSIM$reference$results

resParAllSIM$supplementary$composition
resParAllSIM$supplementary$results


### Multifunctionality ----
resCheck$reference$summary
head(resParAllSIM$simulation$results)

target <- c("CWM_LMA > 0.105", "rao > 2.9", "CWM_Resprouter < 0.76")
target <- c("CWM_LMA > 0.105")
target <- c("richness>10", "unavailable<10", "CWM_LMA > 0.9", "CWM_Dur_flowering > 4", "rao > 2.9", "CWM_Resprouter < 0.76")

# TAREFA ----
# conferir se colunas foram encontradas
# calcular multi das referencia tambem
resParAllSIM <- computeMultifunctionality(resParAllSIM,
                            tests = target)
resParAllSIM
head(resParAllSIM$simulation$results)
head(resParAllSIM$simulation$multifunctionality)
head(resParAllSIM$simulation$multifunctionality)
dim(resParAllSIM$simulation$multifunctionality)
resParAllSIM$simulation$results

### Dissimilarity ----
resParAllSIM <- computeDissimilarity(resParAllSIM, 
                                       dados$trait[,1:2])
resParAllSIM$simulation$results

# TAREFAS ----
# incluir dissimilaridade entre comunidades selecionadas

# Select communities ----

### Global selection (first step) ----
targetSelect1 <- c("CWM_LMA > 0.08", "rao > 2.9", "CWM_Resprouter > 0.5")
resSelectSim <- selectCommunities(resParAllSIM, 
                                  testsDet = targetSelect1)
resSelectSim
class(resSelectSim)
resSelectSim$selection$results %>% dim
resSelectSim$selection$results
resSelectSim$selection$N
resSelectSim$selection$thresholds

### Additional selection (second step) ----
targetSelect2 <- c("CWM_LMA > 0.08", "rao < 3", "CWM_Resprouter > 0.5")
resSelectSim <- selectCommunities(resSelectSim, 
                                  testsDet = targetSelect2)
resSelectSim
str(resSelectSim,1)
resSelectSim$selection$group %>% dim
resSelectSim$selection$composition
resSelectSim$selection$results
resSelectSim$selection$N
resSelectSim$selection$thresholds

### Global selection with dissimilarity and/or multifunctionality ----
# targetSelect3 <- c("multifunctionality >= 2", "dissimilarity < 0.1")
targetSelect3 <- c("alphamultifunctionality >= 2")
resSelectSim3 <- selectCommunities(resParAllSIM, 
                                   testsDet = targetSelect3)
resSelectSim3
resSelectSim3$selection$results
resSelectSim3$selection$N
resSelectSim3$selection$thresholds

## Merge selections ----
### Selections ----
resParAllSIM$simulation$results
targetSelect4 <- c("PREFIX == 'Ongoing'", "restGroup == 'nonEdge'", "rao > 3.6")
resSelectSimPart1 <- selectCommunities(resParAllSIM, 
                                       testsDet = targetSelect4)
resSelectSimPart1
resSelectSimPart1$selection$results
resSelectSimPart1$selection$N

resParAllSIM$simulation$group
targetSelect5 <- c("PREFIX == 'Ongoing'", "restGroup == 'edge'", "rao > 3.6")
resSelectSimPart2 <- selectCommunities(resParAllSIM, 
                                       testsDet = targetSelect5)
resSelectSimPart2
resSelectSimPart2$selection$results
resSelectSimPart2$selection$results
resSelectSimPart2$selection$N

### Merge ----
resSelectSimMerged <- mergeSelection(resSelectSimPart1, resSelectSimPart2)
resSelectSimMerged
resSelectSimMerged$selection$group %>% dim
resSelectSimMerged$selection$composition %>% dim
resSelectSimMerged$selection$results %>% dim

## Extras ----

### Selection then compute dissimilarity and multifunctionality ----

#### First selection ----
targetSelect6 <- c("rao > 5")
resParSelectExtra <- selectCommunities(resParSIM0, 
                                       testsDet = targetSelect6)
resParSelectExtra
resParSelectExtra$selection$N
resParSelectExtra$selection$results

#### Dissimilarity ----
resParSelectExtra <- computeDissimilarity(resParSelectExtra, dados$trait[,1:2])
resParSelectExtra
resParSelectExtra$selection$results

#### Multifunctionality ----
targetMulti <- c("CWM_Height > 50", "CWM_Resprouter > 0.05")
resParSelectExtra <- computeMultifunctionality(resParSelectExtra, tests = targetMulti)
resParSelectExtra
resParSelectExtra$selection$multifunctionality
resParSelectExtra$selection$results

#### Additional selection (second step) ----
targetSelect7 <- c("alphamultifunctionality >= 2")
resParSelectExtra <- selectCommunities(resParSelectExtra, 
                                       testsDet = targetSelect7)
resParSelectExtra
resParSelectExtra$selection$results
resParSelectExtra$reference$results
resParSelectExtra$supplementary$results

## Plots ----
simulateCommunities
### View results
viewResults(resParAllSIM, "CWM_LMA", "richness")
# viewResults(resParAllSIM, "CWM_LMA", "restGroup")
viewResults(resSelectSim, "CWM_LMA", "richness")
# viewResults(resSelectSimMerged, "CWM_LMA", "richness")

### View multifunctionality results
resParAllSIM

# Ver de mudar as cores das barras mo grafico
head(resParAllSIM$simulation$multifunctionality)

colSums(resParAllSIM$simulation$multifunctionality)
viewMultifunctionality(resParAllSIM)
viewMultifunctionality(resParAllSIM, min_degree = 3, max_degree = 6, mode = "exclusive_intersection")
viewMultifunctionality(resParAllSIM, min_degree = 3, max_degree = 6, mode = "inclusive_intersection")

# Ver se tem como tirar a linha ----

# CONFERIR ----
# viewMultifunctionality(resParSelectExtra)

# save.image("CCC_workspace_20240829")
# load("CCC_workspace_20240829")

resParAllSIM_TESTE <- computeParameters(allSIM, 
                                        trait = dados$trait, 
                                        # cwm = dados$cwm,
                                        rao = dados$cwm,
                                        # cwv = dados$cwm,
                                        # ava = dados$ava,
                                        ref = dados$ref[1:10,],
                                        supplementary = dados$ref[11:19,]
)
resParAllSIM_TESTE$simulation$results


resParAllSIM_TESTE2 <- computeParameters(allSIM, 
                                         trait = dados$trait, 
                                         # cwm = dados$cwm,
                                         rao = list(dados$cwm, c("LMA", "Zooc")),
                                         # cwv = dados$cwm,
                                         # ava = dados$ava,
                                         ref = dados$ref[1:10,],
                                         supplementary = dados$ref[11:19,]
)
resParAllSIM_TESTE2$simulation$results

listarao <- list(dados$cwm, c("LMA", "Zooc"))
names(listarao) <- c("Tudo", "LMAZooc")
listarao
resParAllSIM_TESTE3 <- computeParameters(resSIM1, 
                                         trait = dados$trait, 
                                         # cwm = dados$cwm,
                                         rao = listarao,
                                         # cwv = dados$cwm,
                                         # ava = dados$ava,
                                         ref = dados$ref[1:10,],
                                         supplementary = dados$ref[11:19,]
)
resParAllSIM_TESTE3$simulation$results
# NEW ----
require(resbiota)
data("cerrado.mini")
head(cerrado.mini$traits)
# Simulation
scenario <- simulateCommunities(trait = cerrado.mini$traits,
                                ava = "Available",
                                cwm = "BT",
                                rao = c("SLA", "Height", "Seed"),
                                rich = c(10, 15),
                                it = 100)
scenario
# Compute functional parameters
scenario <- computeParameters(x = scenario,
                              trait = cerrado.mini$traits,
                              ava = "Available",
                              cwm = "BT",
                              rao = c("SLA", "Height", "Seed"),
                              cost = "Cost",
                              dens = "Density",
                              reference = cerrado.mini$reference,
                              supplementary = cerrado.mini$supplementary)
scenario
scenario$simulation$results

# Select communities - Deterministic selection
scenarioSelected <- selectCommunities(x = scenario,
                                      testsDet = c("CWM_BT > 6",
                                                   "rao > 2.5"))
scenarioSelected
# Select communities - Hierarchical selection
scenarioSelected <- selectCommunities(x = scenario,
                                      testsHie = c("CWM_BT > 6",
                                                   "rao > 2.5",
                                                   "cost == 'MIN'"))
scenarioSelected
data("cerrado.mini")
sum(cerrado.mini$traits$Available)
scenarioB <- simulateCommunities(trait = cerrado.mini$traits,
                                 restComp = cerrado.mini$restoration,
                                 ava = "Available",
                                 cwm = "BT",
                                 rao = c("SLA", "Height", "Seed"),
                                 rich = 18,
                                 it = 10,
                                 # max_add = 10,
                                 method = "ind",
                                 nInd = 1000,
                                 prob = "Density")
scenarioB$simulation$composition

scenarioB <- computeParameters(x = scenarioB,
                               trait = cerrado.mini$traits,
                               ava = "Available",
                               cwm = "BT",
                               rao = c("SLA", "Height", "Seed"),
                               cost = "Cost",
                               dens = "Density",
                               reference = cerrado.mini$reference,
                               supplementary = cerrado.mini$supplementary)
scenarioB
str(scenarioB)
colnames(scenarioB$simulation$results)
scenarioB$simulation$results

class(scenarioB$simulation$results$unavailable)

all(scenarioB$simulation$results$unavailable == floor(scenarioB$simulation$results$unavailable))
y <- 1:10
y <- rep(NA, 10)
y <- c(NA, 1:10)
y 

all(y == floor(y), na.rm = TRUE)
all(is.na(y))


# Select communities - Hierarchical selection
scenarioSelectedB <- selectCommunities(x = scenarioB,
                                       testsHie = c("CWM_BT > 6",
                                                    "rao > 2.5"),
                                       group = "NAME",
                                       singleselection = TRUE)
scenarioSelectedB

scenarioSelectedB$selection$results
scenarioSelectedB$selection$composition

scenarioSelectedB2 <- selectCommunities(x = scenarioB,
                                       testsDet = c("CWM_BT > 6",
                                                    "rao > 2.5"),
                                       group = "NAME")
scenarioSelectedB2
scenarioSelectedB2$selection$results




selectCommunities(scenarioA,
                  testsDet = c("CWM_BT >= 8"))$selection$results
selectCommunities(scenarioA,
                  testsDet = c("CWM_BT >= 8", "richness < 20"))$selection$results
selectCommunities(scenarioA,
                  testsDet = c("CWM_BT >= 8", "richness < 20"),
                  testsHie = c("CWM_BT == 'MAX'"))$selection$results


selectCommunities(scenarioB,
                  testsDet = c("CWM_BT >= 8"))$selection$results
selectCommunities(scenarioB,
                  testsDet = c("CWM_BT >= 8", "richness > 28"))$selection$results
selectCommunities(scenarioB,
                  testsHie = c("CWM_BT >= 8", "richness < 20"))$selection$results
selectCommunities(scenarioB,
                  testsHie = c("CWM_BT >= 8", "richness > 20", "cost == 'MIN'"),
                  group = "NAME")$selection$results


## ABUNDANCIAs
data("cerrado.mini")
head(cerrado.mini$traits)
# Restoration new sites
scenarioA <- simulateCommunities(trait = cerrado.mini$traits,
                                 # ava = "Available",
                                 cwm = c("Height", "Seed"),
                                 rao = c("SLA", "Height", "Seed"),
                                 rich = c(10, 15),
                                 it = 100)
scenarioA
# Restoration existing sites
cerrado.mini$traits$Tipo <- rep(c("A", "B"), each = 25)
cerrado.mini$traits
scenarioB <- simulateCommunities(trait = cerrado.mini$traits,
                                 # restComp = cerrado.mini$restoration,
                                 # ava = "Available",
                                 # cwm = "BT",
                                 # rao = c("SLA", "Height", "Seed"),
                                 rich = c(10, 15),
                                 it = 4000,
                                 # nInd = 1000,
                                 method = "pro",
                                 # prob = "SLA",
                                 group = "Tipo",
                                 # probGroupRich = c("A" = 0.5, "B" = 0.5),
                                 probGroupAbund = c("A" = 0.5, "B" = 0.5)
                                 )
sum(scenarioB$simulation$composition[,rownames(cerrado.mini$traits)[1:25]])
sum(scenarioB$simulation$composition[,rownames(cerrado.mini$traits)[26:50]])

cerrado.mini$traits$Density
scenario <- simulateCommunities(trait = cerrado.mini$traits,
                                 # restComp = cerrado.mini$restoration,
                                 # ava = "Available",
                                 # cwm = "BT",
                                 # rao = c("SLA", "Height", "Seed"),
                                 rich = c(50),
                                 it = 4,
                                 nInd = 1000,
                                 method = "ind",
                                 # prob = "SLA",
                                 group = "Density",
                                 # probGroupRich = c("2777" = 0, "10000" = 0.5),
                                 probGroupAbund = c("2777" = 0, "10000" = 0.5)
)
nrow(scenario$simulation$composition)


plot(apply(scenarioB$simulation$composition, 2, sum), cerrado.mini$traits$SLA)
plot(apply(scenarioB$simulation$composition, 2, sum), cerrado.mini$traits$Density)


cerrado.mini$traits$Tipo <- factor(cerrado.mini$traits$Tipo)
str(cerrado.mini$traits)
scenario <- computeParameters(x = scenarioB,
                              trait = cerrado.mini$traits,
                              ava = "Available",
                              cwm = "BT",
                              rao = c("SLA", "Height", "Seed"),
                              cost = "Cost",
                              dens = "Density",
                              reference = cerrado.mini$reference,
                              supplementary = cerrado.mini$supplementary)
scenario
scenario$simulation$results
# END ----