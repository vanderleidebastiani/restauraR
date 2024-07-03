require(CCC)
require(magrittr)

data("dados")
ls(dados)
args(comSimulation)

head(dados$trait)

RES0 <- CCC::comSimulation(dados$trait, 
                           ava = dados$ava, 
                           it = dados$it, 
                           rich = dados$rich, 
                           cwm = dados$cwm, 
                           rao = dados$cwm,
                           phi = 1,
                           prefix = "New")
RES0
RES0$sim$parameters
RES0$sim$composition
RES0$sim$composition %>% dim

dados$rest %>% dim
RES1 <- CCC::comSimulation(trait = dados$trait[80:120,],
                           ava = dados$ava,
                           und = dados$und,
                           it = dados$it,
                           rich = c(10,12),
                           cwm = dados$cwm,
                           rao = dados$cwm,
                           rest = dados$rest,
                           max_add = dados$max_add, 
                           min_p = dados$min_p,
                           phi = 1, 
                           prefix = "Ongoing")
RES1
RES1$sim$parameters
RES1$sim$composition
RES1$sim$composition %>% dim

str(RES1)
head(dados$ref[,1:22])
dados$ref[,1:22] %>% dim

colSums(dados$ref[,1:22])==0

ref <- dados$ref[,1:22]
ref <- ref[, !colSums(ref)==0]
allSim <- CCC::mergeSimulations(RES0, RES1)
# allSim1 <- CCC::mergeSimulations(RES0, RES1, ref = ref)

allSim$sim$composition %>% dim
# allSim1$sim$composition %>% dim


resPar0 <- calcPar(allSim, 
                   trait = dados$trait, 
                   cwm = dados$cwm,
                   rao = dados$cwm,
                   cwv = dados$cwm,
                   ava = dados$ava)

resPar1 <- calcPar(allSim, 
                   trait = dados$trait, 
                   cwm = dados$cwm,
                   rao = dados$cwm,
                   cwv = dados$cwm,
                   ava = dados$ava,
                   ref = dados$ref[1:10,],
                   supplementary = dados$ref[11:19,])

resPar1$sim$composition
resPar1$sim$results
resPar1$ref$composition
resPar1$ref$results
resPar1$supplementary$composition
resPar1$supplementary$results


# thresholds if you have calculated CWM for a single trait, for example,
thresholds1 <- c("Zooc > 0.02", "rao > 5")

selSim <- comSelection(resPar1, tests = thresholds1)
selSim

selSim$selection$N
selSim$selection$thresholds
selSim$selection$results
selSim$selection$composition

selSim <- disCalculation(selSim, trait)
selSim$sim$results


thresholds2 <- c("dissim < 0.2")
thresholds <- c(thresholds1, thresholds2)
selSim2 <- comSelection(selSim,
                        test = thresholds2,
                        where = "global")
selSim2$selection$N
selSim2$selection$composition[, !colSums(selSim2$selection$composition)==0]

# multCalculation(selSim2$selection$results, th)

