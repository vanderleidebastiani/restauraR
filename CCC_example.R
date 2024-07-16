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

data("dados")
ls(dados)
args(comSimulation)

head(dados$trait)

checkReference(dados$ref,
               trait = dados$trait,
               cwm = dados$cwm[c(1,6)], 
               rao = dados$cwm)


RES0 <- comSimulation(dados$trait[70:110,], 
                           ava = dados$ava, 
                           it = dados$it, 
                           rich = dados$rich, 
                           cwm = dados$cwm, 
                           rao = dados$cwm,
                           phi = 1,
                          # prefix = "New"
                      )
RES0
# RES0$sim$parameters
RES0$sim$composition
RES0$sim$composition %>% head
RES0$sim$composition
RES0$sim$restGroup
RES0$sim$restGroup %>% head

dados$rest %>% dim
dados$restGroup %>% dim

RES1 <- comSimulation(trait = dados$trait[80:120,],
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
                           # prefix = "Ongoing"
                      )
RES1
# RES1$sim$parameters
RES1$sim$composition %>% head
RES1$sim$composition %>% dim
# RES1$sim$restName
# RES1$sim$prefix
RES1$sim$restGroup
RES1$sim$restGroup %>% head

colnames(dados$rest)
head(dados$rest)
# ref <- dados$ref[,1:22]
# ref <- ref[, !colSums(ref)==0]
allSim <- mergeSimulations(RES0, RES1)
# allSim1 <- CCC::mergeSimulations(RES0, RES1, ref = ref)
allSim$sim$composition %>% dim
allSim$sim$restGroup %>% head
# allSim1$sim$composition %>% dim

RES0$sim$prefix

allSim$sim$composition
# CONFERIR calcPar quando agrupada
resPar0 <- calcPar(allSim, 
                   trait = dados$trait, 
                   cwm = dados$cwm,
                   rao = dados$cwm,
                   cwv = dados$cwm,
                   ava = dados$ava,
                   ref = dados$ref[1:10,],
                   supplementary = dados$ref[11:19,]
                   )
str(RES1, 2)
resPar1 <- calcPar(RES1, 
                   trait = dados$trait, 
                   cwm = dados$cwm,
                   rao = dados$cwm,
                   cwv = dados$cwm,
                   ava = dados$ava,
                   ref = dados$ref[1:10,],
                   supplementary = dados$ref[11:19,]
                   )

resPar0$sim$composition
resPar0$sim$results
resPar0$ref$composition
resPar0$ref$results
resPar0$supplementary$composition
resPar0$supplementary$results

resPar1$sim$composition
resPar1$sim$results
resPar1$ref$composition
resPar1$ref$results
resPar1$supplementary$composition
resPar1$supplementary$results

RES1$sim$restGroup

# multi aqui, antes da selecao
resPar0$sim$results %>% head
# thresholds if you have calculated CWM for a single trait, for example,
thresholds1 <- c("Zooc > 0.02", "rao > 3.7", "Resprouter < 8 ")

resPar0 <- multCalculation(resPar0, tests = thresholds1)
resPar0$sim$multifunctionality


# thresholds1 <- c("Zooc > 0.02", "rao > 3.7", "restName == 'X1' ")

selSim <- comSelection(resPar1, tests = thresholds1)
selSim$selection$results

selSim$selection$N
selSim$selection$thresholds
selSim$selection$results
selSim$selection$composition


selSim <- disCalculation(selSim, trait)
selSim$sim$results

tests <- c("unavailable > 12.6", "richness < 14.6", "LMA > 0.06983576")

thresholds2 <- c("dissim < 0.2")
thresholds <- c(thresholds1, thresholds2)
selSim2 <- comSelection(selSim,
                        test = thresholds2,
                        where = "global")
selSim2$selection$N
selSim2$selection$composition[, !colSums(selSim2$selection$composition)==0]

# multCalculation(selSim2$selection$results, th)

