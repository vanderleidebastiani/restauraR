require(CCC)
require(magrittr)

data("dados")
ls(dados)
args(comSimulation)

RES0 <- CCC::comSimulation(dados$trait, 
                           ava = dados$ava, 
                           it = dados$it, 
                           rich = dados$rich, 
                           cwm = dados$cwm, 
                           rao = dados$cwm,
                           phi = 1)
RES0
RES0$sim$parameters
RES0$sim$composition 
RES0$sim$composition %>% dim

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
                           phi = 1)
RES1
RES1$sim$parameters
RES0$sim$composition 
RES0$sim$composition %>% dim