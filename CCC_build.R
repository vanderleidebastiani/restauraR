rm(list = ls())
require(devtools)
devtools::document()
setwd("CCC")


require(CCC)
data("dados")


setwd("..")
system("R CMD build CCC")
system("R CMD INSTALL CCC")
remove.packages("CCC")

system("R CMD check CCC_0.0.01.tar.gz")
# system("R CMD check --as-cran XXXX_0.0.0.tar.gz")
# system("R CMD check XXX")

# restore - Disponível, mas tem RestoreNet
# SSRES - Disponível, mas tem alguns ssr
# REBUILD - Disponível
# RESTAURA - Disponível