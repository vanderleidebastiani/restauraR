rm(list = ls())
require(devtools)
devtools::document()
devtools::build_vignettes()
devtools::clean_vignettes()
setwd("CCC")

require(CCC)
data("dados")

setwd("..")
system("R CMD build CCC")
system("R CMD INSTALL CCC")
remove.packages("CCC")

system("R CMD check CCC_0.0.01.tar.gz")
system("R CMD INSTALL CCC_0.0.03.tar.gz")
system("R CMD check --as-cran CCC_0.0.02.tar.gz")
# system("R CMD check XXX")

require(CCC)
?CCC::findSpecies