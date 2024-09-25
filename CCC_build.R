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
# system("R CMD build --no-build-vignettes")
system("R CMD INSTALL CCC")
# system("R CMD check CCC")

remove.packages("CCC")

system("R CMD INSTALL CCC_0.0.03.tar.gz")
system("R CMD check --as-cran CCC_0.0.03.tar.gz")
system("R CMD check CCC_0.0.03.tar.gz")

require(CCC)
?CCC::findSpecies
