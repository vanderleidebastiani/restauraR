require(devtools)
devtools::document()
setwd("CCC")

require(CCC)

CCC::disCalculation()
setwd("..")
system("R CMD build CCC")
system("R CMD INSTALL CCC")
remove.packages("CCC")

# system("R CMD check SYNCSA_1.3.5.tar.gz")
# system("R CMD check --as-cran SYNCSA_1.3.5.tar.gz")
# system("R CMD check SYNCSA")
