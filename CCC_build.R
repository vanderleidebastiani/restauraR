require(devtools)
devtools::document()
setwd("CCC")

require(CCC)

setwd("..")
system("R CMD build CCC")
system("R CMD INSTALL CCC")
remove.packages("CCC")

# system("R CMD check XXX_1.3.5.tar.gz")
# system("R CMD check --as-cran XXXX_0.0.0.tar.gz")
# system("R CMD check XXX")
