rm(list = ls())
require(devtools)
require(rmarkdown)
devtools::document()
devtools::build_vignettes()
# devtools::clean_vignettes()
# rmarkdown::render("vignettes/Vignette-Title1.Rmd", 
#                   output_format = rmarkdown::pdf_document(keep_tex = FALSE))
setwd("CCC")

setwd("..")
system("R CMD build CCC")
# system("R CMD build --no-build-vignettes")
# system("R CMD INSTALL CCC")
# system("R CMD check CCC")
remove.packages("CCC")

system("R CMD INSTALL CCC_0.0.03.tar.gz")
system("R CMD check --as-cran CCC_0.0.03.tar.gz")
system("R CMD check CCC_0.0.03.tar.gz")

require(CCC)
?CCC::findSpecies
