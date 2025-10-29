rm(list = ls())
require(devtools)
require(rmarkdown)

#setwd("resbiota")
devtools::document()
devtools::build_vignettes()
# devtools::clean_vignettes()
# rmarkdown::render("vignettes/Framework-application.Rmd",
#                   output_format = rmarkdown::pdf_document(keep_tex = FALSE))

system("R CMD build .")
#setwd("..")
#system("R CMD build resbiota")
# system("R CMD build --no-build-vignettes")
# system("R CMD INSTALL resbiota")
# system("R CMD check resbiota")
# remove.packages("resbiota")

system("R CMD INSTALL resbiota_0.0.4.tar.gz")
# system("R CMD check --as-cran resbiota_0.0.2.tar.gz")
system("R CMD check resbiota_0.0.4.tar.gz")

require(resbiota)
?resbiota::simulateCommunities
runResbiota()



