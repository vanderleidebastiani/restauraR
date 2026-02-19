rm(list = ls())
require(rmarkdown)
require(devtools)
# install_version("Select", version = "1.4", repos = "http://cran.us.r-project.org")
# remove.packages("ggplot2")
# install_version("ggplot2", version = "3.5.2", repos = "http://cran.us.r-project.org")

setwd("resbiota")
devtools::document()
devtools::build_manual()
# devtools::build_vignettes()
# devtools::clean_vignettes()
# rmarkdown::render("vignettes/Framework-application.Rmd",
#                   output_format = rmarkdown::pdf_document(keep_tex = FALSE))

# system("R CMD build .")
setwd("..")
system("R CMD build resbiota")
# system("R CMD build resbiota --no-build-vignettes")
# system("R CMD INSTALL resbiota")
# system("R CMD check resbiota")
# remove.packages("resbiota")

system("R CMD INSTALL resbiota_0.0.5.tar.gz")
system("R CMD check --as-cran resbiota_0.0.5.tar.gz")
# system("R CMD check resbiota_0.0.5.tar.gz")

require(resbiota)
?resbiota::simulateCommunities
runResbiota()
browseVignettes("resbiota")