#' @title Cerrado biome species trait and community composition datasets
#' @name cerrado
#' @docType data
#' @encoding UTF-8
#' @aliases cerrado.mini
#' @description The \code{cerrado} dataset includes functional traits for 483 species used in restoration projects in the savannas of the Cerrado biome. The \code{cerrado.mini} dataset provides a random set of 50 species for testing and demonstration purposes.
#' @format Both datasets arranged as lists:
#' \describe{
#'   \item{traits}{A data frame object with traits data. The data frame contains four traits: BT (bark thickness), SLA (specific leaf area), Height (maximum height) and Seed (seed mass). The column Available indicates if species are available on the market, the column Cost is the cost per individual, and the column Density is the planting density for each species.}
#'   \item{reference}{A community matrix contains relative abundances for 118 species in 6 reference sites. The species are arranged as columns and sites in rows.}
#' }
#' @usage data(cerrado)
#' @keywords datasets
#' @references 
#' Coutinho, A. G., Carlucci, M. B., & Cianciaruso, M. V. (2023). A framework to apply trait-based ecological 
#' restoration at large scales. Journal of Applied Ecology, 60, 1562–1571. https://doi.org/10.1111/1365-2664.14439
NULL