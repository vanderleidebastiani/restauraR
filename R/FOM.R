#' @title Functional traits and community data from Araucaria moist forests
#' @name FOM
#' @docType data
#' @encoding UTF-8
#' @description The \code{FOM} dataset contains species functional traits for 124 species of the Araucaria moist forests, officially classified as mixed ombrophilous forest (Portuguese: "Floresta Ombrófila Mista - FOM") in the southern portion of the Atlantic Forest Biome in Brazil. 
#' @format The dataset is arranged as a list:
#' \describe{
#'   \item{traits}{A data frame object with functional traits data. The data frame contains two traits: `Zoochory` and `WoodDensity`. Furthermore, the column `EcologicalSuccession` indicates the successional groups of species and the column `relativeAbundance` indicates the species abundances in the regional pool. }
#'   \item{referenceSites}{A community matrix contains raw abundances for 60 species across 2 reference sites. The species are arranged as columns and sites in rows.}
#'   \item{restorationSitesComp}{A community matrix contains raw abundances for 19 species across 2 restoration sites. The species are arranged as columns and sites in rows.}
#'   \item{restorationSitesInfo}{A data frame   contains additional information for each restoration site. The columns `nSppMin`, `nSppMax` and `nIndividuals` represent site-specific restoration targets.}
#'   \item{cooccurrenceProb}{A matrix contains co-occurrence probabilities for all 124 species.}
#' }
#' @usage data(FOM)
#' @keywords datasets
#' @references 
#' The Forest and Floristic Inventory of Santa Catarina (IFFSC)
NULL