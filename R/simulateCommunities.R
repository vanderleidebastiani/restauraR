#' @title Generate simulated communities to restoration
#' @description Generates simulated communities.
#' @details 
#' \strong{The framework}
#' 
#' The framework generates communities with distinct species compositions drawn at random from a species pool. The algorithm takes as input a data set with trait information for the species in the regional pool, that is, a list of species from which trait data are available that could be used to restore a particular ecosystem. Using species from the regional pool, the algorithm produces a set of N-simulated communities with species richness that ranges within a user-defined interval. 
#' 
#' \strong{Traits and additional species information}
#' 
#' Each species in the pool can be described by functional traits that represent the ecological functions of communities, ecosystem services, or desirable characteristics for novel or restored communities. Those species can be classified into species that are available on the market and those that are not. Furthermore, the cost per individual, observed frequencies occurring in reference sites, and any additional information can be used to select species composition.
#' 
#' \strong{Simulations}
#' 
#' The species assemblages are generated in the \code{simulateCommunities} function. The species richness within simulated communities varies within a range defined by the user. Thus, it could be specified using the observed range in reference communities or desired restoration targets. 
#' 
#' The sampling of species can be performed completely at random or selected species based on functional traits or a desired trait profile. If the cwm argument is set the species selections are constrained to specific community-weighted mean trait values, using the entire range of each trait. If the rao argument is set, the species selections are performed in order to optimize the functional diversity of a specific set of traits.
#' 
#' To obtain a high number of restoration solutions, all runs of the \code{simulateCommunities} function return a set of simulated communities with random species distribution, and composition based on rules set with cwm and/or rao arguments. Furthermore, if the species available is informed in the ava argument, the sample of species compositions is performed also with only available species on the market. Thus, the function returns a wide range of variability in composition to posterior selection, including compositions with available and not available species in the market.
#' 
#' The framework returns a community matrix indicating the proportion of individuals that need to be added for each species. If no established communities are informed, the simulated communities are set as empty communities (sites to restore start with no species, and all species must be planted for restoration). Alternatively, if established communities are set, the new species and individuals are introduced into the established communities (sites to restore can start with pre-existing species). Thus, it is possible to increase the number of ecosystem functions in ongoing restoration sites and follow an approach of adaptive management.
#' 
#' \strong{Communities parameters}
#' 
#' After the initial step of species composition simulation, several function parameters can be calculated for each community. In the resbiota package, this step is mandatory, given that those parameters are used to select a subset of simulated communities that fit the restoration target. Parameters such as species richness, Functional Diversity (FD), Community Weighted Means (CWM), the average functional dissimilarity between each simulated community, and restoration cost can be calculated. In general, those function parameters as used as a proxy for ecosystem functions relevant to the restoration under evaluation. The functions \code{computeParameters} and \code{computeMultifunctionality} can be used in this step. 
#' 
#' The function \code{computeParameters} is mandatory and allows compute the basic parameters:  richness, Community Weighted Mean, Community Weighted Variance, Rao Quadratic Entropy and functional dissimilarity. The user must define which traits will be used to calculate each metric. Additionally, if are provided the species cost per individual and information about planting density for each species, the restoration costs are estimated. When the availability of species is specified, the function counts species unavailable in each community. Furthermore, it is optional to compute functional dissimilarity between selected communities and reference sites. Functional dissimilarity is a pair-wise metric between each selected community and each reference site. If you have more than one reference site, functional dissimilarity will be the average dissimilarity between each selected community and the reference sites. Functional dissimilarity is calculated with the function discomQE from the package adiv using the default formula “QE”. Therefore, it corresponds to the Rao’s dissimilarity between communities. 
#' 
#' The function \code{standardizeParameters} allows the standardisation of the calculated parameters. Two methods are available: "max", which divides the values by the maximum, and "standardise", which scales the calculated parameters to zero mean and unit variance. For example, the "max" method is useful to scale dissimilarity results by scaling the values in ranges from 0 (no dissimilarity) to 1 (maximum dissimilarity).
#' 
#' The function \code{computeMultifunctionality} is optional and computes the matrix of multifunctionality and alpha multifunctionality. See the multifunctionality topic below.
#' 
#' \strong{Select communities}
#' 
#' The selections of simulated communities are performed using the function \code{selectCommunities}. Selections are based on simple logical tests based on thresholds. The user must define parameter thresholds, that indicate the level of functional composition above (or below) which the function is considered to be restored in each site. The thresholds can be set of one or more calculated parameters in the previous step. This is an essential step that must be based on previous studies that indicate the relationship between species traits and ecosystem functions related to the desired restoration targets. Knowledge of the regional species pool, species availability and reference sites are important to find solutions that adequately optimize restoration goals.
#' 
#' The logical tests are specified using a vector of character class. Each test uses the parameter name, a logical operator and the value threshold. All basic logical tests can be used, setting the default operators: <, <=, >, >=, == and !=.  Each test must be in double or single quote signs. The selection of character type can be performed using different quotation marks than those used to define the entire test (e.g., single quote inside the double quote).
#' 
#' Two selection options can be used, individually or alternatively, based on deterministic and hierarchical selection. In the deterministic selection, all simulations that are true in the input test are returned. In this case, all parameters evaluated must be true to a community to be selected. Multiple selection steps can be performed sequentially, using more restrictive criteria (although the practical results will be the same when applied in a single step with the most restrictive criterion). In the hierarchical selection, the tests are evaluated hierarchically and only one simulation is selected. When all simulations fail in the first test, the function tries the next test. In the end, the function samples only one simulation among those which passed the all tests. In this case, an additional argument defines if the selection is performed inside a specific group. This allows, for example, the selection of one simulation for each site that will be restored. Furthermore, the hierarchical selection allows the use of two special arguments to select, the word MIN to select the minimal value and the word MAX to select the maximum value in a specific parameter.
#' 
#' \strong{The reference sites}
#' 
#' Reference sites can be included in \code{computeParameters} step. Thus, all calculated parameters also are calculated to reference sites and can be used in posterior analysis or auxiliary to the selection procedure by identifying the natural range and values of these parameters in the reference ecosystem. 
#' 
#' The resbiota package also allows the inclusion of supplementary sites to calculate the same set of functional parameters. These supplementary sites are only used to compare proposals, for example, to evaluate the parameters in naturally regenerated species same sites.
#' 
#' \strong{The outputs}
#' 
#' The main outputs are a community matrix with species relative abundances and a data frame with the functional parameters calculated for each community. These outputs allow the user to investigate the relationship between these parameters, select communities that meet the restoration goals, and identify functionally important species that are not available on the market. 
#' 
#' \strong{Multifunctionality}
#' 
#' The restoration projects can target restoring multiple ecosystem services, called multifunctionality. Thus, different restoration objectives should be assigned to distinct sites within a restoration landscape. However, when dealing with multiple functions, trade-offs are likely to arise, whereby the pursuit of one function may prevent the achievement of another. In the resbiota package, the matrix of multifunctionality can be calculated using simple logical tests in each available functional parameter. Thus, the multifunctionality of each restoration site is defined as the number of functions above (or below) a given threshold. The sum of individual tests is defined as the alpha multifunctionality metric.
#' 
#' The selection of simulated species composition can be performed in alpha multifunctionality metric to maximize multifunctionality between restored sites. The alpha multifunctionality also allows the selection of a simulated community when no solution satisfies all initial criteria, thus, the users can use a less restricted solution.
#' 
#' High values of alpha multifunctionality can be a preferable criterion for selection, meaning that, for each restoration site, the selection is performed in order to return a solution with the highest number of ecosystem functions recovered. 
#' 
#' \strong{Visualization}
#' 
#' The results visualization functions are auxiliary to checking parameters in reference sites, analyzing the distribution of functional parameters in simulated communities and showing final solutions of selected species composition. 
#' 
#' The function \code{viewResults} allows the visualization of basic results using scatter plots to show the trade-offs between several parameters, restoration costs, species richness and any calculated functional parameter. This visualization includes all simulated communities and allows includes also observed relations in reference sites. 
#' 
#' The function \code{viewMultifunctionality} allows visualize the multifunctionality sets creating a graphical representation of the number of sites where each function has been restored, and the number of sites where combinations of functions were restored.
#' 
#' \strong{Merge functions}
#' 
#' The resbiota package includes functions to merge results set in different operations. 
#' 
#' The function \code{mergeSimulations} concatenates simulated communities generated under different scenarios. For example, part of restoration sites can be generated from empty communities, and in another scenario, the species composition can be generated based on established communities. Thus, both scenarios can be concatenated to subsequent steps as parameter calculations. 
#' 
#' The function \code{mergeSelection} concatenates different selection procedures. For example, if restoration sites have diverse desired restoration targets, the selection can be performed in distinct steps and then concatenated.
#' @encoding UTF-8
#' @importFrom data.table rbindlist as.data.table
#' @aliases mergeSimulations print.simRest
#' @param trait Data frame or matrix with species traits. Traits as columns and species as rows.
#' @param restComp A matrix with species proportions in the restoration sites. NAs not accepted.
#' @param restGroup Data frame or matrix with complementary information for restoration sites.
#' @param ava A vector indicating trait name which indicates the availability of species (1 or 0) in trait data.
#' @param und A vector indicating trait name which indicates undesired species (1 or 0) in trait data.
#' @param it Number of iterations (communities).
#' @param rich The range of richness values in each community.
#' @param cwm A vector with trait names to constrain Community Weighted Mean (CWM) while maximising functional diversity. Constraints are driven over the range of each trait.
#' @param rao A vector with traits names to be considered in maximize functional diversity (Rao Quadratic Entropy), or distance matrix (class "dist").
#' @param prob A vector indicating trait name which indicates the probabilities to draw individuals in each species. Used only in method "individuals".
#' @param phi A parameter bounded between 0 and 1 that weights the importance of either quadratic entropy or entropy (default phi = 1).
#' @param nInd The number of individuals to draw in each site. Used only in method "individuals".
#' @param cvAbund Coefficient of variation (CV) of the relative abundances in the species pool. Used only in method "individuals".
#' @param prefix A prefix to use in current simulation.
#' @param method Method to obtain the samples, "proportions" or "individuals" (Default method = "proportions").
#' @param group A vector with traits name which indicates the group to which species belongs.
#' @param probGroupRich Vector of probabilities to draw richness in each species group.
#' @param probGroupAbund Vector of probabilities to draw individuals or relative abundances in each species group.
#' @param minAbun Minimal abundance or proportion to keep in simulated communities. 
#' @param reallocate Reallocate removed individuals to species with some abundance (Default = reallocate FALSE)
#' @param ... Objects of class "simRest" to be concatenated. Additional arguments for respective methods.
#' @param x Objects of class "simRest" to print.
#' @returns A list (class "simRest") with the elements:
#' \item{call}{The arguments used.}
#' \item{simulation$composition}{A matrix with species composition for simulated communities.}
#' \item{simulation$group}{A data frame with complementary information for restoration sites.}
#' \item{simulation$results}{A data frame with calculated parameters in each simulated community.}
#' \item{simulation$multifunctionality}{A data frame with binary multifunctionality tests.}
#' \item{reference$composition}{A matrix with species composition for reference sites}
#' \item{reference$results}{A data frame with calculated parameters in reference sites.}
#' \item{supplementary$composition}{A matrix with species composition for supplementary sites.}
#' \item{supplementary$results}{A data frame with calculated parameters in supplementary sites.}
#' @author See \code{\link{resbiota-package}}.
#' @seealso \code{\link{checkReference}}, \code{\link{computeParameters}}, \code{\link{selectCommunities}}, 
#' \code{\link{extractResults}}, \code{\link{viewResults}}
#' @references 
#' Coutinho, A. G., Carlucci, M. B., & Cianciaruso, M. V. (2023). A framework to apply trait-based ecological 
#' restoration at large scales. Journal of Applied Ecology, 60, 1562–1571. https://doi.org/10.1111/1365-2664.14439
#' 
#' Coutinho, A. G., Nunes, A., Branquinho, C., Carlucci, M. B., & Cianciaruso, M. V. (2024). Natural regeneration 
#' enhances ecosystem multifunctionality but species addition can increase it during restoration monitoring. Manuscript 
#' in preparation.
#' @keywords MainFunction
#' @examples
#' data("cerrado.mini")
#' head(cerrado.mini$traits)
#' # Restoration new sites
#' scenarioA <- simulateCommunities(trait = cerrado.mini$traits,
#'                          ava = "Available",
#'                          cwm = "BT",
#'                          rao = c("SLA", "Height", "Seed"),
#'                          rich = c(10, 15),
#'                          it = 100)
#' scenarioA
#' # Restoration existing sites
#' scenarioB <- simulateCommunities(trait = cerrado.mini$traits, 
#'                                  restComp = cerrado.mini$restoration, 
#'                                  ava = "Available", 
#'                                  cwm = "BT", 
#'                                  rao = c("SLA", "Height", "Seed"), 
#'                                  rich = c(10, 15), 
#'                                  it = 100)
#' scenarioB
#' # Merge all scenarios
#' allScenarios <- mergeSimulations(scenarioA, scenarioB)
#' allScenarios
#' @export
simulateCommunities <- function(trait, restComp = NULL, restGroup = NULL, ava = NULL, und = NULL, it = 1000, rich, cwm = NULL, rao = NULL, prob = NULL, phi = 1, nInd = NULL, cvAbund = 1, prefix = NULL, method = "proportions", group = NULL, probGroupRich = NULL, probGroupAbund = NULL){
  RES <- list(call = match.call())
  # Check method
  METHOD <- c("proportions", "individuals")
  methodTest <- pmatch(method, METHOD)
  if (length(methodTest) > 1) {
    stop("Only one argument is accepted in method")
  }
  if (is.na(methodTest)) {
    stop("Invalid method")
  }
  if (methodTest == 2 && is.null(nInd)){
    stop("For the 'individuals' method it is mandatory specify the 'nInd' argument")
  }
  if(it<4){
    stop("The argument 'it' must be at minimum 4")
  }
  # Transform the rich argument in range vector
  if(length(rich) == 1){
    rich <- rep(rich, 2)
  }
  # Generate species proportions
  propMatrix <- propMatrix(trait = trait, ava = ava, und = und, it = it, 
                           rich = rich, cwm = cwm, rao = rao, phi = phi, 
                           nInd = nInd, cvAbund = cvAbund, prob = prob, method = method,
                           group = group, probGroupRich = probGroupRich, probGroupAbund = probGroupAbund)
  # Include species proportions in restoration sites
  if(!is.null(restComp)){
    rowNameProMatrix <- rownames(propMatrix)
    rowNameRest <- rownames(restComp)
    template0 <- makeMatrixTemplate(propMatrix, restComp)
    propMatrix <- reorganizeMatrix(template = template0, propMatrix, fillNA = TRUE)
    restComp <- reorganizeMatrix(template = template0, restComp, fillNA = TRUE)
    # TRANSFORM PROPORTIONS AND SUM TO REST 
    # propMatrixAdd <- propMatrix
    # AQUI ESTA COMENTADO POR HORA
    # propMatrixAdd <- propMatrix * max_add #transforma matriz
    # # Set prop = 0 to rare species
    # if(!is.null(min_p)){
    #   pos <- propMatrixAdd < min_p
    #   propMatrixAdd[pos] <- 0
    #   propMatrixAdd <- (propMatrixAdd/rowSums(propMatrixAdd)) * max_add
    # }
    # Sum simulated species proportions with species proportions in the restoration sites
    propMatrixList <- lapply(1:nrow(restComp), function(i) sweep(propMatrix, MARGIN = 2, STATS = restComp[i, ], FUN = "+"))
    propMatrixTab <- do.call(rbind, propMatrixList)
    # If "proportions" method (re)calculate species proportions
    if(methodTest == 1){
      propMatrixTab <- propMatrixTab/rowSums(propMatrixTab)  
    }
    rownames(propMatrixTab) <- as.vector(t(outer(rowNameRest, rowNameProMatrix, FUN = paste0)))
    restName <- rep(rowNameRest, each = length(rowNameProMatrix))
    if(!is.null(restGroup)){
      restGroup <- restGroup[rep(seq_len(nrow(restGroup)), each = length(rowNameProMatrix)),, drop = FALSE]
      restGroup <- data.frame(Simulation = paste0(prefix, rownames(propMatrixTab)), Site = restName, restGroup)
    } else{
      restGroup <- data.frame(Simulation = paste0(prefix, rownames(propMatrixTab)), Site = restName)
    }
  } else { 
    restGroup <- data.frame(Simulation = paste0(prefix, rownames(propMatrix)))
    propMatrixTab <- propMatrix
  }
  if(!is.null(prefix)){
      restGroup <- data.frame(Scenario = prefix, restGroup)
  }
  rownames(propMatrixTab) <- paste0(prefix, rownames(propMatrixTab))
  rownames(restGroup) <- NULL
  RES$simulation$composition <- propMatrixTab
  RES$simulation$group <- restGroup
  class(RES) <- "simRest"
  # Composicao pode ter linhas e/ou colunas com tudo zero. Remover?
  return(RES)
}