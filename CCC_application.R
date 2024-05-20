## Manual to Run the R script and functions to select species for ecological restoration
## following the trait-based approach of Coutinho AG, Carlucci MB & Cianciaruso MV. 
## A framework to apply trait-based ecological restoration at large scales. J. App. Ecol.

## Download all files in the Supplementary Material and save them in your working 
## directory
## Install the following R packages in your computer
## adiv, Select, fundiversity, RColorBrewer

## Now you are ready to start. Follow this step-by-step guide using the same data we used
## in our paper.

setwd('C:/Users/user1/...') #replace by your working directory

source('framework_functions.R') #read functions and packages

# only if you want to calculate processing time
start = Sys.time()

# set seed to be able to repeat the same simulation in the future
# the seed we used in the manuscript is 2022
set.seed(2022)

###### STEP 1: Generate simulated communities and calculate parameters ######
## For an example or to replicate the findings in the manuscript
## load the example.csv file. 
## ATTENTION when using your own dataset:
#1) the name of the first column should be all_species,
#2) the name of the second column should be available,
#3) If you have reference sites you want to compare your solutions with
#remember to include them with relative abundances (instead of raw abundances)

## load the dataset

data <- read.table('regional_pool.txt', head=T)

## select the columns in data with the functional traits (columns 3 to 6 in #data)
## this will vary depending on the number of traits you have in your own dataset. If this
## is the case change column numbers accordingly

your_traits<-colnames(data[,3:6])
traits<- data[,c(your_traits)]
rownames(traits) <- data$all_species #rename rows with species names

## select the range of species richness you like to have in the random communities. In the
## example below, communities ranging from 20 to 70 species will be generated. 
SR<- c(20,70)

## number of random communities to be generated.
## 
## How large N should be? 
## This depends mostly
## on (1) the number of species in the species pool and (2) the functional trait values of
## the species. A species pool composed by few species that are functionally similar need
## a lower N values than a pool composed by a several number of functionally distinct 
## species. Therefore there is not a straight answer for the "N question" above.
## Depending on your findings (= the number of communities that fall within the 
## restoration thresholds or targets) you can increase N ## always observing how many new
## selected communities this will return. You will able to ## check this easily in the
## STEP 2. But, remember that large N values will increase 
## processing time. This is especially true if you have reference sites.
## We recommend N = 5000 as a minimum value.

N <- 20000

## specify below which metrics you would like to calculate and which traits should be
## used to calculate each of them. 
## *** ATTENTION *** Trait names must match the correspondent column names in the object
## data

## Community Weighted Mean (CWM; Garnier et al. 2004; Lavorel et al. 2008). Define which
## traits will be used to calculate CWM. You can use ordinal traits, but
## the columns in your trait data must be numeric. To use ordinal variables
## transform your categories into numbers. The number of traits should be less than the
## number of species and no missing values are tolerated.
## If you have species with missing trait information you should consider one of the
## several approaches to handling this. For some examples on this see Penone et al. 2014
## (10.1111/2041-210X.12232), Taugourdeau et al. 2014 (10.1002/ece3.989), Johnson et al. ## 2021 (10.1111/geb.13185), Debastiani et al. 2021 (10.1016/j.ecoinf.2021.101315)

# bark thickness CWM
cwm	<- c("BT")

# you can also calculate CWM for more than one trait in object data
# just add them in the vector below
# for example, CWM for bark thickness and CWM SLA
#cwm <- c("BT", "SLA")

## Rao Quadratic Entropy (Rao). 
## This is a widely used measure of functional diversity that combines species abundances
## and functional differences among species. For some examples, see: 
## Botta-Dukat 2005 (10.1111/j.1654-1103.2005.tb02393.x)
## de Bello et al. 2016 (10.1007/s00442-016-3546-0)
## Laughlin et al. 2018 (10.1111/2041-210X.13023)
#######

## You can define either a vector indicating
## which traits will be used to calculate Rao or a distance matrix (a dist object).
## This is especially useful if your trait matrix has several types of
## variables (for example, continous and categorical traits).
## In such case you should use gower distance to create the distance
## matrix. Check Pavoine et al. 2009. Oikos (doi.org/10.1111/#j.1600-0706.2008.16668.x) 
## on how to properly deal with distinct types of variables.
#### *** ATTENTION *** Trait names must match the correspondent column names in the object
## data

# using a trait vector
rao <- c("SLA","Height", "Seed")

## using an object class dist (a distance matrix)

#rao = dist(scale(traits[,c("SLA","Height", "Seed")]) )

# if you have reference sites to which you want to compare your solutions using
#functional dissimilarity, select columns in data which represent your reference
#sites. NAs are not allowed.
your_reference_sites<-colnames(data[,9:14])
ref <- t(data[,your_reference_sites] )
colnames(ref) <- data$all_species

#### if you do not have reference sites or want to skip functional dissimilarity
### calculations set ref as FALSE:
#ref <- FALSE

## If you want to calculate cost, you need to provide a vector of species cost
## per individual and a vector of planting density (for example, number of individuals per
## hectare) for each species.
## Ideally, density values should take into account germination rates and seedling
## mortality.

cost <- data$Cost
dens <- data$Density

## Now, use comSimulation to generate simulated communities and calculate
##functional parameters.
rcomm <- comSimulation(trait = traits, ava = data$available,
                       it = N, rich = SR,
                       cwm = cwm,
                       rao = rao,
                       cost = cost,
                       dens = dens,
                       ref = ref )

rcomm$ref_communities
## STEP 2: Select simulated communities based on restoration targets: ####

## First, use the function viewResults to compare different parameters and
## define the thresholds.
## in the example below view the relationship between bark thickness CWM and Rao
## a plot similar to Figure 2A in the manuscript will be produced. 
viewResults(x = 'cwm_BT', y = 'rao', sim = rcomm,
            xlab = 'CWM_BT', ylab = 'Rao', ylim=c(0,1),
            hide_sel = TRUE)

## Now, define the desired thresholds to select communities. 
## *** Attention: Names must match column names in rcomm object ***

# thresholds if you have calculated CWM for a single trait, for example,
# CWM bark thickness higher than 6mm and Rao higher than 0.7
thresholds <- c("cwm_BT > 6", "rao > 0.7")

# thresholds if you have calculated CWM for more than a trait, for example,
# CWM bark thickness higher than 6mm, CWM SLA higher than 180 and Rao higher than 0.7
#thresholds <- c("cwm_BT > 6", "cwm_SLA > 180", "rao > 0.7")


## Now, use comSelection to filter selected communities that satisfies
## these thresholds:
selSim <- comSelection(param = rcomm$sim_communities$parameters,
                       comp = rcomm$sim_communities$composition,
                       tests = thresholds)

#see the number of selected communities for each threshold (and for all them)
selSim$N 

# Add these results to object rcomm:
rcomm$sel_communities <- selSim
rcomm$thresholds <- c(selSim$thresholds, selSim$thresholds)

################################################################################
## if you do not want to calculate functional dissimilarity between selected
## communities and references sites jump to STEP 3.
################################################################################# 



## Calculating functional dissimilarity ########
##
## If you have reference sites, you can calculate functional dissimilarity
## between selected communities and reference sites.
## Functional dissimilarity is a pair-wise metric between each selected community
## and each reference site. If you have more than a reference site, functional
## dissimilarity will be the average dissimilarity between each selected community and the
## reference sites.
## Functional dissimilarity is calculated with the function discomQE from the package
## adiv using the default formula "QE". Therefore, it corresponds to the Rao's 
## dissimilarity between communities. This metric ranges from 0 (no dissimilarity) to 1  ## (maximum dissimilarity). For more details and references, please see the adiv package R ## Documentation. 

##Because the functional dissimilarity returned is the average dissimilarity between
##each simulated community and each reference site, the dissimilarity
##among reference sites can also be calculated.
##For these reason, you should bind simulated communities with reference sites to
##have a single input in the function disCalculation. This step is required to
#plot reference areas in the functional dissimilarity axis using viewResults.

selSimRef <- as.matrix(rbind(selSim$composition, ref ))

## Now, you need to provide a trait data with traits that should be used
## to calculate functional dissimilarity. A distance matrix (dist object) is also
## accepted.

traitDis <- scale(traits[,c('SLA', 'Height', 'Seed')])
#traitDis = dist(scale(traits[,c("BT", "SLA", "Seed")]))

## Now, use function disCalculation to calculate functional dissimilarity
## depending on your species trait values, some warnings may appear with the following:
## In is.euclid(as.dist(dis)) : Zero distance(s)
## this only means that at least a pair of species have identical trait values, resulting
## in zero distances. This is is the case with datasets (as ours) where trait data is
## missing at species level and replace by genus or family-level averages.

selSimDis <- disCalculation(sim = selSimRef,
                            trait = traitDis,
                            ref = ref)

## Now you can check the functional dissimilarity between your selected simulated 
## communities and reference sites. This is useful to explore the results and help to 
## define a feasible threshold for functional dissimilarity.
## You can plot functional dissimilarity against any other parameter with the
## code below (for a different parameter, change the object 'parameter').

dev.off()
#for rao
parameter <- selSim$parameters$rao
#for CWM Bark Thickness
#parameter <- selSim$parameters$cwm_BT
#for CWM SLA
#parameter <- selSim$parameters$cwm_SLA
#for species richness
#parameter <- selSim$parameters$richness
#for cost
#parameter <- selSim$parameters$cost

plot(selSimDis[1:(length(selSimDis)-nrow(ref))], parameter,
     pch = 19, xlim=c(0,1),xlab = 'Functional dissimilarity')

## Now, it is necessary to add values of functional dissimilarity to the
## object selSim, define a threshold for functional dissimilarity, and use 
##comSelection to select simulated communities again

selSim$parameters$dissim <- selSimDis[!names(selSimDis) %in% row.names(ref)] 
thresholds2 <- c("dissim < 0.35")
selSim2 <- comSelection(selSim$parameters,
                        selSim$composition,
                        thresholds2)

## Now, update these results in rcomm:

#add results for selected communities
rcomm$sel_communities <- selSim2 

#add functional dissimilarity of reference sites
rcomm$ref_communities$parameters$dissim <- selSimDis[row.names(ref)]

# add functional dissimilarity threshold
rcomm$thresholds <- c(selSim$thresholds, selSim2$thresholds)

#######################################################
##
## You can also calculate functional dissimilarity for all
## simulated communities uncommenting the lines below.
## *** ATTENTION ***
## This is necessary if you want to recreate figure 2B of the manuscript (just load
## rcommDis.rds and add to rcomm).
## This calculation can be done to explore functional dissimilarity
## between reference sites and N-communities assembled at random
## Note this will take some time, depending on how many reference sites you have, the 
## number of random communities and the processing power of your computer.  :)
##
#########################################################

#rcommDis <-  disCalculation(sim = rcomm$sim_communities$composition,
#               trait = traitDis,
#               ref = ref)

### save a rds object so you don't have to run this again in the future

#saveRDS(rcommDis, 'rcommDis.rds') 

###load the rds file
rcommDis <- readRDS('rcommDis.rds') 

rcomm$sim_communities$parameters$dissim <- rcommDis


########### STEP 3: Visualize and save the results ####################

## To change colors in the figures you can modify the object COLS below.
## Colors must be in this order: communities with at least one unavailable species;
##communities with only available species; communities with at least
##one unavailable species that satisfy restoration criteria;
##communities with only available species that satisfy restoration criteria.
## For other colour palettes see RColorBrewer package documentation
## if you want to add transparency to your colour palette see add.alpha() 
## in the GISTools package

#library(GISTools)

## defining the colour palette
brpal <- brewer.pal(3, 'Paired')
COLS <- c('grey', 'black', brpal[1], brpal[2], brpal[3]) #default colors


## *** Attention: x and y axis should be defined with the same parameters names 
## in rcomm; to see them:

#names(rcomm$sim_communities$parameters)

## View Rao vs. CWM
## a plot similar to Figure 2A in the manuscript will be produced.

viewResults(x = 'cwm_BT', y = 'rao', sim = rcomm,
            cols = COLS, xlab = 'CWM_BT', ylab = 'Rao', ylim=c(0,1))

## If you calculated functional dissimilarity
## View Rao vs. functional dissimilarity. A plot similar to Figure 2B 
## in the manuscript will be produced but only with selected communities.
## If this functional dissimilarity was only calculated for 
## selected simulated communities, only these communities must be plotted.
## Thus, set 'hide_notsel' = TRUE.

viewResults(x = 'dissim', y = 'rao', sim = rcomm,
            cols = COLS, xlab = 'Functional dissimilarity', ylab = 'Rao',
            ylim = c(0,1), xlim = c(0,1), hide_notsel =  TRUE)

## View cost vs. species richness. A plot similar to Figure 1S in the manuscript
## will be plotted. 
## If cost is calculated for simulated communities
## that contains only available species, simulated communities with any unavailable
## species should not be plotted, setting hide_una = TRUE. However, if you also have cost
## estimates to unavailable species set hide_una = FALSE. It is also possible to calculate
## the "cost" of reference sites setting hide_ref = FALSE. Such "cost" will refer to the
## cost to plant or sown the species found in reference sites. This may be of some use to
## put restoration cost into perspective
## Note that cost will be a function of the density parameter (for example, the number of
## individuals to be planted or sown per hectare considering individual establishment and
## or survival; if these informations are availble. Our default cost is in US dollars per
## hectare. 

viewResults(x = 'richness', y = 'cost', sim = rcomm,
            cols = COLS, xlab = 'species richness', ylab = 'Cost (USD/ha)',
            hide_una = TRUE, hide_ref = TRUE)

## PROCESSING TIME ##########
end = Sys.time()
end - start

### exporting your results

## community matrix for selected communities

selcomposition<- as.data.frame(rcomm$sel_communities$composition)

## parameters calculated for selected communities

selparameters<-as.data.frame(rcomm$sel_communities$parameters)

## combining in a single data frame to store and save
## 

results<-data.frame(selparameters, selcomposition)
dim(results)
save(results, file="results.txt")

## check the unavailable species that were present in selected communities
## these are the species necessary to achieve the thresholds when
## selected communities have at least one species not available on the market

sppSelected <-selcomposition[,colSums(selcomposition)>0]
unaSpp <- data$all_species[data$available == 0]
necessary.spp <- data.frame(unavailable_spp = intersect(unaSpp, colnames(sppSelected) ) )
necessary.spp