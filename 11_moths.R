#Script to scope the use of beetle records for the treescapes project

#libraries:
#devtools::install_github("colinharrower/BRCmap")
library(BRCmap)
library(tidyverse)
library(sf)
library(sp)

theme_set(theme_classic())

### choose taxa #####################################

mytaxa <- "Moths"

### get trait data ###################################

traitsDF <- read.csv("C:/Users/diabow/OneDrive - UKCEH/Projects/Treescapes/traits/moths/data/ecological_traits.csv",skip=1)

#select columns to use
selectNames <- c("scientific_name", "common_name", 
                 "X1..Woodland", "X1a_broadleaved_woodland", "X1b_coniferious_woodland",#habitat
                 "diurnal", "nocturnal", 
                 "forewing_minimum", "forewing_maximum", "estimated_dry_mass",
                 "obligate_univoltine", "obligate_multivoltine", "partial_generation",
                 "specificity", "number_hostplants", 
                 "light_value", "moisture_value", "reaction", "nitrogen", #ellenberg_traits
                 "broadleaf_trees", "coniferous_trees")#common host plants

traitsDF <- traitsDF[,selectNames]

### clean names ########################################

traitsDF <- traitsDF %>%
              rename(Woodland = X1..Woodland,
                     bf_Woodland = X1a_broadleaved_woodland,
                     c_Woodland = X1b_coniferious_woodland) %>%
              rename(Species = scientific_name)

traitsDF[is.na(traitsDF)] <- 0

#### explore ###########################################

colMeans(traitsDF[,sapply(traitsDF,is.numeric)], na.rm=T)

table(traitsDF$bf_Woodland, traitsDF$broadleaf_trees)

table(traitsDF$c_Woodland, traitsDF$coniferous_trees)

### modelled forest associations ######################

gamOutput <- list.files("outputs/HPC/broadleaf/run2", full.names = TRUE) %>%
                str_subset(mytaxa) %>%
                str_subset("gamOutput_glm_subset_random_") %>%
                readRDS() %>%
                rename(Concept = species)

#add on species names
speciesNames <- read.csv("C:/Users/diabow/OneDrive - UKCEH/Projects/General/masterLookup.csv", as.is=TRUE)
gamOutput$Species <- speciesNames$NAME[match(gamOutput$Concept, speciesNames$CONCEPT)]

### compare model-data with trait database #############

#get forest classification from the above database

gamOutput <- gamOutput %>%
              left_join(.,traitsDF, by="Species") 

gamOutput %>%
  filter(!is.na(bf_Woodland)) %>%
  ggplot() +
  geom_boxplot(aes(x=factor(bf_Woodland), y = estimate))

#right direction but overlap

#### missing species #####################################

missingSpecies <- gamOutput$Species[!gamOutput$Species %in% traitsDF$Species]
# nonsense...

### traits vs trends ####################################

mothTrends <- readRDS("outputs/sp_change_Moths.rds") %>%
                rename(Concept = species)

mothTrends$Species <- speciesNames$NAME[match(mothTrends$Concept, tolower(speciesNames$CONCEPT))]

mothTrends <- mothTrends %>%
                left_join(.,traitsDF, by="Species") 

mothTrends  %>%
  filter(!is.na(bf_Woodland)) %>%
  ggplot() +
  geom_violin(aes(x=factor(bf_Woodland), y = mean_change))

mothTrends  %>%
  filter(!is.na(c_Woodland)) %>%
  ggplot() +
  geom_violin(aes(x=factor(c_Woodland), y = mean_change))

### end ################################################