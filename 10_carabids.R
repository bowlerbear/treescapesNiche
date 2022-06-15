#Script to scope the use of beetle records for the treescapes project

#libraries:
#devtools::install_github("colinharrower/BRCmap")
library(BRCmap)
library(tidyverse)
library(sf)
library(sp)

theme_set(theme_classic())

setwd("C:/Users/diabow/OneDrive - UKCEH/Projects/Treescapes")

### choose taxa #######################################

mytaxa <- "Carabids"

### get niche #########################################

#run script in forestNiche.R
#run script in nicheProcessing.R

### data ################################################

#view carabid data
head(taxa_data)
nrow(taxa_data)

### species ############################################

#map to species names - data file obtained from Robin
speciesMap <- read.csv("GBRS_CONCEPTS.csv")
taxa_data$Species <- speciesMap$NAME[match(taxa_data$CONCEPT, speciesMap$CONCEPT)]

### trait mapping ##########################################

speciesSummary <- taxa_data %>%
  filter(YEAR>1979) %>%
  group_by(Species) %>%
  summarise(nuRecs = length(Species),
            nuYears = length(unique(YEAR))) %>%
  ungroup() %>%
  arrange(desc(nuRecs)) %>%
  filter(Species %in% commonSpecies)

#add on linear association
gamOutput <- readRDS("outputs/gamOutput.rds")
speciesSummary$effect <- gamOutput$estimate[match(speciesSummary$Species,
                                                  gamOutput$species)]

#species index file from Pantheon
indexDir <- "C:/Users/diabow/OneDrive - UKCEH/Projects/General/Pantheon/species-index"
indexFile <- read_csv(paste(indexDir,"species-index.csv",sep="/")) %>%
              janitor::clean_names()

#how many of the beetle species are listed here
mean(commonSpecies %in% indexFile$species)

#merge
speciesSummary <- speciesSummary %>%
                    dplyr::rename(species = Species) %>%
                    inner_join(.,indexFile, by="species")

ggplot(speciesSummary) +
  geom_boxplot(aes(x=habitat,y=effect)) +
  geom_hline(yintercept=0, linetype="dashed") +
  coord_flip() +
  ylab("estimated forest effect")


ggplot(speciesSummary) +
  geom_boxplot(aes(x=broad_biotope,y=effect)) +
  geom_hline(yintercept=0, linetype="dashed") +
  coord_flip() +
  ylab("estimated forest effect")


ggplot(speciesSummary) +
  geom_boxplot(aes(x=conservation_status,y=effect)) +
  geom_hline(yintercept=0, linetype="dashed") +
  coord_flip() +
  ylab("estimated forest effect")

#IUCN status
library(taxize)

# Set Tolken:
k <- c("INSERT YOUR TOLKEN HERE")

# Set Function:
RedList_status <- function(s, k){
  info  <- iucn_summary(s, key=k)
  status <- iucn_status(info, key=k)
  status <- as.data.frame(status)
  return(status)
}

# Run Function:
resultado <- RedList_status(s,k)
resultado

usethis::edit_r_environ()

#or use 
library(rredlist)
rl_search()


#and against the data from Ben Woodcock (taken from Dessender and Turin)


#and the weighted correlation coefficient from Bullock
Dir <- "C:/Users/diabow/OneDrive - UKCEH/Projects/Treescapes/literature/Chetcuti_Data"
weightedFile <- read.csv(paste(Dir,"carabidae_habitat_associations_weighted14.csv",sep="/"))
speciesSummary$phi <- weightedFile$R[match(speciesSummary$Species,weightedFile$species)]
speciesSummary$Habitat <- weightedFile$Habitat[match(speciesSummary$Species,weightedFile$species)]

ggplot(speciesSummary) +
  geom_point(aes(x=phi,y=effect)) 

#Notiophilus germinyi and Notiophilus aquaticus - negative classification fits better


### end ####################################################

