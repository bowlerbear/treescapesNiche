library(metafor)
library(tidyverse)
library(ggridges)
theme_set(theme_classic())

selectTaxa <-  c("Ants", "AquaticBugs","Bees","Carabids","Centipedes","Craneflies",
                 "Dragonflies","E&D","Ephemeroptera","Gelechiids","Hoverflies",
                 "Ladybirds","Molluscs","Moths",
                 "Orthoptera","PlantBugs","ShieldBugs",
                 "Soldierflies","Spiders","Trichoptera","Wasps")

### useful functions ###################

source("00_functions.R")

### get species names ################

speciesNames <- read.csv("C:/Users/diabow/OneDrive - UKCEH/Projects/General/masterLookup.csv", as.is=TRUE)

#concept is NA when it is the same as NAME - replace it with the NAME for matching
speciesNames$CONCEPT[is.na(speciesNames$CONCEPT)] <- speciesNames$NAME[is.na(speciesNames$CONCEPT)]
speciesNames$CONCEPT <- tolower(speciesNames$CONCEPT)

#get rid of initials
speciesNames$NAME <- gsub(" s.l.| agg| s.s.| s.str.| agg.| B","", speciesNames$NAME)

### trends ##############################

trendsFolder <- "outputs/speciesTrends"

spTrends <- list.files(trendsFolder, full.names = TRUE) %>%
  set_names() %>%
  map_dfr(readRDS, .id="source") %>%
  rename(Taxa = taxon) %>%
  filter(Taxa %in% selectTaxa)  %>%
  mutate(Taxa = case_when(Taxa=="E&D" ~ "Empidids",
                          TRUE ~ as.character(Taxa))) %>%
  filter(sd_change < outlierValue(sd_change)) %>%
  rename(CONCEPT = species)

table(spTrends$Taxa)

### clean names ##########################

spTrends$Species <- speciesNames$NAME[match(spTrends$CONCEPT, speciesNames$CONCEPT)]

#for species still blank, use original name
spTrends$Species[is.na(spTrends$Species)] <- spTrends$CONCEPT[is.na(spTrends$Species)]

spTrends$species <- tolower(spTrends$Species)

### get pantheon database #####

indexDir <- "C:/Users/diabow/OneDrive - UKCEH/Projects/General/Pantheon/species-index"
indexFile <- read_csv(paste(indexDir,"species-index.csv",sep="/")) %>%
  janitor::clean_names() %>%
  mutate(species = tolower(species)) 

#what species are missing from this database
mean(unique(spTrends$species) %in% indexFile$species) # #75% of species

#missingness across taxa
taxaData <- unique(spTrends[,c("species","Taxa")])
taxaData$Present <- taxaData$species %in% indexFile$species 
tapply(taxaData$Present,taxaData$Taxa,mean)

#join all
spTrends <- spTrends %>%
  left_join(.,indexFile, by="species") 

### clusters ##############

clusterDF <- readRDS("outputs/clustering/deriv_classification_all.rds") %>%
  janitor::clean_names() %>%
  mutate(CONCEPT = tolower(species)) %>%
  select(CONCEPT, cluster)

table(clusterDF$cluster)

#merge with trends
spTrends <- spTrends %>%
  left_join(., clusterDF) 

### example species ######

spTrends %>%
  filter(grepl("tree-associated", broad_biotope)) %>%
  filter(!is.na(cluster)) %>%
  mutate(cluster = reLabel(cluster)) %>%
  mutate(direction = ifelse(lowerCI_change>0,"increase",
                            ifelse(upperCI_change<0,"decrease",
                                   ifelse(mean_change<0 & upperCI_change>0,"non-sig decrease",
                                          "non-sig increase")))) %>%
  filter(direction=="decrease") %>%
  filter(cluster=="prefer woodland") %>%
  select(species, direction, cluster, mean_change, habitat, resources) %>%
  arrange(mean_change)


### habitat vs trends #####

#### biotope ##############

names(indexFile)
table(spTrends$broad_biotope)

#how much data is there?
mean(!is.na(spTrends$broad_biotope))

mean(grepl("tree-associated", spTrends$broad_biotope[!is.na(spTrends$broad_biotope)]))

#broad classification
treeDF <- spTrends %>%
          filter(grepl("tree-associated", broad_biotope))

g1 <- treeDF %>%
ggplot(aes(x=Taxa, y = median_change))+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(colour="lightgrey", alpha=0.25) +
  ylab("Tree-associated species growth rate")+
  geom_hline(yintercept=0, linetype="dashed")+
  coord_flip()

##### testing ##########################

summary(treeDF$mean_change)

rma.mv(mean_change, sd_change^2, 
       random = ~ 1 | Taxa, data = treeDF)
#negative but this isnt the difference between open and trees
#not of interest

#### habitat ##########################

#broad classification
table(spTrends$habitat)

#how much data is there?
mean(!is.na(spTrends$habitat))

mean(grepl("arboreal", spTrends$habitat[!is.na(spTrends$habitat)]))
mean(grepl("decaying wood", spTrends$habitat[!is.na(spTrends$habitat)]))
mean(grepl("shaded woodland floor", spTrends$habitat[!is.na(spTrends$habitat)]))

treeDF <- spTrends %>%
  filter(habitat %in% c("arboreal","decaying wood","shaded woodland floor"))

g2 <- treeDF %>%
  ggplot()+
  geom_boxplot(aes(x=Taxa, y = median_change),
               outlier.shape = NA) +
  ylab("Species growth rate")+
  facet_wrap(~habitat) +
  geom_hline(yintercept=0, linetype="dashed")+
  coord_flip()

##### testing ##########################

summary(treeDF$mean_change)

rma.mv(mean_change, sd_change^2, mods=~habitat,
       random = ~ 1 | Taxa, data = treeDF)

rma.mv(mean_change, sd_change^2, mods=~habitat-1,
       random = ~ 1 | Taxa, data = treeDF)

#                                 estimate      se     zval    pval    ci.lb    ci.ub      
#habitatarboreal                -0.3806  0.1235  -3.0825  0.0021  -0.6225  -0.1386   ** 
#habitatdecaying wood           -0.1184  0.1430  -0.8277  0.4078  -0.3987   0.1619      
#habitatshaded woodland floor   -0.4510  0.1231  -3.6654  0.0002  -0.6922  -0.2099  *** 

# shaded woodland floor (Found in closed canopy woodland and scrub, where it is separated vertically rather than horizontally from arboreal assemblage types. It is associated with low levels of disturbance.)
#	decaying wood: Decay specifically within a tree's heartwood, which may be subdivided into red-rot or white-rot fungi, having a strong impact on the species composition of the associated invertebrates.
# arboreal: A habitat in and on trees, including the canopy, trunks and branches.

#### resources ######

treeDF %>% 
  group_by(resources) %>%
  summarise(nuSpecies = length(resources)) %>%
  arrange(desc(nuSpecies))

#https://pantheon.brc.ac.uk/content/tree-associated-biotope

#conifer or broadleaf
cbDF <- treeDF %>%
          filter(grepl("conifer or broadleaved", treeDF$resources))
nrow(cbDF)#771

cbDF <- treeDF %>%
  filter(grepl("tree species", treeDF$resources))
nrow(cbDF)#0

### habitat vs my metric ####

gamOutputs <- readRDS("outputs/forestAssociations/forestSpecial_mixed_linear.rds") %>%
                  mutate(CONCEPT = tolower(species))

#get max of either forest pref
gamOutputs$estimate_forest <- ifelse(gamOutputs$estimate_broadleaf>gamOutputs$estimate_conif,
                                     gamOutputs$estimate_broadleaf, gamOutputs$estimate_conif)

#sort species namesto match
gamOutputs$Species <- speciesNames$NAME[match(gamOutputs$CONCEPT, speciesNames$CONCEPT)]
gamOutputs$Species[is.na(gamOutputs$Species)] <- gamOutputs$CONCEPT[is.na(gamOutputs$Species)]
gamOutputs$species <- tolower(gamOutputs$Species)

#check missingness across taxa
#taxaData <- unique(gamOutputs[,c("species","taxa")])
#taxaData$Present <- taxaData$species %in% tolower(indexFile$species) 
#tapply(taxaData$Present,taxaData$taxa,mean)

#merge with habitat data
gamOutputs <- gamOutputs %>%
                inner_join(.,indexFile, by="species")
               
gamOutputs$habitat <- ifelse(grepl("tree-associated", gamOutputs$broad_biotope),
                              "Tree-associated","Other")
 
gamOutputs %>%
  filter(abs(estimate_forest)<0.05) %>%
  ggplot() +
  geom_boxplot(aes(x=taxa, y=estimate_forest, fill=habitat)) +
  geom_hline(yintercept=0, linetype="dashed") +
  ylab("Forest association")+
  coord_flip()

### conservation status ####

#### full list ############

conservationSpecies <- list.files("C:/Users/diabow/OneDrive - UKCEH/Projects/General/Protection/conservation-designations-20220202",
                              pattern='.csv', full.names = TRUE) %>% read.csv() 

#subset to select designations
selectDesignations <- read.csv("../../data/Select_Designations.csv") %>%
                        filter(keep=="keep")

conservationSpecies <- conservationSpecies %>%
                        filter(Designation %in% selectDesignations$ï..Designation_type)

#compile two species name columns
conservationSpecies <- tolower(unique(c(conservationSpecies$Recommended.taxon.name,
                         conservationSpecies$Designated.name)))

 
#plotting
conservationSummary <- spTrends %>%
  #filter(cluster %in% c(2,4)) %>%
  filter(species %in% conservationSpecies) %>%
  filter(!is.na(cluster)) %>%
  mutate(cluster = reLabel(cluster)) %>%
  mutate(direction = ifelse(lowerCI_change>0,"increase",
                            ifelse(upperCI_change<0,"decrease",
                                   ifelse(mean_change<0 & upperCI_change>0,"non-sig decrease",
                                          "non-sig increase")))) %>%
  mutate(direction = factor(direction,
                            levels = c("decrease", "non-sig decrease", 
                                       "non-sig increase", "increase"))) 

conservationSummary %>%
  group_by(Taxa, direction, cluster) %>%
  summarise(nuSpecies = length(unique(species))) %>%
  ggplot() +
  geom_col(aes(x=Taxa, y = nuSpecies, fill=direction)) +
  scale_fill_brewer("Trend", type="div") +
  ylab("Number of species with a conservation designation") +
  facet_wrap(~cluster, scales="free_x") +
  coord_flip() +
  theme(legend.position="top")

ggsave("plots/conservation_status.png",width=6,height=7)

#summary of proportions
conservationSummary %>%
  group_by(direction, cluster) %>%
  summarise(nuSpecies = length(unique(species))) %>%
  group_by(direction) %>%
  mutate(totalNu = sum(nuSpecies)) %>%
  filter(direction %in% c("decrease", "increase")) %>%
  mutate(prop = nuSpecies/totalNu)

#chisq
conservationSummary %>%
  group_by(direction, cluster) %>%
  summarise(nuSpecies = length(unique(species))) %>%
  group_by(direction) %>%
  mutate(totalNu = sum(nuSpecies)) %>%
  filter(direction %in% c("decrease", "increase"))

#identify declining forest species
conservationSummary %>%
  filter(cluster %in% c("prefer forest", "prefer intermediate")) %>%
  filter(direction =="increase") %>%
  arrange(median_change) %>%
  select(Species, median_change) %>%
  pull("Species")

#### S41 ###################

#all possible status
table(spTrends$conservation_status)


#S41 species as a measure of conservation status
spTrends$S41 <- sapply(spTrends$conservation_status, function(x){
  ifelse(grepl("Section 41",x),1,0)})

table(spTrends$S41)

spTrends %>%
  filter(broad_biotope == "tree-associated") %>%
  filter(S41==1) %>%
  mutate(direction = ifelse(lowerCI_change>0,"increase",
                            ifelse(upperCI_change<0,"decrease",
                                   ifelse(mean_change<0 & upperCI_change>0,"non-sig decrease",
                                          "non-sig increase")))) %>%
  group_by(Taxa, direction) %>%
  summarise(nuSpecies = length(unique(species))) %>%
  ggplot() +
  geom_col(aes(x=Taxa, y = nuSpecies, fill=direction)) +
  scale_fill_brewer("Trend", type="div") +
  ylab("Number of Section 41 species") +
  coord_flip()

speciesTreeS41 <- spTrends$species[spTrends$S41==1 & spTrends$broad_biotope=="tree-associated"]

saveRDS(speciesTreeS41,file="outputs/speciesTreesS41.rds")

#### BAP list ############

prioritySpecies <- list.files("C:/Users/diabow/OneDrive - UKCEH/Projects/General/Protection",
                              pattern='.csv', full.names = TRUE) %>%
                    read.csv(.,skip=1) %>%
                    mutate(species = tolower(NBN.current.scientific.name))

#sum(missingSpecies %in% prioritySpecies$NBN.current.scientific.name)

spTrends$priority <- ifelse(spTrends$species %in% prioritySpecies$species,1,0)
table(spTrends$priority)

#save as file
data.frame(Species = spTrends$Species[spTrends$priority==1],
          Taxa = spTrends$Taxa[spTrends$priority==1]) %>%
saveRDS(.,file="outputs/prioritySpecies.rds")

#compare the two
table(spTrends$priority,spTrends$S41)

spTrends %>%
  filter(broad_biotope == "tree-associated") %>%
  filter(priority==1) %>%
  mutate(direction = ifelse(lowerCI_change>0,"increase",
                            ifelse(upperCI_change<0,"decrease",
                                   ifelse(mean_change<0 & upperCI_change>0,"non-sig decrease",
                                          "non-sig increase")))) %>%
  group_by(Taxa, direction) %>%
  summarise(nuSpecies = length(unique(species))) %>%
  ggplot()+
  geom_col(aes(x=Taxa, y = nuSpecies, fill=direction)) +
  scale_fill_brewer("Trend", type="div") +
  ylab("Number of BAP species") +
  coord_flip()

#### select time series #############################################

#retreived from the data labs

speciesTS <- list.files("outputs/priority41", full.names = TRUE) %>%
                map_dfr(readRDS) %>%
                inner_join(., spTrends, by="species") %>%
                mutate(year = as.numeric(gsub("year_","",year)))

speciesTS$NAME[is.na(speciesTS$NAME)] <- speciesTS$species[is.na(speciesTS$NAME)]

speciesTS$Species <- sapply(speciesTS$NAME, function(x){
  paste0(toupper(substr(x,1,1)), tolower(substr(x,2,nchar(x))))
}) %>% as.character()

ggplot(speciesTS) +
  geom_point(aes(x = year, y = meanOcc)) +
  geom_ribbon(aes(x = year, y=meanOcc, ymin = lowerOcc, ymax = upperOcc), alpha=0.5) +
  ylab("Predicted occupancy proportion") +
  xlab("Year") +
  facet_wrap(~Species)

### end ######################