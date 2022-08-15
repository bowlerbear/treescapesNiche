library(tidyverse)
library(ggridges)
theme_set(theme_classic())

selectTaxa <-  c("Ants", "AquaticBugs","Bees","Carabids","Centipedes","Craneflies",
                 "Dragonflies","E&D","Ephemeroptera","Gelechiids","Hoverflies",
                 "Ladybirds","LeafSeedBeetles","Molluscs","Moths",
                 "Orthoptera","PlantBugs","ShieldBugs",
                 "Soldierflies","Spiders","Trichoptera","Wasps")

### trends ##############################

trendsFolder <- "outputs/speciesTrends"

spTrends <- list.files(trendsFolder, full.names = TRUE) %>%
  set_names() %>%
  map_dfr(readRDS, .id="source") %>%
  group_by(source) %>%
  rename(Taxa = taxon) %>%
  filter(Taxa %in% selectTaxa) 

table(spTrends$Taxa)

spTrends %>%
  ggplot() +
  geom_density_ridges(aes(x = mean_change, y = Taxa, fill=Taxa),
                      rel_min_height = 0.001) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none") +
  xlab("Trends")

#filter those with large uncertainties
hist(spTrends$sd_change)
summary(spTrends$sd_change)

spTrends <- spTrends %>%
  filter(sd_change < 2)

#### get species names ####

speciesNames <- read.csv("C:/Users/diabow/OneDrive - UKCEH/Projects/General/masterLookup.csv", as.is=TRUE)
speciesNames$CONCEPT[is.na(speciesNames$CONCEPT)] <- speciesNames$NAME[is.na(speciesNames$CONCEPT)]
speciesNames$CONCEPT <- tolower(speciesNames$CONCEPT)

#get rid of initials
speciesNames$NAME <- gsub(" s.l.| agg| s.s.| s.str.| agg.| B","", speciesNames$NAME)

#add to trends file
spTrends$Species <- speciesNames$NAME[match(spTrends$species, speciesNames$CONCEPT)]

#our all out species in this file
spTrends$species[!spTrends$species %in% speciesNames$CONCEPT]
head(subset(spTrends,!species %in% speciesNames$CONCEPT))

#in these cases, use raw name
spTrends$Species[is.na(spTrends$Species)] <- spTrends$species[is.na(spTrends$Species)]

#make all capital case
spTrends$Species <- sapply(spTrends$Species, function(x){
  paste0(toupper(substr(x,1,1)), tolower(substr(x,2,nchar(x))))
}) %>% as.character()

### get pantheon database #####

indexDir <- "C:/Users/diabow/OneDrive - UKCEH/Projects/General/Pantheon/species-index"
indexFile <- read_csv(paste(indexDir,"species-index.csv",sep="/")) %>%
  janitor::clean_names() %>%
  rename(Species = species)

#what species are missing from this database
mean(unique(spTrends$Species) %in% indexFile$Species) # #77% of species
taxaData <- unique(spTrends[,c("Species","Taxa")])
taxaData$Present <- taxaData$Species %in% indexFile$Species 
tapply(taxaData$Present,taxaData$Taxa,mean)

#can we get any of these as a synonymn
#missingSpecies <- sort(unique(spTrends$Species))[!unique(spTrends$Species) %in%
#                                                   indexFile$Species]

#join all
spTrends <- spTrends %>%
  left_join(.,indexFile, by="Species") 

### habitat vs trends #####

names(indexFile)
table(spTrends$broad_biotope)

#how much data is there?
mean(!is.na(spTrends$broad_biotope))

#broad classification
treeDF <- spTrends %>%
          filter(grepl("tree-associated", broad_biotope))

(g1 <- treeDF %>%
ggplot()+
  geom_boxplot(aes(x=Taxa, y = median_change)) +
  ylab("Species growth rate")+
  geom_hline(yintercept=0, linetype="dashed")+
  coord_flip()) 

#broad classification
table(spTrends$habitat)

#how much data is there?
mean(!is.na(spTrends$habitat))

treeDF <- spTrends %>%
  filter(habitat %in% c("arboreal","decaying wood","shaded woodland floor"))

(g2 <- treeDF %>%
  ggplot()+
  geom_boxplot(aes(x=Taxa, y = median_change)) +
  ylab("Species growth rate")+
  facet_wrap(~habitat) +
  geom_hline(yintercept=0, linetype="dashed")+
  coord_flip()) 

#cowplot::plot_grid(g1,g2,
#                   nrow=2)

#ggsave("plots/pantheon_traits.png",width=5.5,height=6)

### habitat vs new metric ####

gamOutputs <- readRDS("outputs/forestAssociations/forestSpecial_mixed_linear.rds") %>%
                  mutate(species = tolower(species))

#get max of either forest pref
gamOutputs$estimate_forest <- ifelse(gamOutputs$estimate_broadleaf>gamOutputs$estimate_conif,
                                     gamOutputs$estimate_broadleaf, gamOutputs$estimate_conif)

#sort species names
gamOutputs$Species <- speciesNames$NAME[match(gamOutputs$species, speciesNames$CONCEPT)]
gamOutputs$Species[is.na(gamOutputs$Species)] <- gamOutputs$species[is.na(gamOutputs$Species)]
gamOutputs$Species <- tolower(gamOutputs$Species)

#merge with habitat data
gamOutputs <- indexFile %>%
                mutate(Species = tolower(Species)) %>%
                inner_join(.,gamOutputs, by="Species")
               
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

#maybe a barplot is better - number of forest species with a specific conservation status
table(spTrends$conservation_status)

#S41 species as a measure of conservation status
spTrends$S41 <- sapply(spTrends$conservation_status, function(x){
  ifelse(grepl("Section 41",x),1,0)
})
table(spTrends$S41)

spTrends %>%
  filter(broad_biotope == "tree-associated") %>%
  filter(S41==1)%>%
  mutate(direction = ifelse(median_change>0,"increase","decrease"))%>%
  group_by(Taxa, direction) %>%
  summarise(nuSpecies = length(unique(species)))%>%
  ggplot()+
  geom_col(aes(x=Taxa, y = nuSpecies, fill=direction)) +
  ylab("Number of species")

speciesTreeS41 <- spTrends$species[spTrends$S41==1 & spTrends$broad_biotope=="tree-associated"]

saveRDS(speciesTreeS41,file="outputs/speciesTreesS41.rds")

### full list ####

prioritySpecies <- list.files("C:/Users/diabow/OneDrive - UKCEH/Projects/General/Protection",
                              pattern='.csv', full.names = TRUE) %>%
                    read.csv(.,skip=1)

#sum(missingSpecies %in% prioritySpecies$NBN.current.scientific.name)

spTrends$priority <- ifelse(spTrends$Species %in% prioritySpecies$NBN.current.scientific.name,
                            1,0)

myprioritySpecies <- data.frame(Species = spTrends$Species[spTrends$priority==1],
                                Taxa = spTrends$Taxa[spTrends$priority==1])

saveRDS(myprioritySpecies,
        file="outputs/prioritySpecies.rds")

#compare the two
table(spTrends$priority,spTrends$S41)

spTrends %>%
  filter(broad_biotope == "tree-associated") %>%
  filter(priority==1)%>%
  mutate(direction = ifelse(median_change>0,"increase","decrease"))%>%
  group_by(Taxa, direction) %>%
  summarise(nuSpecies = length(unique(species)))%>%
  ggplot()+
  geom_col(aes(x=Taxa, y = nuSpecies, fill=direction)) +
  ylab("Number of species")

ggsave("plots/conservation_status.png",width=6,height=3)

### section 41 species time series #############################################

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