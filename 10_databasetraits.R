library(tidyverse)
library(ggridges)
theme_set(theme_classic())

selectTaxa <-  c("Ants", "AquaticBugs","Bees","Carabids","Centipedes","Craneflies",
                 "Dragonflies","E&D","Ephemeroptera","Gelechiids","Hoverflies",
                 "Ladybirds","LeafSeedBeetles","Molluscs","Moths",
                 "Orthoptera","PlantBugs","ShieldBugs",
                 "Soldierflies","Spiders","Trichoptera","Wasps","Weevils")

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
  filter(sd_change < 3)

#### get species names ####

speciesNames <- read.csv("C:/Users/diabow/OneDrive - UKCEH/Projects/General/masterLookup.csv", as.is=TRUE)
speciesNames$CONCEPT[is.na(speciesNames$CONCEPT)] <- speciesNames$NAME[is.na(speciesNames$CONCEPT)]
spTrends$Species <- speciesNames$NAME[match(spTrends$species, tolower(speciesNames$CONCEPT))]

#make all capital case
spTrends$Species <- sapply(spTrends$Species, function(x){
  paste0(toupper(substr(x,1,1)), tolower(substr(x,2,nchar(species))))
}) %>% as.character()

### get pantheon database #####

mothTrends <- readRDS("outputs/sp_change_Moths.rds") %>%
  rename(Concept = species)

mothTrends$Species <- speciesNames$NAME[match(mothTrends$Concept, tolower(speciesNames$CONCEPT))]

mothTrends <- mothTrends %>%
  left_join(.,traitsDF, by="Species") 

