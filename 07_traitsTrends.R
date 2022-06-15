### species-level trends vs forest traits ####

library(tidyverse)

selectTaxa <-  c("Ants", "AquaticBugs","Bees","Bryophytes","Carabids","Centipedes","Craneflies",
                 "Dragonflies","E&D","Ephemeroptera","FungusGnats","Gelechiids","Hoverflies",
                 "Ladybirds","LeafSeedBeetles","Lichens","Millipedes","Molluscs","Moths","Neuropterida",
                 "Orthoptera","PlantBugs","Plecoptera","RoveBeetles","ShieldBugs","SoldierBeetles",
                 "Soldierflies","Spiders","Trichoptera","Wasps","Weevils")

### choose models #######################

#trends
trendsFolder <- "outputs/spTrends"

#forest associations
forestFolder <- "outputs/HPC/broadleaf/run2"

### trends ##############################

spTrends <- list.files(trendsFolder, full.names = TRUE) %>%
              set_names() %>%
              map_dfr(readRDS, .id="source") %>%
              group_by(source) %>%
              mutate(Taxa = strsplit(source,"_")[[1]][2]) %>%
              mutate(Taxa = gsub(".rds","",Taxa)) %>%
              ungroup() %>%
              filter(Taxa %in% selectTaxa) 
              

### forest preferences ###################

gamOutputs <- list.files(forestFolder,full.names=TRUE) %>%
                  str_subset("gamOutput_glm_subset_random_") %>%
                  set_names() %>%
                  map_dfr(readRDS, .id="source") %>%
                  group_by(source) %>%
                  mutate(Taxa = strsplit(source,"_")[[1]][5]) %>%
                  ungroup() %>%
                  filter(Taxa %in% selectTaxa) %>%
                  mutate(species = tolower(species),
                         forest_assoc = estimate)
                  
### merge ################################

df <- inner_join(spTrends, gamOutputs, by = c("species","Taxa"))

###plotting ##############################

ggplot(df) +
  geom_point(aes(x = forest_assoc, y = mean_change)) +
  facet_wrap(~Taxa) +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_vline(xintercept = 0, linetype="dashed") 

### lms #################################

#for each taxa, fit an lm

### end ##################################