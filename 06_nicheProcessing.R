#outputs of script - forestNiche.R

library(tidyverse)
library(ggridges)
theme_set(theme_classic())

selectTaxa <-  c("Ants", "AquaticBugs","Bees","Bryophytes","Carabids","Centipedes","Craneflies",
                 "Dragonflies","E&D","Ephemeroptera","FungusGnats","Gelechiids","Hoverflies",
                 "Ladybirds","LeafSeedBeetles","Lichens","Millipedes","Molluscs","Moths","Neuropterida",
                 "Orthoptera","PlantBugs","Plecoptera","RoveBeetles","ShieldBugs","SoldierBeetles",
                 "Soldierflies","Spiders","Trichoptera","Wasps","Weevils")
#31 of them

### choose models ###########################

modelFolder <- "outputs/forestAssociations/broadleaf/run2"

### species per taxa ########################

list.files(modelFolder,full.names=TRUE) %>%
  str_subset("gamOutput_glm_subset_random_") %>%
  set_names() %>%
  map_dfr(readRDS, .id="source") %>%
  group_by(source) %>%
  mutate(Taxa = strsplit(source,"_")[[1]][5]) %>%
  ungroup() %>%
  filter(Taxa %in% selectTaxa) %>%
  group_by(Taxa) %>%
  summarise(nuSpecies = length(species)) %>%
  arrange(nuSpecies)

### linear associations #####################

gamOutputs <- list.files(modelFolder,full.names=TRUE) %>%
                str_subset("gamOutput_glm_subset_random_") %>%
                set_names() %>%
                map_dfr(readRDS, .id="source") %>%
                group_by(source) %>%
                mutate(Taxa = strsplit(source,"_")[[1]][5]) %>%
                ungroup() %>%
                filter(Taxa %in% selectTaxa) %>%
                mutate(Taxa = case_when(Taxa=="E&D" ~ "Empidid",
                          TRUE ~ as.character(Taxa)))  %>%
                arrange(desc(estimate))
          
#what taxa do we have?
sort(unique(gamOutputs$Taxa))#all taxa

#order taxa by median preferece
taxaSummary <- gamOutputs %>%
                group_by(Taxa) %>%
                summarise(medEffect = median(estimate)) %>%
                ungroup() %>%
                arrange(desc(medEffect))
gamOutputs$Taxa <- factor(gamOutputs$Taxa, levels=taxaSummary$Taxa)

#### plotting #####

gamOutputs %>%
  filter(estimate>(-0.1)) %>%
  ggplot() +
  geom_density_ridges(aes(x = estimate, y = Taxa, fill=Taxa),
                      rel_min_height = 0.001) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none") +
  xlab("Association with forest cover")

### gam shape ##########################################

gamOutputs <- list.files(modelFolder,full.names=TRUE) %>%
  str_subset("gamOutput_gam_shape_subset_random_") %>%
  set_names() %>%
  map_dfr(readRDS, .id="source") %>%
  group_by(source) %>%
  mutate(Taxa = strsplit(source,"_")[[1]][6]) %>%
  ungroup() %>%
  filter(Taxa %in% selectTaxa) %>%
  mutate(Taxa = case_when(Taxa=="E&D" ~ "Empidid",
                          TRUE ~ as.character(Taxa))) 

#what taxa do we have?
sort(unique(gamOutputs$Taxa))#all!!

#### plotting #####

speciesMax <- gamOutputs %>%
  group_by(Species) %>%
  summarise(maxPred = max(preds)) %>%
  ungroup()

gamOutputs %>%
ggplot()+
  geom_line(aes(x=decidForest, y=preds, group=Species))+
  facet_wrap(~Taxa)+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  theme_classic()

### linear association/gamm4 ##################################################

gamOutputs <- list.files(modelFolder,full.names=TRUE) %>%
  str_subset("gamOutput_gamm4_subset_random_") %>%
  set_names() %>%
  map_dfr(readRDS, .id="source") %>%
  group_by(source) %>%
  mutate(Taxa = strsplit(source,"_")[[1]][5]) %>%
  ungroup() %>%
  filter(Taxa %in% selectTaxa) %>%
  mutate(Taxa = case_when(Taxa=="E&D" ~ "Empidid",
                          TRUE ~ as.character(Taxa))) %>%
  arrange(desc(estimate))

#what taxa do we have?
sort(unique(gamOutputs$Taxa))
#missing moths

### gam shape/gamm4 ###########################################################

gamOutputs <- list.files(modelFolder,full.names=TRUE) %>%
  str_subset("gamOutput_gamm4_shape_subset_random_") %>%
  set_names() %>%
  map_dfr(readRDS, .id="source") %>%
  group_by(source) %>%
  mutate(Taxa = strsplit(source,"_")[[1]][6]) %>%
  ungroup() %>%
  filter(Taxa %in% selectTaxa) %>%
  mutate(Taxa = case_when(Taxa=="E&D" ~ "Empidid",
                          TRUE ~ as.character(Taxa))) 

#what taxa do we have?
sort(unique(gamOutputs$Taxa))
#missing moths 

### compare subsets ############################################################


### end ########################################################################

#still missing moths

#Error in pwrssUpdate(pp, resp, tol = tolPwrss, GQmat = GQmat, compDev = compDev,  : 
#                       pwrssUpdate did not converge in (maxit) iterations
#                     Calls: %>% ... <Anonymous> -> <Anonymous> -> stopifnot -> fn -> pwrssUpdate
#                     Execution halted
                     