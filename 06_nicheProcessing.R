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

modelFolder <- "outputs/forestAssociations/broadleaf"

modelFolder <- "outputs/forestAssociations/conif"

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

### get models #############################

#function to process a set of models

getModels <- function(modelFolder,modeltype = "simple_linear"){ 
  
  #translate
  if(modeltype=="simple_linear"){
    fileName <- "gamOutput_glm_subset_random_"
    
  } else if(modeltype=="simple_shape"){
    fileName <- "gamOutput_gam_shape_subset_random_"
    
  }else if(modeltype=="mixed_linear"){
    fileName <- "gamOutput_gamm4_subset_random_"
    
  }else if(modeltype=="mixed_shape"){
    fileName <- "gamOutput_gamm4_shape_subset_random_"
  }
  
  list.files(modelFolder,full.names=TRUE) %>%
    str_subset(fileName) %>%
    set_names() %>%
    map_dfr(readRDS, .id="source") %>%
    group_by(source) %>%
    mutate(Taxa = strsplit(source, fileName)[[1]][2]) %>%
    mutate(Taxa = gsub("conif_","",Taxa)) %>%
    mutate(Taxa = strsplit(Taxa,"_")[[1]][1])%>%
    ungroup() %>%
    filter(Taxa %in% selectTaxa) %>%
    mutate(Taxa = case_when(Taxa=="E&D" ~ "Empidid",
                          TRUE ~ as.character(Taxa)))  %>%
    add_column(modeltype = modeltype)
}

### linear associations #####################

gamOutputs <- getModels(modelFolder,modeltype = "simple_linear")
          
#what taxa do we have?
sort(unique(gamOutputs$Taxa))#all taxa

#order taxa by median preferece
taxaSummary <- gamOutputs %>%
                group_by(Taxa) %>%
                summarise(medEffect = median(estimate)) %>%
                ungroup() %>%
                arrange(desc(medEffect))
gamOutputs$Taxa <- factor(gamOutputs$Taxa, levels=taxaSummary$Taxa)

gamOutputs %>%
  filter(estimate>(-0.1)) %>%
  ggplot() +
  geom_density_ridges(aes(x = estimate, y = Taxa, fill=Taxa),
                      rel_min_height = 0.001) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none") +
  xlab("Association with forest cover")

### gam shape ##########################################

gamOutputs <- getModels(modelFolder,modeltype = "simple_shape")

#what taxa do we have?
sort(unique(gamOutputs$Taxa))#all!!

speciesMax <- gamOutputs %>%
  group_by(Species) %>%
  summarise(maxPred = max(preds)) %>%
  ungroup()

#for broad leaf
gamOutputs %>%
ggplot()+
  geom_line(aes(x=decidForest, y=preds, group=Species))+
  facet_wrap(~Taxa)+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  theme_classic()

#for coniferous
gamOutputs %>%
  ggplot()+
  geom_line(aes(x=conifForest, y=preds, group=Species))+
  facet_wrap(~Taxa)+
  xlab("Conif forest cover %") + ylab("Occupancy")+
  theme_classic()

### linear/gamm4 ##################################################

gamOutputs <- getModels(modelFolder,modeltype = "mixed_linear")

#what taxa do we have?
sort(unique(gamOutputs$Taxa))
#missing moths

### gam shape/gamm4 ###############################################

gamOutputs <- getModels(modelFolder,modeltype = "mixed_shape")

#what taxa do we have?
sort(unique(gamOutputs$Taxa))
#missing moths 

### compare models #################################################

#compare model with and without year as a random effect

gamOutputs_mixed <- getModels(modelFolder,modeltype = "mixed_linear")
gamOutputs_simple <- getModels(modelFolder,modeltype = "simple_linear")

allGams <- bind_rows(gamOutputs_mixed,gamOutputs_simple) %>%
  select(species, Taxa, modeltype, estimate, std_error) %>%
  pivot_wider(everything(),
              names_from="modeltype", 
              values_from=c("estimate","std_error")) %>%
  janitor::clean_names()

ggplot(allGams) +
  geom_point(aes(x = estimate_mixed_linear, y = estimate_simple_linear)) +
  facet_wrap(~taxa, scales="free") +
  geom_abline(aes(intercept = 0, slope = 1, linetype = "dashed"))


gamOutputs_broadleaf <- getModels(modelFolder = "outputs/forestAssociations/broadleaf",
                                  modeltype = "simple_linear") %>%
                        add_column(forest = "broadleaf")

gamOutputs_conif <- getModels(modelFolder = "outputs/forestAssociations/conif",
                              modeltype = "simple_linear") %>%
                        add_column(forest = "conif")

allGams <- bind_rows(gamOutputs_broadleaf,gamOutputs_conif) %>%
            select(forest, species, Taxa, modeltype, estimate, std_error) %>%
            pivot_wider(everything(),
                        names_from="forest", 
                        values_from=c("estimate","std_error")) %>%
            janitor::clean_names()

ggplot(allGams) +
  geom_point(aes(x = estimate_broadleaf, y = estimate_conif)) +
  facet_wrap(~taxa, scales="free") +
  geom_abline(aes(intercept = 0, slope = 1, linetype = "dashed"))

### compare decid vs conif #######################################

gamOutputs_broadleaf <- getModels(modelFolder = "outputs/forestAssociations/broadleaf",
                                  modeltype = "simple_linear") %>%
                                  add_column(forest = "broadleaf")

gamOutputs_conif <- getModels(modelFolder = "outputs/forestAssociations/conif",
                              modeltype = "simple_linear") %>%
                              add_column(forest = "conif")

allGams <- bind_rows(gamOutputs_broadleaf,gamOutputs_conif) %>%
  select(forest, species, Taxa, modeltype, estimate, std_error) %>%
  pivot_wider(everything(),
              names_from="forest", 
              values_from=c("estimate","std_error")) %>%
  janitor::clean_names()

ggplot(allGams) +
  geom_point(aes(x = estimate_broadleaf, y = estimate_conif)) +
  facet_wrap(~taxa, scales="free") +
  geom_abline(aes(intercept = 0, slope = 1),linetype = "dashed")

#mostly correlated

### end ##########################################################
