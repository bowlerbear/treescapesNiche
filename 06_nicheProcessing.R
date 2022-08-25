#outputs of script - 04_forestNiche.R

library(tidyverse)
library(ggridges)
theme_set(theme_classic())

selectTaxa <-  c("Ants", "AquaticBugs","Bees","Carabids","Centipedes","Craneflies",
                 "Dragonflies","E&D","Ephemeroptera","Gelechiids","Hoverflies",
                 "Ladybirds","Molluscs","Moths",
                 "Orthoptera","PlantBugs","ShieldBugs",
                 "Soldierflies","Spiders","Trichoptera","Wasps")
#21

#check Aquatic bugs, Cranefiles, E&D

### choose models ###########################

modelFolder <- "outputs/forestAssociations/broadleaf_subsample3"

modelFolder <- "outputs/forestAssociations/conif_subsample3"

### species per taxa ########################

list.files(modelFolder,full.names=TRUE) %>%
  str_subset("gamOutput_gamm_subset_random_") %>%
  set_names() %>%
  map_dfr(readRDS, .id="source") %>%
  group_by(source) %>%
  mutate(Taxa = strsplit(source,"_")[[1]][7]) %>%
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
    fileName <- "gamOutput_gamm_subset_random_"
    
  }else if(modeltype=="mixed_shape"){
    fileName <- "gamOutput_gamm_shape_subset_random_"
  }
  
  temp <- list.files(modelFolder,full.names=TRUE) %>%
    str_subset(fileName) %>%
    set_names() %>%
    map_dfr(readRDS, .id="source") %>%
    group_by(source) %>%
    mutate(Taxa = strsplit(source, fileName)[[1]][2]) %>%
    mutate(Taxa = gsub("conif_","",Taxa)) %>%
    mutate(Taxa = gsub("decid_","",Taxa)) %>%
    mutate(Taxa = strsplit(Taxa,"_")[[1]][1])%>%
    ungroup() %>%
    filter(Taxa %in% selectTaxa) %>%
    mutate(Taxa = case_when(Taxa=="E&D" ~ "Empidids",
                          TRUE ~ as.character(Taxa)))  %>%
    add_column(modeltype = modeltype)
  
  #fix names
  
  if("Species" %in% names(temp)){
    temp <- temp %>% rename(Species = species)
  }
  
  if("decid_forest" %in% names(temp)){
    temp <- temp %>% rename(decidForest = decid_forest)
    
  }
  
  return(temp)
}

### linear #####################

gamOutputs <- getModels(modelFolder,modeltype = "simple_linear")
          
saveRDS(gamOutputs, file="outputs/broadleafAssocations_simple_linear.rds")

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
  #filter(estimate>(-0.1)) %>%
  filter(std_error<0.05) %>%
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

### linear/gamm ##################################################

gamOutputs <- getModels(modelFolder,modeltype = "mixed_linear")

saveRDS(gamOutputs, file="outputs/forestAssociations/broadleafAssocations_mixed_linear.rds")

#what taxa do we have?
sort(unique(gamOutputs$Taxa))#all present for broadleaf
sort(unique(gamOutputs$Taxa))#all present for conif
selectTaxa[!selectTaxa %in%  sort(unique(gamOutputs$Taxa))]

#order taxa by median preferece
taxaSummary <- gamOutputs %>%
  group_by(Taxa) %>%
  summarise(medEffect = median(estimate)) %>%
  ungroup() %>%
  arrange(desc(medEffect))
gamOutputs$Taxa <- factor(gamOutputs$Taxa, levels=taxaSummary$Taxa)

#plotting
fig1a <- gamOutputs %>%
  filter(estimate>(-0.1)) %>%
  filter(std_error<0.05) %>%
  ggplot() +
  geom_density_ridges(aes(x = estimate, y = Taxa, fill=Taxa),
                      rel_min_height = 0.001) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none") +
  ylab("Taxa (ordered by median value)") +
  xlab("Association with forest cover")

saveRDS(fig1a, file="plots/fig1a.rds")

### gam shape/gamm ###############################################

gamOutputs <- getModels(modelFolder,modeltype = "mixed_shape")

#what taxa do we have?
sort(unique(gamOutputs$Taxa))
selectTaxa[!selectTaxa %in%  sort(unique(gamOutputs$Taxa))]

speciesMax <- gamOutputs %>%
  group_by(species) %>%
  summarise(maxPred = max(preds)) %>%
  ungroup()

#for broad leaf
gamOutputs %>%
  ggplot()+
  geom_line(aes(x=decidForest, y=preds, group=species))+
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

### compare models #################################################

#compare model with and without year as a random effect
modelFolder <- "outputs/forestAssociations/broadleaf"

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
#highly correlated

### compare decid vs conif #######################################

gamOutputs_broadleaf <- getModels(modelFolder = "outputs/forestAssociations/broadleaf_subsample3",
                                  modeltype = "mixed_linear") %>%
                                  add_column(forest = "broadleaf")

gamOutputs_conif <- getModels(modelFolder = "outputs/forestAssociations/conif_subsample3",
                              modeltype = "mixed_linear") %>%
                              add_column(forest = "conif")


allGams <- bind_rows(gamOutputs_broadleaf,gamOutputs_conif) %>%
  dplyr::select(forest, species, Taxa, modeltype, estimate, std_error) %>%
  pivot_wider(everything(),
              names_from="forest", 
              values_from=c("estimate","std_error")) %>%
  janitor::clean_names()

ggplot(allGams) +
  geom_point(aes(x = estimate_broadleaf, y = estimate_conif)) +
  facet_wrap(~taxa, scales="free") +
  geom_abline(aes(intercept = 0, slope = 1),linetype = "dashed") +
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0) +
  xlab("Broadleaf Association") + ylab("Conifer Association")


ggplot(allGams) +
  geom_point(aes(x = std_error_broadleaf, y = std_error_conif)) +
  facet_wrap(~taxa, scales="free") +
  geom_abline(aes(intercept = 0, slope = 1),linetype = "dashed") +
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)

#mostly correlated

### forest special 1 ##############################################

#based on continous forest associations

#run code in previous section

allGams$estimate_broadleafS <- ifelse(allGams$estimate_broadleaf<0,0.0001,
                                     allGams$estimate_broadleaf)

allGams$estimate_conifS <- ifelse(allGams$estimate_conif<0,0.0001,
                                     allGams$estimate_conif)

allGams$ForestSpecial <- log(allGams$estimate_broadleafS/allGams$estimate_conifS)

summary(allGams$ForestSpecial)

subset(allGams, ForestSpecial >6)

subset(allGams, ForestSpecial < (-38))

allGams %>%
  saveRDS(., file="outputs/forestAssociations/forestSpecial_mixed_linear.rds")

allGams %>%
  filter(abs(ForestSpecial)<2) %>%
  ggplot() +
  geom_point(aes(x = estimate_broadleaf, y = estimate_conif, 
                 colour=ForestSpecial)) +
  facet_wrap(~taxa, scales="free") +
  geom_abline(aes(intercept = 0, slope = 1),linetype = "dashed") +
  scale_color_gradient2()+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)

allGams %>%
  filter(abs(ForestSpecial)<10) %>%
ggplot() +
  geom_point(aes(x = estimate_broadleaf, y = estimate_conif, 
                 colour=ForestSpecial)) +
  geom_abline(aes(intercept = 0, slope = 1),linetype = "dashed") +
  scale_color_gradient2() +
  geom_hline(yintercept=0) + geom_vline(xintercept=0) +
  xlim(-0.1,0.1) + ylim(-0.1,0.1)

### forest special 2 ##############################################

# #based on GAMM shape models
# gamOutputs_broadleaf <- getModels(modelFolder = "outputs/forestAssociations/broadleaf",
#                                   modeltype = "mixed_shape") %>%
#   add_column(forest = "broadleaf") %>%
#   rename(forestCover = decidForest)
# 
# gamOutputs_conif <- getModels(modelFolder = "outputs/forestAssociations/conif",
#                               modeltype = "mixed_shape") %>%
#   add_column(forest = "conif") %>%
#   rename(forestCover = conif_forest)
# 
# 
# allGams <- bind_rows(gamOutputs_broadleaf,gamOutputs_conif) %>%
#   dplyr::select(forest, forestCover, species, Taxa, preds) %>%
#   filter(forestCover==50) %>%
#   pivot_wider(everything(),
#               names_from="forest", 
#               values_from="preds") %>%
#   janitor::clean_names()
# 
# allGams %>%
#   mutate(ForestSpecial = boot::logit(broadleaf) - boot::logit(conif)) %>%
#   saveRDS(., file="outputs/forestAssociations/forestSpecial_mixed_shape.rds")

### occ-det models ##############################################

modelFolder <- "outputs/forestAssociations/broadleaf/occuModels"

carabidMods <- list.files(modelFolder,full.names=TRUE) %>%
  str_subset("occuGam_forestcover") %>%
  set_names() %>%
  map_dfr(readRDS, .id="source") %>%
  group_by(source) %>%
  mutate(species = strsplit(source,"_")[[1]][4]) %>%
  mutate(species = gsub(".rds","", species)) %>%
  ungroup() %>%
  rename(estimate_occdet = estimate) %>%
  filter(para == "decidForest")

gamOutputs <- getModels(modelFolder = "outputs/forestAssociations/broadleaf",
                        modeltype = "simple_linear") %>%
  filter(Taxa == "Carabids") %>%
  mutate(species = gsub("Col_", "", species))

compareOutput <- inner_join(carabidMods, gamOutputs, by = "species")


qplot(estimate_occdet, estimate, data=compareOutput)
#bit different but mostly similar
cor(compareOutput$estimate_occdet,compareOutput$estimate)

### end ##########################################################
