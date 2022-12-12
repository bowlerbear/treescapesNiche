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

#check whether to exclude: Aquatic bugs, Cranefiles, E&D

### useful functions ###################

source("00_functions.R")

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

### linear #####################

# gamOutputs <- getModels(modelFolder,modeltype = "simple_linear")
#           
# saveRDS(gamOutputs, file="outputs/broadleafAssocations_simple_linear.rds")
# 
# #what taxa do we have?
# sort(unique(gamOutputs$Taxa))#all taxa
# 
# #order taxa by median preferece
# taxaSummary <- gamOutputs %>%
#                 group_by(Taxa) %>%
#                 summarise(medEffect = median(estimate)) %>%
#                 ungroup() %>%
#                 arrange(desc(medEffect))
# gamOutputs$Taxa <- factor(gamOutputs$Taxa, levels=taxaSummary$Taxa)
# 
# gamOutputs %>%
#   filter(abs(estimate) < outlierValue(abs(gamOutputs$estimate))) %>%
#   filter(std_error < outlierValue(std_error)) %>%
#   ggplot() +
#   geom_density_ridges(aes(x = estimate, y = Taxa, fill=Taxa),
#                       rel_min_height = 0.001) +
#   geom_vline(xintercept = 0, linetype = "dashed") +
#   theme(legend.position = "none") +
#   xlab("Association with forest cover")

### gam shape ##########################################

# gamOutputs <- getModels(modelFolder,modeltype = "simple_shape")
# 
# #what taxa do we have?
# sort(unique(gamOutputs$Taxa))#all!!
# 
# speciesMax <- gamOutputs %>%
#   group_by(Species) %>%
#   summarise(maxPred = max(preds)) %>%
#   ungroup()
# 
# #for broad leaf
# gamOutputs %>%
# ggplot()+
#   geom_line(aes(x=decidForest, y=preds, group=Species))+
#   facet_wrap(~Taxa)+
#   xlab("Decid forest cover %") + ylab("Occupancy")+
#   theme_classic()
# 
# #for coniferous
# gamOutputs %>%
#   ggplot()+
#   geom_line(aes(x=conifForest, y=preds, group=Species))+
#   facet_wrap(~Taxa)+
#   xlab("Conif forest cover %") + ylab("Occupancy")+
#   theme_classic()

### linear/gamm ##################################################

gamOutputs <- getModels(modelFolder,modeltype = "mixed_linear")

saveRDS(gamOutputs, file="outputs/forestAssociations/broadleafAssocations_mixed_linear.rds")

saveRDS(gamOutputs, file="outputs/forestAssociations/conifAssocations_mixed_linear.rds")

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
  filter(abs(estimate) < outlierValue(abs(estimate))) %>%
  filter(std_error < outlierValue((std_error))) %>%
  ggplot() +
  geom_density_ridges(aes(x = estimate, y = Taxa, fill=Taxa),
                      rel_min_height = 0.001) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none") +
  ylab("Taxa (ordered by median value)") +
  xlab("Association with broadleaf woodland cover")

saveRDS(fig1a, file="plots/fig1a.rds")
#saveRDS(fig1a, file="plots/fig1a_coniferous.rds")

#magnitude statistics
quantile(gamOutputs$estimate)
length(gamOutputs$estimate)

#positive association per 10% forest cover
sum(exp(gamOutputs$estimate*10)>1.2)
subset(gamOutputs, exp(estimate*10)>1.2)$species
subset(gamOutputs, exp(estimate)>1.02)$species

#median coniferous association
median(gamOutputs$estimate[gamOutputs$estimate>0])#0.01060032
median(gamOutputs$estimate[gamOutputs$estimate>0])#0.01234388

### illustrative linear plots ####################################

glmPreds <- readRDS("outputs/gamOutput_illustrative_glm.rds")

fig1b <- ggplot(glmPreds)+
  geom_line(aes(x=decid_forest, y=preds, colour=species))+
  xlab("Broadleaf woodland cover %")+
  ylab("Occupancy probability (logit-scale)")+
  theme(legend.position = "none")

saveRDS(fig1b, file="plots/fig1b.rds")

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

#for each species, get forest cover at which occupancy is maximised
speciesOptimums <- gamOutputs %>%
                      group_by(species, Taxa) %>%
                      summarise(maxForest = decidForest[which.max(preds)]) %>%
                      ungroup()

saveRDS(speciesOptimums, file="outputs/forestAssociations/broadleafAssocations_optimums.rds")

summary(speciesOptimums$maxForest)
hist(speciesOptimums$maxForest)

#number of species with optimum at zero
mean(speciesOptimums$maxForest==0)#49
#at 100%
mean(speciesOptimums$maxForest==100)#37
#somewhere in between
mean(speciesOptimums$maxForest>0 & speciesOptimums$maxForest<100)

#summary of those in between
summary(speciesOptimums$maxForest[speciesOptimums$maxForest>0 & speciesOptimums$maxForest<100])

### discrete classification ######################################

#deciduous
gamOutputs <- readRDS("outputs/forestAssociations/broadleafAssocations_mixed_linear.rds")

#add on trend classification
gamOutputs$Trend <- ifelse(gamOutputs$estimate>0 & gamOutputs$pr_t<0.05, "positive",
                           ifelse(gamOutputs$estimate<0 & gamOutputs$pr_t<0.05, "negative",
                                  "none"))
#overall
table(gamOutputs$Trend)
#negative     none positive 
#891     1365      816

#split by taxa
gamOutputs %>%
  group_by(Taxa, Trend) %>%
  count() %>%
  group_by(Taxa) %>%
  mutate(total = sum(n),
         prop = n/total) %>%
  filter(Trend=="positive") %>%
  arrange(desc(prop))


#split by taxa and direction of trend
gamOutputs %>%
  group_by(Trend) %>%
  summarise(meanE = median(estimate)) %>%
  arrange(desc(meanE))

1-exp(0.0168)

#coniferous
gamOutputs <- readRDS("outputs/forestAssociations/conifAssocations_mixed_linear.rds")

#add on trend classification
gamOutputs$Trend <- ifelse(gamOutputs$estimate>0 & gamOutputs$pr_t<0.05, "positive",
                           ifelse(gamOutputs$estimate<0 & gamOutputs$pr_t<0.05, "negative",
                                  "none"))
#overall
table(gamOutputs$Trend)
#negative     none positive 
#652     1845      575

#split by taxa
gamOutputs %>%
  group_by(Taxa, Trend) %>%
  count() %>%
  group_by(Taxa) %>%
  mutate(total = sum(n),
         prop = n/total) %>%
  filter(Trend=="positive") %>%
  arrange(desc(prop))

### compare gam vs gamm models #################################################

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

### compare subset options ######################################

gamOutputs_subset1 <- getModels(modelFolder = "outputs/forestAssociations/broadleaf",
                                  modeltype = "mixed_linear") %>%
                        add_column(subset = "subset1")

nrow(gamOutputs_subset1)
#1649

gamOutputs_subset2 <- getModels(modelFolder = "outputs/forestAssociations/broadleaf_subsample3",
                                  modeltype = "mixed_linear") %>%
                        add_column(subset = "subset2")

nrow(gamOutputs_subset2)
#2181

allGams <- bind_rows(gamOutputs_subset1, gamOutputs_subset2) %>%
  dplyr::select(subset, species, Taxa, modeltype, estimate, std_error) %>%
  pivot_wider(everything(),
              names_from="subset", 
              values_from=c("estimate","std_error")) %>%
  janitor::clean_names() %>%
  filter(complete.cases(.))

ggplot(allGams) +
  geom_point(aes(x = estimate_subset1, y = estimate_subset2)) +
  facet_wrap(~taxa, scales="free") +
  geom_abline(aes(intercept = 0, slope = 1),linetype = "dashed") +
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0) +
  xlab("Subset1 Association") + ylab("Subset3 Association")

ggplot(allGams) +
  geom_point(aes(x = std_error_subset1, y = std_error_subset2)) +
  facet_wrap(~taxa, scales="free") +
  geom_abline(aes(intercept = 0, slope = 1),linetype = "dashed") +
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)

#get mean correlation and mean difference in error

allGams %>%
  group_by(taxa) %>%
  summarise(corr = cor(estimate_subset1, estimate_subset2)) %>%
  ungroup()


### compare decid vs conif #######################################

gamOutputs_broadleaf <- getModels(modelFolder = "outputs/forestAssociations/broadleaf_subsample3",
                                  modeltype = "mixed_linear") %>%
                                  add_column(forest = "broadleaf") %>%
                                  group_by(Taxa) %>%
                                  filter(std_error < outlierValue(std_error)) %>%
                                  ungroup()

gamOutputs_conif <- getModels(modelFolder = "outputs/forestAssociations/conif_subsample3",
                              modeltype = "mixed_linear") %>%
                              add_column(forest = "conif") %>%
                              group_by(Taxa) %>%
                              filter(std_error < outlierValue(std_error)) %>%
                              ungroup()

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

#quantify the strength of correlations
temp <- allGams %>%
  group_by(taxa) %>%
  summarise(corr = cor(estimate_broadleaf, estimate_conif, 
                       use="pairwise.complete.obs")) %>%
  arrange(desc(corr))

summary(temp$corr)

### forest special 1 ##############################################

#based on continous forest associations

#run code in previous section

allGams$estimate_broadleafS <- ifelse(allGams$estimate_broadleaf<0,0.0001,
                                     allGams$estimate_broadleaf)

allGams$estimate_conifS <- ifelse(allGams$estimate_conif<0,0.0001,
                                     allGams$estimate_conif)

allGams$ForestSpecial <- log(allGams$estimate_broadleafS/allGams$estimate_conifS)

summary(allGams$ForestSpecial)

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

### species summaries ###############################################

output <- list.files("outputs/speciesSummary",full.names=TRUE) %>%
  set_names() %>%
  map_dfr(readRDS, .id="source") %>%
  group_by(source) %>%
  mutate(Taxa = strsplit(source,"_")[[1]][2]) %>%
  ungroup() %>%
  filter(Taxa %in% selectTaxa)

nrow(output)

rareSpecies <- output %>%
  filter(nuRecs < 50 | nuSites < 20)
nrow(rareSpecies)
head(rareSpecies)

### end ##########################################################
