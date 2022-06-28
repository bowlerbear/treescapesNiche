### cluster-analysis #################################

library(tidyverse)
library(cluster)

selectTaxa <-  c("Ants", "AquaticBugs","Bees","Carabids","Centipedes","Craneflies",
                 "Dragonflies","E&D","Ephemeroptera","Gelechiids","Hoverflies",
                 "Ladybirds","LeafSeedBeetles","Molluscs","Moths",
                 "Orthoptera","PlantBugs","ShieldBugs",
                 "Soldierflies","Spiders","Trichoptera","Wasps","Weevils")

### choose models ###########################

modelFolder <- "outputs/forestAssociations/broadleaf"

### species-level ###################################
# 
# gamOutput <- readRDS("outputs/gamOutput_gam_shape_Carabids.rds")
# 
# #Summarize data for each species
# gamSummary <- gamOutput %>%
#   group_by(Species) %>%
#   summarise(meanPreds = mean(preds),
#             sdPreds = sd(preds))
# 
# #remove species were the sd is 0
# gamOutput <- gamOutput %>%
#   filter(Species %in% gamSummary$Species[gamSummary$sdPreds!=0])

### all taxa #########################################

gamOutputs <- list.files(modelFolder,full.names=TRUE) %>%
  str_subset("gamOutput_gam_shape_subset_random_") %>%
  set_names() %>%
  map_dfr(readRDS, .id="source") %>%
  group_by(source) %>%
  mutate(Taxa = strsplit(source,"_")[[1]][7]) %>%
  ungroup() %>%
  filter(Taxa %in% selectTaxa) %>%
  mutate(Taxa = case_when(Taxa=="E&D" ~ "Empidid",
                          TRUE ~ as.character(Taxa))) 

#Summarize data for each species
gamSummary <- gamOutputs %>%
  group_by(Species) %>%
  summarise(meanPreds = mean(preds),
            sdPreds = sd(preds))

#what prop of species show no variation
mean(gamSummary$sdPreds==0) # few!!

#remove species were the sd is 0 - prob too rare to be modelled
gamOutputs <- gamOutputs %>%
  filter(Species %in% gamSummary$Species[gamSummary$sdPreds!=0])


#remove species with large sd
mean(gamSummary$sdPreds>0.05) # quite a few...
gamOutputs <- gamOutputs %>%
  filter(Species %in% gamSummary$Species[gamSummary$sdPreds<0.05])
nrow(gamOutputs)

#### pam #########################################

#clusters both the height and the shape of the niche
#scale them relative to mean occupancy

library(cluster)

#put into a matrix
speciesResponses <- reshape2::acast(gamOutputs,
                                    decidForest ~ Species,
                                    value.var = "preds")

dim(speciesResponses)

#scale each column 
mydata <- apply(speciesResponses, 2, function(x){
  boot::logit(x) - median(boot::logit(x))
})

#keep those with variation 
# mydata <- mydata[,!is.nan(colMeans(mydata))]

#transpose
mydata <- t(mydata)

#compare clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#pick one
fit <- pam(mydata, 5) # cluster solution
mydata <- data.frame(mydata, cluster = fit$clustering)
table(mydata$cluster)

#add back onto the gamOutput
gamOutputs$cluster <- mydata$cluster[match(gamOutputs$Species,
                                              row.names(mydata))]

#species within
gamOutputs %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds, groups = Species))+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_wrap(~cluster)

gamOutputs %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds, groups = Species, colour=factor(cluster)))+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_wrap(~Taxa, scales="free")

gamOutputs %>%
  filter(Taxa %in% sort(unique(gamOutputs$Taxa))[6:10]) %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds, groups = Species))+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_wrap(cluster~Taxa, scales="free")

#mean per cluster
gamOutputs %>%
  group_by(cluster,decidForest) %>%
  summarise(preds = median(preds)) %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds))+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_wrap(~cluster)

ggsave("plots/clustering_all_kmeans_means.png")

#how each taxa is distributed in each cluster
gamOutputs %>%
  group_by(Taxa, cluster) %>%
  summarise(nuSpecies = length(unique(Species))) %>%
  ungroup() %>%
ggplot() +
  geom_col(aes(x=Taxa, y = nuSpecies, fill = factor(cluster))) +
  coord_flip()

ggsave("plots/clustering_all_kmeans_prop.png")

#save classification
gamOutputs %>%
  select(Species, cluster) %>%
  filter(!duplicated(Species)) %>%
  saveRDS(., file="outputs/clustering/kmeans_classification_all.rds")

#### correlation based ##############################

#this one captures the shape but not the magnitude of change

library(TSclust)

myTS <- reshape2::acast(gamOutputs,
                        Species ~ decidForest,
                        value.var = "preds")

#correlation-based distance
IP.dis <- diss(myTS, "COR")
fit <- hclust(IP.dis)
plot(fit)

#choose clusters
fit <- pam(IP.dis, k = 4)
mydata <- data.frame(mydata, cluster = fit$clustering)
table(mydata$cluster)

#add back onto the gamOutput
gamOutputs$cluster <- mydata$cluster[match(gamOutputs$Species,
                                           row.names(mydata))]

#species within
gamOutputs %>%
  group_by(Species, Taxa, cluster) %>%
  mutate(preds = scale(preds)) %>%
  ungroup %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds, groups = Species))+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_wrap(~cluster)

#mean per cluster
gamOutputs %>%
  group_by(cluster,decidForest) %>%
  summarise(preds = median(preds)) %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds))+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_wrap(~cluster)

ggsave("plots/clustering_all_corr_means.png")

#how each taxa is distributed in each cluster
gamOutputs %>%
  group_by(Taxa, cluster) %>%
  summarise(nuSpecies = length(unique(Species))) %>%
  ungroup() %>%
  ggplot() +
  geom_col(aes(x=Taxa, y = nuSpecies, fill = factor(cluster))) +
  coord_flip()
ggsave("plots/clustering_all_corr_prop.png")

#save classification
gamOutputs %>%
  select(Species, cluster) %>%
  filter(!duplicated(Species)) %>%
saveRDS(., file="outputs/clustering/corr_classification_all.rds")

#### derivatives ######################################

#get derivatives of the fitted gam?
#best do this within the model code

library(gratia)
library(mgcv)

getDerivatives <- function(myspecies){
  
  test <- gamOutputs %>%
    filter(Species == myspecies) 
  
  mod <- gam(preds ~ s(decidForest), data = test, 
             method = "REML")
  
  deriv1 <- derivatives(mod, type = "central", order=1) %>%
    add_column(Species = myspecies)
  
  return(deriv1)
  
}


allDerivatives <- lapply(sort(unique(gamOutputs$Species)), function(x){
  print(x)
  getDerivatives(x)
}) %>%
  reduce(rbind)

#pam on them
speciesResponses <- reshape2::acast(allDerivatives,
                                    data ~ Species,
                                    value.var = "derivative")

mydata <- speciesResponses

#transpose
mydata <- t(mydata)

#clusters
fit <- pam(mydata, 4) # cluster solution
mydata <- data.frame(mydata, cluster = fit$clustering)
table(mydata$cluster)
#most in one cluster

#add back onto the gamOutput
gamOutputs$cluster <- mydata$cluster[match(gamOutputs$Species,
                                          row.names(mydata))]

#species within
gamOutputs %>%
  group_by(Species, Taxa, cluster) %>%
  mutate(preds = scale(preds)) %>%
  ungroup %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds, groups = Species))+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_wrap(~cluster)

gamOutputs %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds, groups = Species, colour=factor(cluster)))+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_wrap(~Taxa, scales="free")

gamOutputs %>%
  filter(Taxa %in% sort(unique(gamOutputs$Taxa))[1:5]) %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds, groups = Species))+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_grid(cluster~Taxa)

#mean per cluster
gamOutputs %>%
  mutate(cluster = case_when(cluster==1 ~ 'flat',
                             cluster==2 ~ 'humped',
                             cluster==3 ~ 'open',
                             cluster==4 ~ 'forest')) %>%
  group_by(cluster,decidForest) %>%
  summarise(preds = median(preds)) %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds))+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_wrap(~cluster) +
  theme_classic()
#flat = 1
#humped = 2
#open = 3
#forest = 4
ggsave("plots/clustering_all_deriv_means.png")

#how each taxa is distributed in each cluster
gamOutputs %>%
  group_by(Taxa, cluster) %>%
  summarise(nuSpecies = length(unique(Species))) %>%
  ungroup() %>%
  mutate(cluster = case_when(cluster==1 ~ 'flat',
                             cluster==2 ~ 'humped',
                             cluster==3 ~ 'open',
                             cluster==4 ~ 'forest')) %>%
  mutate(cluster = factor(cluster, 
                          levels=c("forest","humped","flat","open"))) %>%
  ggplot() +
  geom_col(aes(x=Taxa, y = nuSpecies, fill = cluster)) +
  scale_fill_brewer(palette = "RdYlGn", direction =-1) +
  coord_flip()+
  theme_classic()

ggsave("plots/clustering_all_deriv_prop.png")

gamOutputs %>%
  select(Species, cluster) %>%
  filter(!duplicated(Species)) %>%
saveRDS(., file="outputs/clustering/deriv_classification_all.rds")

### concordance #####################################

corr_clustering <- readRDS("outputs/clustering/corr_classification_all.rds")
kmeans_clustering <- readRDS("outputs/clustering/kmeans_classification_all.rds")

all_clustering <- inner_join(corr_clustering,kmeans_clustering, by="Species")

table(all_clustering$cluster.x, all_clustering$cluster.y)

#pretty good!!

### end #############################################


