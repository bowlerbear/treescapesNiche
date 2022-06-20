### cluster-analysis #################################

library(tidyverse)

selectTaxa <-  c("Ants", "AquaticBugs","Bees","Bryophytes","Carabids","Centipedes","Craneflies",
                 "Dragonflies","E&D","Ephemeroptera","FungusGnats","Gelechiids","Hoverflies",
                 "Ladybirds","LeafSeedBeetles","Lichens","Millipedes","Molluscs","Moths","Neuropterida",
                 "Orthoptera","PlantBugs","Plecoptera","RoveBeetles","ShieldBugs","SoldierBeetles",
                 "Soldierflies","Spiders","Trichoptera","Wasps","Weevils")

### choose models ###########################

modelFolder <- "outputs/forestAssociations/broadleaf"

### all taxa #########################################

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

#### kmeans #########################################

#put into a matrix
speciesResponses <- reshape2::acast(gamOutputs,
                                    decidForest ~ Species,
                                    value.var = "preds")

dim(speciesResponses)

#scale each column
mydata <- apply(speciesResponses, 2, function(x){
  scale(boot::logit(x))})

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
fit <- kmeans(mydata, 4) # cluster solution
mydata <- data.frame(mydata, cluster = fit$cluster)
table(mydata$cluster)

#add back onto the gamOutput
gamOutputs$cluster <- mydata$cluster[match(gamOutputs$Species,
                                              row.names(mydata))]

#mean per cluster
gamOutputs %>%
  group_by(cluster,decidForest) %>%
  summarise(preds = median(preds)) %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds))+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_wrap(~cluster)

ggsave("plots/clustering_all_kmeans_means.png")

#species within
gamOutputs %>%
  group_by(Species, Taxa, cluster) %>%
  mutate(preds = scale(preds)) %>%
  ungroup %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds, groups = Species))+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_wrap(~cluster)

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

library(TSclust)

myTS <- reshape2::acast(gamOutputs,
                        Species ~ decidForest,
                        value.var = "preds")

#correlation-based distance
IP.dis <- diss(myTS, "COR")
fit <- hclust(IP.dis)
plot(fit)

#choose clusters
IP.clus <- pam(IP.dis, k = 4)$clustering
table(IP.clus)
clusterDF <- data.frame(Species = names(IP.clus), 
                        Cluster = as.numeric(IP.clus))

#add back onto the gamOutput
gamOutputs$cluster <- clusterDF$Cluster[match(gamOutputs$Species,
                                             clusterDF$Species)]

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


#mean per cluster
gamOutputs %>%
  group_by(cluster,decidForest) %>%
  summarise(preds = median(preds)) %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds))+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_wrap(~cluster)

ggsave("plots/clustering_all_deriv_means.png")

#how each taxa is distributed in each cluster
gamOutputs %>%
  group_by(Taxa, cluster) %>%
  summarise(nuSpecies = length(unique(Species))) %>%
  ungroup() %>%
  ggplot() +
  geom_col(aes(x=Taxa, y = nuSpecies, fill = factor(cluster))) +
  coord_flip()

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

### species-level ###################################

gamOutput <- readRDS("outputs/gamOutput_gam_shape_Carabids.rds")

#Summarize data for each species
gamSummary <- gamOutput %>%
  group_by(Species) %>%
  summarise(meanPreds = mean(preds),
            sdPreds = sd(preds))

#remove species were the sd is 0
gamOutput <- gamOutput %>%
  filter(Species %in% gamSummary$Species[gamSummary$sdPreds!=0])

#### kmeans #########################################

#put into a matrix
speciesResponses <- reshape2::acast(gamOutput,
                                    decidForest ~ Species,
                                    value.var = "preds")

#scale each column
mydata <- apply(speciesResponses, 2, 
                function(x){scale(boot::logit(x))})

#keep those with variation
mydata <- mydata[,!is.nan(colMeans(mydata))]

#transpose
mydata <- t(mydata)

#compare clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#pick one
fit <- kmeans(mydata, 4) # cluster solution
mydata <- data.frame(mydata, cluster=fit$cluster)
table(mydata$cluster)

#add back onto the gamOutput
gamOutput$cluster <- mydata$cluster[match(gamOutput$Species,
                                          row.names(mydata))]


#rename and reorder clusters
gamOutput <- gamOutput %>%
  mutate(cluster = fct_relevel(factor(cluster), "2","1","3","4")) %>%
  mutate(cluster_names = fct_recode(cluster,
                                    "prefers high forest (n=18)" = "4",
                                    "prefers open (n=81)" = "2",
                                    "indifferent (n=18)" = "1",
                                    "prefers low forest cover (n=32)" = "3"))

#mean per cluster
gamOutput %>%
  group_by(cluster_names,decidForest) %>%
  summarise(preds = median(preds)) %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds))+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_wrap(~cluster_names)

#species in each cluster
ggplot(gamOutput,aes(x=decidForest, y=preds, groups=Species))+
  geom_line()+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_wrap(~cluster_names)+
  theme_classic()

#plotting each cluster separately
gamOutput %>%
  filter(cluster==1) %>%
  ggplot(aes(x=decidForest, y=preds, groups=Species))+
  geom_line()+
  facet_wrap(~Species,scales="free")+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  theme_classic()

#### correlation based ##############################

library(TSclust)

myTS <- reshape2::acast(gamOutput,
                        Species ~ decidForest,
                        value.var = "preds")

#apply a transformation
#myTS <- apply(myTS, 1, boot::logit)
#sum(is.na(myTS))

#correlation-based distance
IP.dis <- diss(myTS, "COR")
fit <- hclust(IP.dis)
plot(fit)

#choose clusters
IP.clus <- pam(IP.dis, k = 4)$clustering
table(IP.clus)
clusterDF <- data.frame(Species = names(IP.clus), 
                        Cluster = as.numeric(IP.clus))

#add back onto the gamOutput
gamOutput$cluster <- clusterDF$Cluster[match(gamOutput$Species,
                                             clusterDF$Species)]

#rename and reorder clusters
gamOutput <- gamOutput %>%
  mutate(cluster = fct_relevel(factor(cluster), "3","2","1","4")) %>%
  mutate(cluster_names = fct_recode(cluster,
                                    "open specialist" = "3",
                                    "prefers open" = "2",
                                    "prefers intermediate forest cover" = "1",
                                    "prefers higher forest cover" = "4"))

#mean per cluster
gamOutput %>%
  group_by(cluster_names,decidForest) %>%
  summarise(preds = mean(preds)) %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds))+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_wrap(~cluster_names)

#species in each cluster
ggplot(gamOutput,aes(x=decidForest, y=preds, groups=Species))+
  geom_line()+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_wrap(~cluster_names)+
  theme_classic()

#plotting each cluster separately
gamOutput %>%
  filter(cluster==1) %>%
  ggplot(aes(x=decidForest, y=preds, groups=Species))+
  geom_line()+
  facet_wrap(~Species,scales="free")+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  theme_classic()

#### derivatives ######################################

#get derivatives of the fitted gam?
#best do this within the model code

library(gratia)
library(mgcv)

getDerivatives <- function(myspecies){
  
  test <- gamOutput %>%
    filter(Species == myspecies) 
  
  mod <- gam(preds ~ s(decidForest), data = test, 
             method = "REML")
  
  deriv1 <- derivatives(mod, type = "central", order=1) %>%
    add_column(Species = myspecies)
  
  return(deriv1)
  
}


#try on all carabids
allDerivatives <- lapply(sort(unique(gamOutput$Species)), function(x){
  print(x)
  getDerivatives(x)
}) %>%
  reduce(rbind)

#k means on them

speciesResponses <- reshape2::acast(allDerivatives,
                                    data ~ Species,
                                    value.var = "derivative")

#keep those with variation
mydata <- speciesResponses
mydata <- mydata[,!is.nan(colMeans(mydata))]

#transpose
mydata <- t(mydata)

#clusters
fit <- pam(mydata, 4) # cluster solution
mydata <- data.frame(mydata, cluster = fit$clustering)
table(mydata$cluster)
#most in cluster 1!!


#add back onto the gamOutput
gamOutput$cluster <- mydata$cluster[match(gamOutput$Species,
                                          row.names(mydata))]

#mean per cluster
gamOutput %>%
  group_by(cluster,decidForest) %>%
  summarise(preds = median(preds)) %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds))+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_wrap(~cluster)

#species in each cluster
ggplot(gamOutput,aes(x=decidForest, y=preds, groups=Species))+
  geom_line()+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_wrap(~cluster)+
  theme_classic()

### end ########################################################################