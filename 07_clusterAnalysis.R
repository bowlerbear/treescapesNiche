### cluster-analysis #################################

library(tidyverse)
library(cluster)
library(TSclust)

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
  str_subset("gamOutput_gamm_shape_subset_random_") %>%
  set_names() %>%
  map_dfr(readRDS, .id="source") %>%
  group_by(source) %>%
  mutate(Taxa = strsplit(source,"_")[[1]][7]) %>%
  ungroup() %>%
  filter(Taxa %in% selectTaxa) %>%
  mutate(Taxa = case_when(Taxa=="E&D" ~ "Empidid",
                          TRUE ~ as.character(Taxa)))  %>%
  rename(Species = species,
         decidForest = decid_forest)

#Summarize data for each species
gamSummary <- gamOutputs %>%
  group_by(Species) %>%
  summarise(meanPreds = mean(preds),
            sdPreds = sd(preds))

#what prop of species show no variation

mean(gamSummary$sdPreds==0) # few!! 10 species

#remove species were the sd is 0 - prob too rare to be modelled
gamOutputs <- gamOutputs %>%
  filter(Species %in% gamSummary$Species[gamSummary$sdPreds!=0])

#remove species with large sd

quantile(gamSummary$sdPreds, 0.95)
mean(gamSummary$sdPreds>0.1)

gamOutputs <- gamOutputs %>%
  filter(Species %in% gamSummary$Species[gamSummary$sdPreds<0.1])
nrow(gamOutputs)

#### pam #########################################

#clusters both the height and the shape of the niche
#scale them relative to mean occupancy

#put into a matrix
speciesResponses <- reshape2::acast(gamOutputs,
                                    decidForest ~ Species,
                                    value.var = "preds")

dim(speciesResponses)

#scale each column 
mydata <- apply(speciesResponses, 2, function(x){
  boot::logit(x)
})

#transpose
mydata <- t(mydata)

#compare clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#pick one
fit <- pam(mydata, 4, metric="manhattan") # cluster solution
mydata <- data.frame(mydata, cluster = fit$clustering)
table(mydata$cluster)

#get silhouette values
ss <- silhouette(mydata$cluster, 
                 dist(mydata[,-which(names(mydata)=="cluster")],
                      method = "manhattan"))
mydata$sil_width <- ss[,"sil_width"]


#add back onto the gamOutput
gamOutputs <-  mydata %>%
                select(cluster, sil_width) %>%
                add_column(Species = row.names(mydata)) %>%
                inner_join(.,gamOutputs)
nrow(gamOutputs)
#drop those whose siloutte width was negative
gamOutputs <- gamOutputs %>%
                filter(sil_width >0.05)
nrow(gamOutputs)#loose 10%

tapply(gamOutputs$sil_width, gamOutputs$cluster, mean)
#1         2         3         4 
#0.4772783 0.4879862 0.4128816 0.3617228

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

ggsave("plots/clustering_all_pam_means.png")

#how each taxa is distributed in each cluster
gamOutputs %>%
  group_by(Taxa, cluster) %>%
  summarise(nuSpecies = length(unique(Species))) %>%
  ungroup() %>%
ggplot() +
  geom_col(aes(x=Taxa, y = nuSpecies, fill = factor(cluster))) +
  coord_flip()

ggsave("plots/clustering_all_pam_prop.png")

#save classification
gamOutputs %>%
  select(Species, cluster) %>%
  filter(!duplicated(Species)) %>%
  saveRDS(., file="outputs/clustering/pam_classification_all.rds")

#### correlation based ##############################

#this one captures the shape but not the magnitude of change

mydata <- reshape2::acast(gamOutputs,
                        Species ~ decidForest,
                        value.var = "preds")

#correlation-based distance
IP.dis <- diss(mydata, "COR")
fit <- pam(IP.dis, k = 4, metric="manhattan")

#add on to response dataframe
mydata <- data.frame(mydata, cluster = fit$clustering)
table(mydata$cluster)

#get silhouette values
ss <- silhouette(mydata$cluster, IP.dis)
mydata$sil_width <- ss[,"sil_width"]

#add back onto the gamOutput
gamOutputs <-  mydata %>%
  select(cluster, sil_width) %>%
  add_column(Species = row.names(mydata)) %>%
  inner_join(.,gamOutputs)
nrow(gamOutputs)

#drop those whose siloutte width was negative
gamOutputs <- gamOutputs %>%
  filter(sil_width >0.05)
nrow(gamOutputs)#loose 10%

tapply(gamOutputs$sil_width, gamOutputs$cluster, mean)
#1         2         3         4 
#0.3889281 0.8118945 0.6423463 0.5319811

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

#all those in a specific clusters
gamOutputs %>%
  filter(cluster==2) %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds, groups = Species))+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_wrap(~Taxa)

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

# posthoc

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
  #print(x)
  getDerivatives(x)
}) %>%
   reduce(rbind)

saveRDS(allDerivatives, 
        file="outputs/clustering/allDerivatives.rds")

# # derivatives taken directly from fitted model
# 
# allDerivatives <- list.files(modelFolder,full.names=TRUE) %>%
#   str_subset("gamm_derivatives") %>%
#   set_names() %>%
#   map_dfr(readRDS, .id="source") %>%
#   group_by(source) %>%
#   mutate(Taxa = strsplit(source, "gamm_derivatives_subset_random_decid_")[[1]][2]) %>%
#   mutate(Taxa = strsplit(Taxa,"_")[[1]][1]) %>%
#   ungroup() %>%
#   filter(Taxa %in% selectTaxa) %>%
#   mutate(Taxa = case_when(Taxa=="E&D" ~ "Empidid",
#                           TRUE ~ as.character(Taxa)))
# #filter extremes?? 
# temp <- subset(allDerivatives, deriv)

#continue here:

allDerivatives <- readRDS("outputs/clustering/allDerivatives.rds")

#pam on them
mydata <- reshape2::acast(allDerivatives,
                                    data ~ Species,
                                    value.var = "derivative")

#transpose
mydata <- t(mydata)

#clusters
fit <- pam(mydata, 4, metric="manhattan")

#add on to response dataframe
mydata <- data.frame(mydata, cluster = fit$clustering)
table(mydata$cluster)

#get silhouette values
ss <- silhouette(mydata$cluster, dist(mydata, method="manhattan"))
mydata$sil_width <- ss[,"sil_width"]

#add back onto the gamOutput
gamOutputs <-  mydata %>%
  select(cluster, sil_width) %>%
  add_column(Species = row.names(mydata)) %>%
  inner_join(.,gamOutputs)
nrow(gamOutputs)

#drop those whose siloutte width was negative
gamOutputs <- gamOutputs %>%
  filter(sil_width >0.05)
nrow(gamOutputs)#loose 0%

#better clustering!!!
tapply(gamOutputs$sil_width, gamOutputs$cluster, mean)
tapply(gamOutputs$sil_width, gamOutputs$cluster, min)
#1         2         3         4 
#0.8817067 0.9465818 0.8045520 0.8061120 

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
  mutate(cluster = case_when(cluster==1 ~ 'open',
                             cluster==2 ~ 'flat',
                             cluster==3 ~ 'forest',
                             cluster==4 ~ 'humped')) %>%
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
  mutate(cluster = case_when(cluster==1 ~ 'open',
                             cluster==2 ~ 'flat',
                             cluster==3 ~ 'forest',
                             cluster==4 ~ 'humped')) %>%
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
deriv_clustering <- readRDS("outputs/clustering/deriv_classification_all.rds")

all_clustering <- inner_join(corr_clustering, deriv_clustering, by="Species")

table(all_clustering$cluster.x, all_clustering$cluster.y)

#plots of the differences

#corr - 1 is mostly flat, 
all_clustering %>%
  filter(cluster.x=="1") %>%
  left_join(gamOutputs) %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds, groups = Species))+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_wrap(~cluster.y)
#many are put in the flat category based on the derivs

#corr - 2 is forest, 
all_clustering %>%
        filter(cluster.x=="2") %>%
        left_join(gamOutputs) %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds, groups = Species))+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_wrap(~cluster.y)
#many are put in the flat category based on the derivs 
#but they seem to only increase slightly with forest, so seems ok

#corr - 4 is humped
all_clustering %>%
  filter(cluster.x=="4") %>%
  left_join(gamOutputs) %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds, groups = Species))+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_wrap(~cluster.y)
#most also humped according to the derivatives
#few are put into the forest cover based on derivs 
#but these are only slighty humped

#deriv - 2 is flat
all_clustering %>%
  filter(cluster.y=="2") %>%
  left_join(gamOutputs) %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds, groups = Species))+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_wrap(~cluster.x)
#mostly flat

#deriv - 3 is forest
all_clustering %>%
  filter(cluster.y=="3") %>%
  left_join(gamOutputs) %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds, groups = Species))+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_wrap(~cluster.x)

# a few are U-shaped??
outliers <- all_clustering$Species[all_clustering$cluster.y==3 &
                         all_clustering$cluster.x==1]

#check pattern without these
all_clustering %>%
  filter(cluster.y=="3") %>%
  left_join(gamOutputs) %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds, groups = Species))+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_wrap(~Taxa)

deriv_clustering %>%
  filter(!Species %in% outliers) %>%
saveRDS(.,"outputs/clustering/deriv_classification_all.rds")

#deriv - 4 is humped
all_clustering %>%
  filter(cluster.y=="4") %>%
  left_join(gamOutputs) %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds, groups = Species))+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_wrap(~cluster.x)
#humped seems reasonable

### end #############################################


