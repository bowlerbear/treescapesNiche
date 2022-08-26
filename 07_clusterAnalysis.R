### cluster-analysis #################################

library(cowplot)
library(tidyverse)
library(cluster)
library(TSclust)

selectTaxa <-  c("Ants", "AquaticBugs","Bees","Carabids","Centipedes","Craneflies",
                 "Dragonflies","E&D","Ephemeroptera","Gelechiids","Hoverflies",
                 "Ladybirds","Molluscs","Moths",
                 "Orthoptera","PlantBugs","ShieldBugs",
                 "Soldierflies","Spiders","Trichoptera","Wasps")

### useful functions ###################

source("00_functions.R")

### choose models ###########################

modelFolder <- "outputs/forestAssociations/broadleaf_subsample3"

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
  mutate(Taxa = strsplit(source,"_")[[1]][8]) %>%
  ungroup() %>%
  filter(Taxa %in% selectTaxa) %>%
  mutate(Taxa = case_when(Taxa=="E&D" ~ "Empidids",
                          TRUE ~ as.character(Taxa)))  %>%
  rename(Species = species,
         decidForest = decid_forest)

#cap at 50% - the 99% forest quartile
gamOutputs <- gamOutputs %>% filter(decidForest<50)

#Summarize data for each species
gamSummary <- gamOutputs %>%
  group_by(Species) %>%
  summarise(meanPreds = mean(preds),
            sdPreds = sd(preds))

#what prop of species show no variation

mean(gamSummary$sdPreds==0) # none

#remove species were the sd is 0 - prob too rare to be modelled
gamOutputs <- gamOutputs %>%
  filter(Species %in% gamSummary$Species[gamSummary$sdPreds!=0])

#remove species with large sd
gamOutputs <- gamOutputs %>%
  filter(Species %in% 
           gamSummary$Species[gamSummary$sdPreds<outlierValue(gamSummary$sdPreds)])
nrow(gamOutputs)

#### pam #########################################

# #clusters both the height and the shape of the niche
# #scale them relative to mean occupancy
# 
# #put into a matrix
# speciesResponses <- reshape2::acast(gamOutputs,
#                                     decidForest ~ Species,
#                                     value.var = "preds")
# 
# dim(speciesResponses)
# 
# #scale each column 
# mydata <- apply(speciesResponses, 2, function(x){
#   boot::logit(x)
# })
# 
# #transpose
# mydata <- t(mydata)
# 
# #compare clusters
# wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
# for (i in 2:15) wss[i] <- sum(kmeans(mydata,
#                                      centers=i)$withinss)
# plot(1:15, wss, type="b", xlab="Number of Clusters",
#      ylab="Within groups sum of squares")
# 
# #pick one
# fit <- pam(mydata, 4, metric="manhattan") # cluster solution
# mydata <- data.frame(mydata, cluster = fit$clustering)
# table(mydata$cluster)
# 
# #get silhouette values
# ss <- silhouette(mydata$cluster, 
#                  dist(mydata[,-which(names(mydata)=="cluster")],
#                       method = "manhattan"))
# mydata$sil_width <- ss[,"sil_width"]
# 
# 
# #add back onto the gamOutput
# gamOutputs <-  mydata %>%
#                 select(cluster, sil_width) %>%
#                 add_column(Species = row.names(mydata)) %>%
#                 inner_join(.,gamOutputs)
# nrow(gamOutputs)
# #drop those whose siloutte width was negative
# gamOutputs <- gamOutputs %>%
#                 filter(sil_width >0.05)
# nrow(gamOutputs)#loose 10%
# 
# tapply(gamOutputs$sil_width, gamOutputs$cluster, mean)
# #1         2         3         4 
# #0.4772783 0.4879862 0.4128816 0.3617228
# 
# #species within
# gamOutputs %>%
#   ggplot()+
#   geom_line(aes(x = decidForest, y = preds, groups = Species))+
#   xlab("Decid forest cover %") + ylab("Occupancy")+
#   facet_wrap(~cluster)
# 
# gamOutputs %>%
#   ggplot()+
#   geom_line(aes(x = decidForest, y = preds, groups = Species, colour=factor(cluster)))+
#   xlab("Decid forest cover %") + ylab("Occupancy")+
#   facet_wrap(~Taxa, scales="free")
# 
# gamOutputs %>%
#   filter(Taxa %in% sort(unique(gamOutputs$Taxa))[6:10]) %>%
#   ggplot()+
#   geom_line(aes(x = decidForest, y = preds, groups = Species))+
#   xlab("Decid forest cover %") + ylab("Occupancy")+
#   facet_wrap(cluster~Taxa, scales="free")
# 
# #mean per cluster
# gamOutputs %>%
#   group_by(cluster,decidForest) %>%
#   summarise(preds = median(preds)) %>%
#   ggplot()+
#   geom_line(aes(x = decidForest, y = preds))+
#   xlab("Decid forest cover %") + ylab("Occupancy")+
#   facet_wrap(~cluster)
# 
# ggsave("plots/clustering_all_pam_means.png")
# 
# #how each taxa is distributed in each cluster
# gamOutputs %>%
#   group_by(Taxa, cluster) %>%
#   summarise(nuSpecies = length(unique(Species))) %>%
#   ungroup() %>%
# ggplot() +
#   geom_col(aes(x=Taxa, y = nuSpecies, fill = factor(cluster))) +
#   coord_flip()
# 
# ggsave("plots/clustering_all_pam_prop.png")
# 
# #save classification
# gamOutputs %>%
#   select(Species, cluster) %>%
#   filter(!duplicated(Species)) %>%
#   saveRDS(., file="outputs/clustering/pam_classification_all.rds")

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
nrow(gamOutputs)#loose 9%

tapply(gamOutputs$sil_width, gamOutputs$cluster, mean)
#1         2         3         4 
#0.7729179 0.5111890 0.7990487 0.4617364

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
  filter(cluster==4) %>%
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
  
  #formal gam
  mod <- gam(preds ~ s(decidForest), data = test,
             method = "REML")
  test$newPreds <- predict(mod)
  
  #ggplot(test)+
  #  geom_point(aes(x=decidForest, y=preds))+
  #  geom_line(aes(x=decidForest, y=newPreds))
  
  deriv1 <- derivatives(mod, type = "central", order=1) %>%
    add_column(Species = myspecies,
               meanPred = mean(test$preds),
               cor = cor(test$newPreds, test$preds))

  return(deriv1)

}

allDerivatives1 <- lapply(sort(unique(gamOutputs$Species)), function(x){
  #print(x)
  getDerivatives(x)
}) %>%
   reduce(rbind)

saveRDS(allDerivatives, 
        file="outputs/clustering/allDerivatives.rds")

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
ss <- silhouette(mydata$cluster, dist(mydata[,-which(names(mydata)=="cluster")],
                                      method="manhattan"))
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
nrow(gamOutputs)#lose 9%

tapply(gamOutputs$sil_width, gamOutputs$cluster, mean)
tapply(gamOutputs$sil_width, gamOutputs$cluster, min)
#1         2         3         4 
#0.5717501 0.4168141 0.6368796 0.3519009

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
  filter(Taxa %in% sort(unique(gamOutputs$Taxa))[1:5]) %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds, groups = Species))+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_grid(cluster~Taxa)

#mean per cluster
(fig1b <- gamOutputs %>%
  mutate(cluster = reLabel(cluster)) %>%
  group_by(cluster,decidForest, Taxa) %>%
  summarise(preds = median(preds)) %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds, colour=Taxa))+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_wrap(~cluster) +
  theme_classic() +
  theme(legend.position = "none"))
ggsave("plots/clustering_all_deriv_means.png")

#how each taxa is distributed in each cluster
(fig1c <- gamOutputs %>%
  group_by(Taxa, cluster) %>%
  summarise(nuSpecies = length(unique(Species))) %>%
  ungroup() %>%
  mutate(cluster = reLabel(cluster)) %>%
  ggplot() +
  geom_col(aes(x=Taxa, y = nuSpecies, fill = cluster)) +
  ylab("Number of species") +
  scale_fill_brewer(palette = "RdYlGn", direction =-1) +
  coord_flip()+
  theme_classic())
ggsave("plots/clustering_all_deriv_prop.png")

gamOutputs %>%
  select(Species, cluster) %>%
  filter(!duplicated(Species)) %>%
saveRDS(., file="outputs/clustering/deriv_classification_all.rds")

### fig1 #######################################

fig1a <- readRDS("plots/fig1a.rds")
fig1a <- fig1a + theme_classic() +
  theme(legend.position="none")

fig1bc <- plot_grid(fig1b, fig1c, nrow=2, labels=c("b", "c"))

plot_grid(fig1a, fig1bc, ncol=2, labels=c("a",""))

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

#corr - 3 is open
all_clustering %>%
  filter(cluster.x=="3") %>%
  left_join(gamOutputs) %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds, groups = Species))+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_wrap(~cluster.y)
#most put in flat category based on derivs

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

#deriv - 1 is open
all_clustering %>%
  filter(cluster.y=="1") %>%
  left_join(gamOutputs) %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds, groups = Species))+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_wrap(~cluster.x)
#most put in open

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
#all in category 2 of corr

#deriv - 4 is humped
all_clustering %>%
  filter(cluster.y=="4") %>%
  left_join(gamOutputs) %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds, groups = Species))+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_wrap(~cluster.x)
#humped seems reasonable for all

### end #############################################


