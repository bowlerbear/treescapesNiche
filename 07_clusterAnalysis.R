### cluster-analysis #################################

library(cowplot)
library(tidyverse)
library(cluster)
library(TSclust)
library(fpc)
theme_set(theme_classic())

selectTaxa <-  c("Ants", "AquaticBugs","Bees","Carabids","Centipedes","Craneflies",
                 "Dragonflies","E&D","Ephemeroptera","Gelechiids","Hoverflies",
                 "Ladybirds","Molluscs","Moths",
                 "Orthoptera","PlantBugs","ShieldBugs",
                 "Soldierflies","Spiders","Trichoptera","Wasps")

### useful functions ###################

source("00_functions.R")

### choose models ###########################

modelFolder <- "outputs/forestAssociations/broadleaf_subsample3"

### all taxa #########################################

gamOutputs <- getModels(modelFolder,modeltype = "mixed_shape")%>%
                rename(Species = species)

#cap at 50% - the 99% forest quartile??
summary(gamOutputs$max_forest)
#gamOutputs <- gamOutputs %>% filter(decidForest<50)

#Summarize data for each species
gamSummary <- gamOutputs %>%
  group_by(Species) %>%
  summarise(meanPreds = mean(preds),
            sdPreds = mean(preds_se))

#what prop of species show no variation
mean(gamSummary$sdPreds==0) # a few
unique(gamSummary$Species[gamSummary$sdPreds==0])

#remove species were the sd is 0 - prob too rare to be modelled
gamOutputs <- gamOutputs %>%
  filter(Species %in% gamSummary$Species[gamSummary$sdPreds>0.00000001])

#remove species with large sd
gamOutputs <- gamOutputs %>%
  filter(Species %in% 
           gamSummary$Species[gamSummary$sdPreds<outlierValue(gamSummary$sdPreds)])
nrow(gamOutputs)

length(gamSummary$Species[gamSummary$sdPreds<outlierValue(gamSummary$sdPreds)])
#5%

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

#choose k
temp <- pamk(IP.dis, krange=2:10 ,criterion="asw", critout=TRUE)

#get k with optimal clusters
fit <- pam(IP.dis, k = 3)

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
nrow(gamOutputs)#loose 1%

tapply(gamOutputs$sil_width, gamOutputs$cluster, mean)
#1         2         3 
#0.7738916 0.7749720 0.4318701

#species within
gamOutputs %>%
  group_by(Species, Taxa, cluster) %>%
  mutate(preds = scale(preds)) %>%
  ungroup %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds, group = Species))+
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
  filter(cluster==3) %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds, group = Species))+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_wrap(~Taxa)

#mean per cluster
gamOutputs %>%
    #mutate(cluster = reLabel(cluster)) %>%
    group_by(cluster,decidForest, Taxa) %>%
    summarise(preds = median(preds)) %>%
    ggplot()+
    geom_line(aes(x = decidForest, y = preds, colour=Taxa))+
    xlab("Decid forest cover %") + ylab("Occupancy")+
    facet_wrap(~cluster) +
    theme_classic() +
    theme(legend.position = "none")
ggsave("plots/clustering_all_corr_means.png")

#how each taxa is distributed in each cluster
gamOutputs %>%
    group_by(Taxa, cluster) %>%
    summarise(nuSpecies = length(unique(Species))) %>%
    ungroup() %>%
    #mutate(cluster = reLabel(cluster)) %>%
    ggplot() +
    geom_col(aes(x=Taxa, y = nuSpecies, fill = factor(cluster))) +
    ylab("Number of species") +
    scale_fill_brewer(palette = "RdYlGn", direction =-1) +
    coord_flip()+
    theme_classic()
ggsave("plots/clustering_all_corr_prop.png")

gamOutputs %>%
  select(Species, cluster) %>%
  filter(!duplicated(Species)) %>%
  saveRDS(., file="outputs/clustering/corr_classification_all.rds")

#### derivatives ######################################

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

# allDerivatives <- lapply(sort(unique(gamOutputs$Species)), function(x){
#   print(x)
#   getDerivatives(x)
# }) %>%
#    reduce(rbind)
# 
# saveRDS(allDerivatives, 
#         file="outputs/clustering/allDerivatives.rds")

#next time
allDerivatives <- readRDS("outputs/clustering/allDerivatives.rds")

#pam on them
mydata <- reshape2::acast(allDerivatives,
                                    data ~ Species,
                                    value.var = "derivative")

#transpose
mydata <- t(mydata)

#choose k
#temp <- pamk(dist(mydata,method = "manhattan"), 
#             krange=2:10 ,criterion="asw", critout=TRUE)

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
nrow(gamOutputs)#15%

tapply(gamOutputs$sil_width, gamOutputs$cluster, mean)
tapply(gamOutputs$sil_width, gamOutputs$cluster, min)
#1         2         3         4 
#0.8053582 0.7372487 0.9287358 0.760630

#species within
gamOutputs %>%
  group_by(Species, Taxa, cluster) %>%
  mutate(preds = scale(preds)) %>%
  ungroup %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds, group = Species))+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_wrap(~cluster)

gamOutputs %>%
  filter(Taxa %in% sort(unique(gamOutputs$Taxa))[1:5]) %>%
  mutate(cluster = reLabel(cluster)) %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds, group = Species))+
  xlab("Decid forest cover %") + ylab("Occupancy")+
  facet_grid(cluster~Taxa)

#mean per cluster
(fig1c <- gamOutputs %>%
  mutate(cluster = reLabel(cluster)) %>%
  group_by(cluster,decidForest, Taxa) %>%
  summarise(preds = median(preds)) %>%
  ggplot()+
  geom_line(aes(x = decidForest, y = preds, colour=Taxa))+
  xlab("Broadleaf woodland cover %") + ylab("Occupancy probability")+
  facet_wrap(~cluster) +
  theme_classic() +
  theme(legend.position = "none"))
ggsave("plots/clustering_all_deriv_means.png")

#how each taxa is distributed in each cluster?
temp <- gamOutputs %>%
  group_by(Taxa, cluster) %>%
  summarise(nuSpecies = length(unique(Species))) %>%
  ungroup() %>%
  mutate(cluster = reLabel(cluster))

#get mean to order plot by
tempMean <- temp %>%
  pivot_wider(names_from="cluster",
              values_from="nuSpecies",
              values_fill=0) %>%
  janitor::clean_names() %>%
  add_column(nuWood = (.$prefer_woodland + .$prefer_intermediate),
             nuTotal = (.$prefer_woodland + .$prefer_intermediate + .$prefer_open + .$no_strong_preferences),
             propWood = nuWood/nuTotal) %>%
  arrange(desc(prefer_woodland))
temp$Taxa <- factor(temp$Taxa, levels=tempMean$taxa)
  
(fig1d <- temp %>%   
   ggplot() +
    geom_col(aes(x=Taxa, y = nuSpecies, fill = cluster)) +
    ylab("Number of species") +
  ylab("Number of species") +
    xlab("Taxa (ordered by # of woodland species)") +
  scale_fill_brewer("Broadleaf cluster",palette = "RdYlGn", direction =-1) +
  coord_flip()+
  theme(axis.text = element_text(size=8),
        legend.position=c(0.77,0.8)))
ggsave("plots/clustering_all_deriv_prop.png")

gamOutputs %>%
  select(Species, cluster) %>%
  filter(!duplicated(Species)) %>%
saveRDS(., file="outputs/clustering/deriv_classification_all.rds")

### fig1 #######################################

fig1a <- readRDS("plots/fig1a.rds")
fig1a <- fig1a + theme_classic() +
  theme(legend.position="none",
        axis.text.y = element_text(size=8))

fig1b <- readRDS("plots/fig1b.rds")
fig1b <- fig1b + theme_classic() +
  theme(legend.position="none")

fig1ab <- plot_grid(fig1b, fig1a, nrow=2, labels=c("a", "b"))

fig1cd <- plot_grid(fig1c, fig1d, nrow=2, labels=c("c", "d"))

plot_grid(fig1ab, fig1cd, ncol=2, 
          labels=c("Linear", "Non-linear"),
          scale=c(0.95,0.95))

ggsave("plots/fig1.png", height=8, width=8)

### summary stats ###################################

gamOutputs %>%
  mutate(cluster = reLabel(cluster)) %>%
  select(Species, Taxa,cluster) %>%
  filter(!duplicated(Species)) %>%
  group_by(cluster) %>%
  count()


### clusters vs linear ###############################

clusterDF <- readRDS("outputs/clustering/deriv_classification_all.rds") %>%
                  rename(species = Species)

linearDF <- readRDS("outputs/forestAssociations/broadleafAssocations_mixed_linear.rds") 

allDF <- inner_join(clusterDF, linearDF, by="species") %>%
            mutate(cluster = reLabel(cluster))
  
allDF %>%
  filter(estimate>(-0.2)) %>%
ggplot()+
  geom_violin(aes(x=cluster, y=estimate)) +
  geom_hline(yintercept=0, linetype="dashed") +
  xlab("Broadleaf woodland association  cluster") +
  ylab("Linear broadlead woodland association estimate")


lm1 <- lm(estimate ~ cluster, data=allDF)
summary(lm1)

head(allDF)

#how many had significant positive association but were placed in the no strong trends

### concordance #####################################

# corr_clustering <- readRDS("outputs/clustering/corr_classification_all.rds")
# deriv_clustering <- readRDS("outputs/clustering/deriv_classification_all.rds")
# all_clustering <- inner_join(corr_clustering, deriv_clustering, by="Species")
# 
# table(all_clustering$cluster.x, all_clustering$cluster.y)
# 
# #plots of the differences
# 
# #corr - 1 is mostly flat, 
# all_clustering %>%
#   filter(cluster.x=="1") %>%
#   left_join(gamOutputs) %>%
#   ggplot()+
#   geom_line(aes(x = decidForest, y = preds, groups = Species))+
#   xlab("Decid forest cover %") + ylab("Occupancy")+
#   facet_wrap(~cluster.y)
# #many are put in the flat category based on the derivs
# 
# #corr - 2 is forest, 
# all_clustering %>%
#         filter(cluster.x=="2") %>%
#         left_join(gamOutputs) %>%
#   ggplot()+
#   geom_line(aes(x = decidForest, y = preds, groups = Species))+
#   xlab("Decid forest cover %") + ylab("Occupancy")+
#   facet_wrap(~cluster.y)
# #many are put in the flat category based on the derivs 
# #but they seem to only increase slightly with forest, so seems ok
# 
# #corr - 3 is open
# all_clustering %>%
#   filter(cluster.x=="3") %>%
#   left_join(gamOutputs) %>%
#   ggplot()+
#   geom_line(aes(x = decidForest, y = preds, groups = Species))+
#   xlab("Decid forest cover %") + ylab("Occupancy")+
#   facet_wrap(~cluster.y)
# #most put in flat category based on derivs
# 
# #corr - 4 is humped
# all_clustering %>%
#   filter(cluster.x=="4") %>%
#   left_join(gamOutputs) %>%
#   ggplot()+
#   geom_line(aes(x = decidForest, y = preds, groups = Species))+
#   xlab("Decid forest cover %") + ylab("Occupancy")+
#   facet_wrap(~cluster.y)
# #most also humped according to the derivatives
# #few are put into the forest cover based on derivs 
# #but these are only slighty humped
# 
# #deriv - 1 is open
# all_clustering %>%
#   filter(cluster.y=="1") %>%
#   left_join(gamOutputs) %>%
#   ggplot()+
#   geom_line(aes(x = decidForest, y = preds, groups = Species))+
#   xlab("Decid forest cover %") + ylab("Occupancy")+
#   facet_wrap(~cluster.x)
# #most put in open
# 
# #deriv - 2 is flat
# all_clustering %>%
#   filter(cluster.y=="2") %>%
#   left_join(gamOutputs) %>%
#   ggplot()+
#   geom_line(aes(x = decidForest, y = preds, groups = Species))+
#   xlab("Decid forest cover %") + ylab("Occupancy")+
#   facet_wrap(~cluster.x)
# #mostly flat
# 
# #deriv - 3 is forest
# all_clustering %>%
#   filter(cluster.y=="3") %>%
#   left_join(gamOutputs) %>%
#   ggplot()+
#   geom_line(aes(x = decidForest, y = preds, groups = Species))+
#   xlab("Decid forest cover %") + ylab("Occupancy")+
#   facet_wrap(~cluster.x)
# #all in category 2 of corr
# 
# #deriv - 4 is humped
# all_clustering %>%
#   filter(cluster.y=="4") %>%
#   left_join(gamOutputs) %>%
#   ggplot()+
#   geom_line(aes(x = decidForest, y = preds, groups = Species))+
#   xlab("Decid forest cover %") + ylab("Occupancy")+
#   facet_wrap(~cluster.x)
# #humped seems reasonable for all

### end #############################################


