### species-level trends vs forest traits ####

library(tidyverse)
library(ggridges)
theme_set(theme_classic())

selectTaxa <-  c("Ants", "AquaticBugs","Bees","Carabids","Centipedes","Craneflies",
                 "Dragonflies","E&D","Ephemeroptera","Gelechiids","Hoverflies",
                 "Ladybirds","LeafSeedBeetles","Molluscs","Moths",
                 "Orthoptera","PlantBugs","ShieldBugs",
                 "Soldierflies","Spiders","Trichoptera","Wasps")

### choose models #######################

#trends
trendsFolder <- "outputs/speciesTrends"

#forest associations
forestFolder <- "outputs/forestAssociations/broadleaf"

### species trends ##############################

spTrends <- list.files(trendsFolder, full.names = TRUE) %>%
              set_names() %>%
              map_dfr(readRDS, .id="source") %>%
              group_by(source) %>%
              rename(Taxa = taxon) %>%
              filter(Taxa %in% selectTaxa) 
              
table(spTrends$Taxa)

spTrends %>%
  ggplot() +
  geom_density_ridges(aes(x = mean_change, y = Taxa, fill=Taxa),
                      rel_min_height = 0.001) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none") +
  xlab("Long-term distribution trends")

#filter those with large uncertainties

hist(spTrends$sd_change)
summary(spTrends$sd_change)

spTrends <- spTrends %>%
              filter(sd_change < 2)

### cont forest preferences ###################

gamOutputs <- list.files(forestFolder,full.names=TRUE) %>%
                  str_subset("gamOutput_gamm_subset_random_") %>%
                  set_names() %>%
                  map_dfr(readRDS, .id="source") %>%
                  group_by(source) %>%
                  mutate(Taxa = strsplit(source,"_")[[1]][6]) %>%
                  ungroup() %>%
                  filter(Taxa %in% selectTaxa) %>%
                  mutate(species = tolower(species),
                         forest_assoc = estimate) %>%
                  filter(std_error<0.01)
     
quantile(gamOutputs$std_error,0.95)

df <- inner_join(spTrends, gamOutputs, by = c("species","Taxa"))

ggplot(df) +
  geom_point(aes(x = forest_assoc, y = mean_change, colour=Taxa)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_vline(xintercept = 0, linetype="dashed") +
  theme_classic()

ggplot(df) +
  geom_point(aes(x = forest_assoc, y = mean_change)) +
  facet_wrap(~Taxa) +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_vline(xintercept = 0, linetype="dashed") 

(g1 <- ggplot(df) +
  geom_bin2d(aes(x = forest_assoc, y = mean_change)) +
  scale_fill_viridis_c("number of species") +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_vline(xintercept = 0, linetype="dashed") +
  xlab("Forest association") + ylab("Species growth rate")+
  xlim(-0.2,0.15)+
  theme(legend.position = "top"))

ggsave("plots/forestVStrends.png",width=5, height=3.5)

### cluster differences ############################

clusterDF <- readRDS("outputs/clustering/deriv_classification_all.rds") %>%
                janitor::clean_names() %>%
                mutate(species = tolower(species))

#merge with trends
spTrends <- spTrends %>%
              inner_join(.,clusterDF) 

ggplot(spTrends) +
  geom_boxplot(aes(x = Taxa, y = mean_change)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  coord_flip() +
  facet_wrap(~cluster,nrow=1)

ggplot(spTrends) +
  geom_boxplot(aes(x = Taxa, y = mean_initial)) +
  coord_flip() +
  facet_wrap(~cluster,nrow=1)

ggplot(spTrends)+
  geom_point(aes(x=mean_initial, y = mean_change))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  facet_wrap(~cluster)

(g2 <- spTrends %>%
  mutate(cluster = case_when(cluster==1 ~ 'prefer open',
                             cluster==2 ~ 'weak preference',
                             cluster==3 ~ 'prefer forest',
                             cluster==4 ~ 'prefer intermediate')) %>%
  mutate(cluster = factor(cluster, 
                          levels=c("prefer forest","prefer intermediate","weak preference",
                                   "prefer open"))) %>%
ggplot() +
  geom_boxplot(aes(x = cluster, y = mean_change), draw_quantiles = c(0.25,0.5,0.75)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  ylab("Species growth rate") +
  coord_flip())

ggsave("plots/forest_vs_cluster.png",width=5, height=4)

### taxa-level ###################

(g3 <- spTrends %>%
  mutate(cluster = case_when(cluster==1 ~ 'prefer open',
                             cluster==2 ~ 'weak preference',
                             cluster==3 ~ 'prefer forest',
                             cluster==4 ~ 'prefer intermediate')) %>%
  mutate(cluster = factor(cluster, 
                          levels=c("prefer forest","prefer intermediate","weak preference",
                                   "prefer open"))) %>%
   filter(cluster=="prefer forest") %>%
ggplot() +
  geom_boxplot(aes(x = Taxa, y = mean_change), 
               outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype="dashed") +
  coord_flip() +
  scale_fill_brewer(palette = "RdYlGn", direction =-1) +
  ylab("Forest species growth rate") +
  theme(legend.position = "top"))

#ggsave("plots/forest_species_vs_cluster.png",width=6, height=7)

(g4 <- spTrends %>%
    mutate(cluster = case_when(cluster==1 ~ 'prefer open',
                               cluster==2 ~ 'weak preference',
                               cluster==3 ~ 'prefer forest',
                               cluster==4 ~ 'prefer intermediate')) %>%
    mutate(cluster = factor(cluster, 
                            levels=c("prefer forest","prefer intermediate","weak preference",
                                     "prefer open"))) %>%
    filter(cluster=="prefer open") %>%
    ggplot() +
    geom_boxplot(aes(x = Taxa, y = mean_change), 
                 outlier.shape = NA) +
    geom_hline(yintercept = 0, linetype="dashed") +
    coord_flip() +
    scale_fill_brewer(palette = "RdYlGn", direction =-1) +
    ylab("Open habitat species growth rate") +
    theme(legend.position = "top"))

### plotting ####################

fig3ab <- plot_grid(g1,g2, nrow=1, labels=c("a", "b"))
fig3cd <- plot_grid(g3,g4, nrow=1, labels=c("c", "d"))

plot_grid(fig3ab, fig3cd, ncol=1)

### forest specialisation ###############

forestSpecial <- readRDS("outputs/forestAssociations/forestSpecial_mixed_linear.rds") %>%
                    mutate(species = tolower(species))

#preference for broadleaf
summary(forestSpecial$ForestSpecial[forestSpecial$ForestSpecial>0])

#preference for coniferous
summary(forestSpecial$ForestSpecial[forestSpecial$ForestSpecial<0])

#create categories
forestSpecial$forestType <- ifelse(forestSpecial$ForestSpecial>2, "Broadleaf",
                                   ifelse(forestSpecial$ForestSpecial<(-2), "Conif",
                                   "Indifferent"))
table(forestSpecial$forestType)


#using discrete classification
typeTrends <- inner_join(spTrends, forestSpecial) %>%
                  filter(forestType %in% c("Broadleaf", "Conif"))

(g1 <- ggplot(typeTrends) +
  geom_boxplot(aes(x = forestType, y = mean_change)) +
  geom_hline(yintercept=0, linetype="dashed")+
  ylab("Species growth rate"))

(g2 <- ggplot(typeTrends) +
  geom_boxplot(aes(x = Taxa, y = mean_change, fill=forestType)) +
  coord_flip() +
  geom_hline(yintercept=0, linetype="dashed") +
  theme(legend.position="top")+
    ylab("Species growth rate")) 

plot_grid(g1,g2, nrow=1, labels=c("a","b"))

#using continuous measure
typeTrends <- inner_join(spTrends, forestSpecial)
summary(typeTrends$ForestSpecial)

#cap extremes
upperQuant <- quantile(typeTrends$ForestSpecial,0.99) 
typeTrends$ForestSpecial[typeTrends$ForestSpecial>upperQuant] <- upperQuant

lowerQuant <- quantile(typeTrends$ForestSpecial,0.01) 
typeTrends$ForestSpecial[typeTrends$ForestSpecial<lowerQuant] <- lowerQuant

#get uncertainty info
typeTrends$FS_sd <- typeTrends$std_error_broadleaf + typeTrends$std_error_conif

ggplot(typeTrends, aes(x=ForestSpecial, y = mean_change, size = 1/FS_sd)) +
  geom_point()+
  stat_smooth(method="gam")+
  geom_hline(yintercept=0, linetype="dashed")+
  theme(legend.position = "none")

### end ####################################