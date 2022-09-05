### species-level trends vs forest traits ####

library(metafor)
library(tidyverse)
library(ggridges)
theme_set(theme_classic())

selectTaxa <-  c("Ants", "AquaticBugs","Bees","Carabids","Centipedes","Craneflies",
                 "Dragonflies","E&D","Ephemeroptera","Gelechiids","Hoverflies",
                 "Ladybirds","Molluscs","Moths",
                 "Orthoptera","PlantBugs","ShieldBugs",
                 "Soldierflies","Spiders","Trichoptera","Wasps")

### useful functions ###################

source("00_functions.R")

### choose models #######################

#trends
trendsFolder <- "outputs/speciesTrends"

#forest associations
forestFolder <- "outputs/forestAssociations/broadleaf_subsample2"

### species trends ##############################

spTrends <- list.files(trendsFolder, full.names = TRUE) %>%
              set_names() %>%
              map_dfr(readRDS, .id="source") %>%
              group_by(source) %>%
              rename(Taxa = taxon) %>%
              filter(Taxa %in% selectTaxa) 
              
table(spTrends$Taxa)

#filter those with large uncertainties

hist(spTrends$sd_change)
summary(spTrends$sd_change)

spTrends <- spTrends %>%
  filter(sd_change < outlierValue(spTrends$sd_change))

#plot trends by taxa
spTrends %>%
  ggplot() +
  geom_density_ridges(aes(x = mean_change, y = Taxa, fill=Taxa),
                      rel_min_height = 0.001) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(legend.position = "none") +
  xlab("Long-term distribution trends")

### cont forest pref ###################

gamOutputs <- getModels(forestFolder,modeltype = "mixed_linear") %>%
                  mutate(species = tolower(species),
                         forest_assoc = estimate) %>%
                  filter(std_error < outlierValue(std_error))

summary(gamOutputs$std_error)
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
  xlim(-0.075,0.075)+
  theme(legend.position = "top"))

ggsave("plots/forestVStrends.png",width=5, height=3.5)

#### testing #####

summary(lm(mean_change ~ forest_assoc, data=df))

rma.mv(mean_change, sd_change^2, mods= ~ forest_assoc,
       random = ~ 1 | Taxa, data = df)

### signif forest pref ####################

gamOutputs$forestPref <- ifelse(gamOutputs$estimate>0 & gamOutputs$pr_t<0.05, "positive",
                           ifelse(gamOutputs$estimate<0 & gamOutputs$pr_t<0.05, "negative",
                                  "none"))
table(gamOutputs$forestPref)

df <- inner_join(spTrends, gamOutputs, by = c("species","Taxa"))

#across all
ggplot(df, aes(x= forestPref , y = median_change)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha=0.2, colour="lightgrey") +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Forest association") + ylab("Species growth rate") 

#for each taxa
ggplot(df, aes(x= Taxa, y = median_change)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha=0.2, colour="lightgrey") +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Taxa") + ylab("Species growth rate") +
  facet_wrap(~Trend)+
  coord_flip()

#### testing #####

rma.mv(mean_change, sd_change^2, mods= ~ Trend,
       random = ~ 1 | Taxa, data = df)

#positive slightly higher than negative

rma.mv(mean_change, sd_change^2, mods= ~ Trend -1,
       random = ~ 1 | Taxa, data = df)

### WoL trends #############################

df %>%
  mutate(direction = ifelse(lowerCI_change>0,"increase",
                            ifelse(upperCI_change<0,"decrease",
                                   ifelse(mean_change<0 & upperCI_change>0,"non-sig decrease",
                                          "non-sig increase")))) %>%
  mutate(direction = factor(direction,
                            levels = c("decrease", "non-sig decrease", "non-sig increase", "increase"))) %>%
  group_by(forestPref, direction) %>%
  summarise(nuSpecies = length(unique(species))) %>%
  ggplot() +
  geom_col(aes(x=forestPref, y = nuSpecies, fill=direction)) +
  scale_fill_brewer("Distribution trend", type="div") +
  ylab("Number of species") +
  coord_flip()

### cluster differences ############################

clusterDF <- readRDS("outputs/clustering/deriv_classification_all.rds") %>%
                janitor::clean_names() %>%
                mutate(species = tolower(species))

#merge with trends
df <- spTrends %>%
              inner_join(.,clusterDF) 

#### cross-taxa #####

ggplot(df) +
  geom_boxplot(aes(x = Taxa, y = mean_change)) +
  geom_hline(yintercept = 0, linetype="dashed") +
  coord_flip() +
  facet_wrap(~cluster,nrow=1)

ggplot(df) +
  geom_boxplot(aes(x = Taxa, y = mean_initial)) +
  coord_flip() +
  facet_wrap(~cluster,nrow=1)

ggplot(df)+
  geom_point(aes(x=mean_initial, y = mean_change))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  facet_wrap(~cluster)

(g2 <- df %>%
    mutate(cluster = reLabel(cluster)) %>%
ggplot(aes(x = cluster, y = mean_change)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(colour="lightgrey", alpha=0.1) +
  geom_hline(yintercept = 0, linetype="dashed") +
  ylab("Species growth rate") +
  coord_flip())

ggsave("plots/forest_vs_cluster.png",width=5, height=4)

#### taxa-level clusters ###################

(g3 <- df %>%
   mutate(cluster = reLabel(cluster)) %>%
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

(g4 <- df %>%
    mutate(cluster = reLabel(cluster)) %>%
    filter(cluster=="prefer open") %>%
    ggplot() +
    geom_boxplot(aes(x = Taxa, y = mean_change), 
                 outlier.shape = NA) +
    geom_hline(yintercept = 0, linetype="dashed") +
    coord_flip() +
    scale_fill_brewer(palette = "RdYlGn", direction =-1) +
    ylab("Open habitat species growth rate") +
    theme(legend.position = "top"))

#### plotting ####################

fig3ab <- plot_grid(g1,g2, nrow=1, labels=c("a", "b"))
fig3cd <- plot_grid(g3,g4, nrow=1, labels=c("c", "d"))

plot_grid(fig3ab, fig3cd, ncol=1)

#### testing #####################

rma.mv(mean_change, sd_change^2, mods= ~ factor(cluster),
       random = ~ 1 | Taxa, data = df)

rma.mv(mean_change, sd_change^2, mods= ~ factor(cluster)-1,
       random = ~ 1 | Taxa, data = df)

### forest specialisation ###############

forestSpecial <- readRDS("outputs/forestAssociations/forestSpecial_mixed_linear.rds") %>%
                    mutate(species = tolower(species))

#preference for broadleaf
summary(forestSpecial$ForestSpecial[forestSpecial$ForestSpecial>0])

#preference for coniferous
summary(forestSpecial$ForestSpecial[forestSpecial$ForestSpecial<0])

#### categorical #####

forestSpecial$forestType <- ifelse(forestSpecial$ForestSpecial>2, "Broadleaf",
                                   ifelse(forestSpecial$ForestSpecial<(-2), "Coniferous",
                                   "Indifferent"))
table(forestSpecial$forestType)

typeTrends <- inner_join(spTrends, forestSpecial) %>%
                  filter(forestType %in% c("Broadleaf", "Coniferous"))

(g1 <- ggplot(typeTrends, aes(x = forestType, y = mean_change)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(colour="lightgrey", alpha=0.1) +
  geom_hline(yintercept=0, linetype="dashed") +
  ylab("Species growth rate") +
  xlab("Forest"))

(g2 <- ggplot(typeTrends) +
  geom_boxplot(aes(x = Taxa, y = mean_change, fill=forestType), 
               outlier.shape = NA) +
  coord_flip() +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_fill_viridis_d("Forest")+
  theme(legend.position="top")+
    ylab("Species growth rate")) 

plot_grid(g1,g2, nrow=1, labels=c("a","b"))

#### testing ##########################

rma.mv(mean_change, sd_change^2, mods= ~ forestType,
       random = ~ 1 | Taxa, data = typeTrends)

#coniferous patterns

rma.mv(mean_change, sd_change^2, mods= ~ forestType-1,
       random = ~ 1 | Taxa, data = typeTrends)

### end ####################################