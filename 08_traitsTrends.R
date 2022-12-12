### species-level trends vs forest traits ####

library(metafor)
library(tidyverse)
library(ggridges)
library(cowplot)
theme_set(theme_classic())

selectTaxa <-  c("Ants", "AquaticBugs","Bees","Carabids",
                 "Centipedes","Craneflies",
                 "Dragonflies","E&D","Ephemeroptera","Gelechiids","Hoverflies",
                 "Ladybirds","Molluscs","Moths",
                 "Orthoptera","PlantBugs","ShieldBugs",
                 "Soldierflies","Spiders","Trichoptera","Wasps")

#AquaticBug, Craneflies, E&D
#Plantbugs
#Spiders
#Moths approved

### useful functions ###################

source("00_functions.R")

### choose models #######################

#trends
trendsFolder <- "outputs/speciesTrends"

#forest associations
forestFolder <- "outputs/forestAssociations/broadleaf_subsample3"

### species trends ##############################

spTrends <- list.files(trendsFolder, full.names = TRUE) %>%
              set_names() %>%
              map_dfr(readRDS, .id="source") %>%
              group_by(source) %>%
              rename(Taxa = taxon) %>%
              filter(Taxa %in% selectTaxa) %>%
              mutate(Taxa = case_when(Taxa=="E&D" ~ "Empidids",
                          TRUE ~ as.character(Taxa)))
              
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


allTaxa <- sort(unique(spTrends$Taxa))

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

(g1 <- ggplot(df, aes(x = forest_assoc, y = mean_change)) +
  geom_bin2d() +
  scale_fill_viridis_c("number of species") +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_vline(xintercept = 0, linetype="dashed") +
  xlab("Broadleaf woodland association") + ylab("Species growth rate")+
  xlim(-0.075,0.075)+
  theme(legend.position = "top"))

ggsave("plots/forestVStrends.png",width=5, height=3.5)

#### testing #####

qplot(forest_assoc, mean_change, data=df)

summary(lm(mean_change ~ forest_assoc, data=df))

rma1 <- rma.mv(mean_change, sd_change^2, mods= ~ forest_assoc,
       random = ~ 1 | Taxa, data = df)

#estimate      se     zval    pval    ci.lb    ci.ub   ​ 
#intrcpt        -0.1506  0.0666  -2.2615  0.0237  -0.2812  -0.0201  * 
#forest_assoc    1.1919  0.6161   1.9346  0.0530  -0.0156   2.3994  .

#get R2
rma1 <- rma.mv(mean_change, sd_change^2, mods= ~ forest_assoc,random = ~ 1 | Taxa, data = df, method="ML")
rma2 <- rma.mv(mean_change, sd_change^2, mods= ~ 1,random = ~ 1 | Taxa, data = df, method="ML")
anova(rma1,rma2)
(rma1$sigma2 - rma2$sigma2)/rma1$sigma2

#direction of effect
rma.mv(mean_change, sd_change^2, mods= ~ forest_assoc,
       random = ~ 1 | Taxa, data = subset(df, forest_assoc>0))
#estimate      se     zval    pval    ci.lb   ci.ub   ​ 
#intrcpt        -0.1469  0.0796  -1.8461  0.0649  -0.3028  0.0091  . 
#forest_assoc   -1.4957  1.3362  -1.1194  0.2630  -4.1146  1.1232  

rma.mv(mean_change, sd_change^2, mods= ~ forest_assoc,
       random = ~ 1 | Taxa, data = subset(df, forest_assoc<0))

#estimate      se     zval    pval    ci.lb   ci.ub   ​ 
#intrcpt        -0.1182  0.0827  -1.4294  0.1529  -0.2802  0.0439    
#forest_assoc    0.4385  1.6779   0.2613  0.7938  -2.8501  3.7271


#maybe test within each taxa too
lapply(allTaxa, function(x){
  print(x)
  temp <- rma(mean_change, sd_change^2, mods= ~ forest_assoc, 
       data = subset(df, Taxa==x))
  data.frame(Species = x, beta = temp$beta[2,1], p=temp$pval[2])
}) %>%
  reduce(rbind) %>%
  filter(p<0.05)

### signif forest pref ####################

gamOutputs$forestPref <- ifelse(gamOutputs$estimate>0 & gamOutputs$pr_t<0.05, "positive",
                           ifelse(gamOutputs$estimate<0 & gamOutputs$pr_t<0.05, "negative",
                                  "none"))
table(gamOutputs$forestPref)

df <- inner_join(spTrends, gamOutputs, by = c("species","Taxa"))

#across all
ggplot(df, aes(x= forestPref , y = mean_change)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha=0.2, colour="lightgrey") +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Forest association") + ylab("Species growth rate") 

#for each taxa#
ggplot(df, aes(x= Taxa, y = median_change)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha=0.2, colour="lightgrey") +
  theme_classic() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  xlab("Taxa") + ylab("Species growth rate") +
  facet_wrap(~forestPref)+
  coord_flip()

#### testing #####

rma.mv(mean_change, sd_change^2, mods= ~ forestPref,
       random = ~ 1 | Taxa, data = df)

#                   estimate      se     zval    pval    ci.lb    ci.ub     
#intrcpt              -0.1494  0.0681  -2.1924  0.0283  -0.2829  -0.0158   * 
#forestPrefnone       -0.0535  0.0243  -2.2026  0.0276  -0.1011  -0.0059   * 
#forestPrefpositive    0.0567  0.0208   2.7268  0.0064   0.0159   0.0974  **

rma.mv(mean_change, sd_change^2, mods= ~ forestPref -1,
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
  scale_fill_brewer("Long-term trend", type="div") +
  ylab("Number of species") + xlab("Forest preference") + 
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

df %>%
  mutate(cluster = reLabel(cluster)) %>%
ggplot() +
  geom_boxplot(aes(x = Taxa, y = mean_change, fill=cluster,
                   colour=cluster),outlier.shape = NA) +
  scale_fill_brewer("Broadleaf cluster",palette = "RdYlGn", direction =-1) +
  scale_colour_brewer("Broadleaf cluster",palette = "RdYlGn", direction =-1) +
  geom_hline(yintercept = 0, linetype="dashed") +
  ylab("Species growth rate") +
  ylim(-5,5) +
  coord_flip() 

df %>%
    mutate(cluster = reLabel(cluster)) %>%
ggplot(aes(x = cluster, y = mean_change)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(colour="lightgrey", alpha=0.1) +
  geom_hline(yintercept = 0, linetype="dashed") +
  ylab("Species growth rate") +
  coord_flip()


#add on direction of trend
df <- df %>%
  mutate(direction = ifelse(lowerCI_change>0,"increase",
                          ifelse(upperCI_change<0,"decrease",
                                 ifelse(mean_change<0 & upperCI_change>0,"non-sig decrease",
                                        "non-sig increase")))) %>%
  mutate(direction = factor(direction,
                            levels = c("decrease", "non-sig decrease", 
                                       "non-sig increase", "increase"))) %>%
  mutate(cluster = reLabel(cluster))
  

(g2 <- df %>%
  group_by(cluster, direction) %>%
  summarise(nuSpecies = length(unique(species))) %>%
  group_by(cluster) %>%
  mutate(totalSpecies = sum(nuSpecies)) %>%
  ungroup() %>%
  ggplot() +
  geom_col(aes(x=cluster, y = nuSpecies/totalSpecies, fill=direction)) +
  scale_fill_brewer("", type="div") +
  ylab("Proportion of species") + xlab("Broadleaf cluster") + 
  theme(legend.position = c(0.3,1),
        legend.title = element_text(size=8), 
        legend.text = element_text(size=6),
        legend.key.size = unit(0.5, 'cm'), 
        legend.key.height = unit(0.5, 'cm'), 
        legend.key.width = unit(0.5, 'cm'))+
  guides(fill = guide_legend(reverse=TRUE,nrow=1))+
  coord_flip())

ggsave("plots/forest_vs_cluster.png",width=5, height=4)

#### taxa-level clusters ###################

(g3 <- df %>%
   mutate(cluster = reLabel(cluster)) %>%
   filter(cluster=="prefer woodland") %>%
ggplot() +
  geom_boxplot(aes(x = Taxa, y = mean_change), 
               outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype="dashed") +
  coord_flip() +
  scale_fill_brewer(palette = "RdYlGn", direction =-1) +
  ylab("Broadleaf species growth rate") +
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

ggsave("plots/fig.3.png",width=8,height=7)

#### testing #####################

rma.mv(mean_change, sd_change^2, mods= ~ factor(cluster),
       random = ~ 1 | Taxa, data = df)

rma.mv(mean_change, sd_change^2, mods= ~ factor(cluster)-1,
       random = ~ 1 | Taxa, data = df)

#estimate      se     zval    pval    ci.lb    ci.ub     
#intrcpt                                -0.1907  0.0717  -2.6614  0.0078  -0.3311  -0.0503   ** 
# factor(cluster)prefer intermediate      0.4533  0.0707   6.4092  <.0001   0.3147   0.5919  *** 
# factor(cluster)no strong preferences   -0.0589  0.0308  -1.9089  0.0563  -0.1194   0.0016    . 
#factor(cluster)prefer open             -0.0140  0.0466  -0.3007  0.7636  -0.1053   0.0773       


df$cluster <- factor(df$cluster, levels=c("prefer open","prefer intermediate",
                                          "no strong preferences","prefer woodland"))

#estimate      se     zval    pval    ci.lb    ci.ub     ​ 
#intrcpt                                -0.2047  0.0739  -2.7682  0.0056  -0.3496  -0.0598   ** 
#factor(cluster)prefer intermediate      0.4673  0.0725   6.4493  <.0001   0.3253   0.6093  *** 
#factor(cluster)no strong preferences   -0.0449  0.0415  -1.0823  0.2791  -0.1262   0.0364      
#factor(cluster)prefer woodland          0.0140  0.0466   0.3007  0.7636  -0.0773   0.1053     

### summary statistics #################

df %>%
  group_by(cluster, direction) %>%
  count() %>%
  group_by(cluster) %>%
  mutate(clusterProp = n/sum(n)) %>%
  arrange(cluster) %>%
  filter(direction %in% c("decrease", "increase"))


df %>%
  group_by(cluster) %>%
  summarise(medChange = median(median_change))

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
#Broadleaf  Coniferous Indifferent 
#463         381        1850 

typeTrends <- inner_join(spTrends, forestSpecial) %>%
                  filter(forestType %in% c("Broadleaf", "Coniferous"))

(g1 <- ggplot(typeTrends, aes(x = forestType, y = mean_change)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(colour="lightgrey", alpha=0.1) +
  geom_hline(yintercept=0, linetype="dashed") +
  ylab("Species growth rate") +
  xlab("Woodland type") +
    ylim(-5,5))

(g2 <- ggplot(typeTrends) +
  geom_boxplot(aes(x = Taxa, y = mean_change, fill=forestType), 
               outlier.shape = NA) +
  coord_flip() +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_fill_viridis_d("Woodland")+
  theme(legend.position="top")+
  ylab("Species growth rate") +
  guides(fill = guide_legend(reverse=TRUE, byrow=TRUE,nrow=2))+
  ylim(-5,5))

plot_grid(g1,g2, nrow=1, labels=c("a","b"))

ggsave("plots/fig.4.png",width=7,height=5)

#### testing ##########################

rma.mv(mean_change, sd_change^2, mods= ~ forestType,
       random = ~ 1 | Taxa, data = typeTrends)

#coniferous patterns
#estimate      se     zval    pval    ci.lb    ci.ub     ​ 
#intrcpt                -0.0659  0.0788  -0.8364  0.4029  -0.2203   0.0885      
#forestTypeConiferous   -0.1818  0.0528  -3.4395  0.0006  -0.2854  -0.0782  *** 
  

rma.mv(mean_change, sd_change^2, mods= ~ forestType-1,
       random = ~ 1 | Taxa, data = typeTrends)

#estimate      se     zval    pval    ci.lb    ci.ub    ​ 
#forestTypeBroadleaf    -0.0659  0.0788  -0.8364  0.4029  -0.2203   0.0885     
#forestTypeConiferous   -0.2477  0.0847  -2.9248  0.0034  -0.4136  -0.0817  **  
  
### end ####################################