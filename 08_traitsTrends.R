### species-level trends vs forest traits ####

library(tidyverse)
library(ggridges)
theme_set(theme_classic())

selectTaxa <-  c("Ants", "AquaticBugs","Bees","Bryophytes","Carabids","Centipedes","Craneflies",
                 "Dragonflies","E&D","Ephemeroptera","FungusGnats","Gelechiids","Hoverflies",
                 "Ladybirds","LeafSeedBeetles","Lichens","Millipedes","Molluscs","Moths","Neuropterida",
                 "Orthoptera","PlantBugs","Plecoptera","RoveBeetles","ShieldBugs","SoldierBeetles",
                 "Soldierflies","Spiders","Trichoptera","Wasps","Weevils")

### choose models #######################

#trends
trendsFolder <- "outputs/speciesTrends"

#forest associations
forestFolder <- "outputs/forestAssociations/broadleaf"

### trends ##############################

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
  xlab("Trends")

#filter those with large uncertainties

hist(spTrends$sd_change)
summary(spTrends$sd_change)

spTrends <- spTrends %>%
              filter(sd_change < 3)

### forest preferences ###################

gamOutputs <- list.files(forestFolder,full.names=TRUE) %>%
                  str_subset("gamOutput_glm_subset_random_") %>%
                  set_names() %>%
                  map_dfr(readRDS, .id="source") %>%
                  group_by(source) %>%
                  mutate(Taxa = strsplit(source,"_")[[1]][5]) %>%
                  ungroup() %>%
                  filter(Taxa %in% selectTaxa) %>%
                  mutate(species = tolower(species),
                         forest_assoc = estimate)
                  
### merge ################################

df <- inner_join(spTrends, gamOutputs, by = c("species","Taxa"))

### outlier ##############################

df <- df %>%
        filter(!species %in% c("ara_1391"))

###plotting ##############################

ggplot(df) +
  geom_point(aes(x = forest_assoc, y = mean_change)) +
  facet_wrap(~Taxa) +
  geom_hline(yintercept = 0, linetype="dashed") +
  geom_vline(xintercept = 0, linetype="dashed") 

### lms #################################

#https://github.com/paul-buerkner/brms/issues/643

getForestEffect <- function(taxa){
  
  require(brms)
  
  df1 <- subset(df,Taxa==taxa & forest_assoc>0)
  bm1 <- brm(mean_change|se(sd_change, sigma = TRUE) ~ me(forest_assoc, std_error),
             data = df1)
  
  
  df2 <- subset(df,Taxa==taxa & forest_assoc<0)
  bm2 <- brm(mean_change|se(sd_change, sigma = TRUE) ~ me(forest_assoc, std_error),
             data = df2)
  
  
  data.frame(Taxa = taxa,
            posEffect = fixef(bm1)[2,1],
            posEffect_se = fixef(bm1)[2,2],
            negEffect = fixef(bm2)[2,1],
            negEffect_se = fixef(bm2)[2,2])
            
}


lapply(sort(unique(df$Taxa))[26:28], function(x){
  temp <- getForestEffect(x)
  saveRDS(temp,file=paste0("outputs/forestVStrends/forestVStrends_",x,".rds"))
  })

### process lms #########################

lmOutputs <- list.files("outputs/forestVStrends",full.names = TRUE) %>%
              map_dfr(readRDS)


g1 <- ggplot(lmOutputs)+
  geom_pointrange(aes(x = Taxa, y = posEffect, 
                      ymin =posEffect - posEffect_se,
                      ymax = posEffect + posEffect_se),
                      size = rel(0.3))+
  geom_hline(yintercept=0, linetype="dashed")+
  ylab("effect of forest specialisation")+
  coord_flip()

g2 <- ggplot(lmOutputs)+
  geom_pointrange(aes(x = Taxa, y = negEffect, 
                      ymin =negEffect - negEffect_se,
                      ymax = negEffect + negEffect_se),
                      size = rel(0.3))+
  geom_hline(yintercept=0, linetype="dashed")+
  ylab("effect of forest avoidance")+
  coord_flip()

cowplot::plot_grid(g1,g2, labels=c("A","B"))
ggsave("plots/forestVStrends.png",width=6, height=3.5)

### end ##################################