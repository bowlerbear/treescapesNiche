# analysis was run on datalabs

library(tidyverse)

selectTaxa <-  c("Ants", "AquaticBugs","Bees","Carabids","Centipedes","Craneflies",
                 "Dragonflies","E&D","Ephemeroptera","Gelechiids","Hoverflies",
                 "Ladybirds","Molluscs","Moths",
                 "Orthoptera","PlantBugs","ShieldBugs",
                 "Soldierflies","Spiders","Trichoptera","Wasps")

# data ######

msiTS <- list.files("outputs/msi_cont", full.names=TRUE) %>%
            str_subset("msi_timeseries") %>%
            map_dfr(readRDS) %>%
            mutate(taxon = case_when(taxon=="E&D" ~ "Empidids",
                                      TRUE ~ as.character(taxon))) 

# labelling #####

msiTS$cluster <- ifelse(msiTS$cluster==1, "positive",
                        ifelse(msiTS$cluster==2, "negative",
                        "none"))

# any to remove

msiTS <- msiTS %>%
          filter(!(taxon=="ShieldBugs" & cluster==3))

# check plot ######

msiTS %>%
  filter(cluster=="positive") %>%
ggplot()+
  geom_point(aes(x=year, y=msi))+
  geom_line(aes(x=year, y=msi))+
  geom_ribbon(aes(x=year, ymin=lowerCI, ymax=upperCI),alpha=0.5)+
  facet_wrap(~taxon, scales="free")+
  theme_classic() +
  ylab("MSI occupancy") + xlab("Year") +
  theme(legend.position="top")


# final plot ######

ggplot(msiTS)+
  geom_point(aes(x=year, y=msi, colour=cluster))+
  geom_line(aes(x=year, y=msi, colour=cluster))+
  geom_ribbon(aes(x=year, ymin=lowerCI, ymax=upperCI, fill=cluster),alpha=0.5)+
  facet_wrap(~taxon, scales="free")+
  theme_classic() +
  scale_colour_brewer("Forest association", palette = "RdYlGn", direction =1) +
  scale_fill_brewer("Forest association", palette = "RdYlGn", direction =1) +
  ylab("MSI occupancy") + xlab("Year") +
  theme(legend.position="top")

ggsave("plots/msi_cont_ts.png", width=10,height=7)

# standardardized ###

ggplot(msiTS)+
  geom_point(aes(x=year, y=msi_scaled, colour=cluster))+
  geom_line(aes(x=year, y=msi_scaled, colour=cluster))+
  geom_ribbon(aes(x=year, ymin=lowerCI_scaled, ymax=upperCI_scaled, fill=cluster),alpha=0.5)+
  facet_wrap(~taxon, scales="free")+
  theme_classic() +
  scale_colour_brewer("Forest association", palette = "RdYlGn", direction =1) +
  scale_fill_brewer("Forest association", palette = "RdYlGn", direction =1) +
  ylab("MSI occupancy") + xlab("Year") +
  theme(legend.position="top")

ggsave("plots/msi_cont_standard_ts.png", width=10,height=7)
                                               
# select time series ######

#### ants #####

gamOutputs <- readRDS("outputs/forestAssociations/broadleafAssocations_mixed_linear.rds")

#add on trend classification
gamOutputs$Trend <- ifelse(gamOutputs$estimate>0 & gamOutputs$pr_t<0.05, "positive",
                           ifelse(gamOutputs$estimate<0 & gamOutputs$pr_t<0.05, "negative",
                                  "none"))

antsTS <- readRDS("outputs/antsTS.rds") 
#[1] "formica fusca"         "formica rufa"          "lasius platythorax"    "leptothorax acervorum"
#[5] "myrmica ruginodis"

#subset to forest species
antsTS <- antsTS %>%
  filter(species %in% tolower(gamOutputs$species)[gamOutputs$Trend=="positive"]) %>%
  mutate(year = as.numeric(gsub("year_", "", year)))

sort(unique(antsTS$species))

ggplot(antsTS) +
  geom_line(aes(x=year, y=meanOcc))+
  geom_ribbon(aes(x=year, ymin=lowerOcc, ymax=upperOcc), alpha=0.5)+
  facet_wrap(~species)
                                               
# end ######
