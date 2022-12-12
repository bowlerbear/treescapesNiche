# analysis was run on datalabs

library(tidyverse)

selectTaxa <-  c("Ants", "AquaticBugs","Bees","Carabids","Centipedes","Craneflies",
                 "Dragonflies","E&D","Ephemeroptera","Gelechiids","Hoverflies",
                 "Ladybirds","Molluscs","Moths",
                 "Orthoptera","PlantBugs","ShieldBugs",
                 "Soldierflies","Spiders","Trichoptera","Wasps")

# data ######

msiTS <- list.files("outputs/msi_deriv", full.names=TRUE) %>%
            str_subset("msi_timeseries") %>%
            map_dfr(readRDS) %>%
            filter(taxon %in% selectTaxa) %>%
            mutate(taxon = case_when(taxon=="E&D" ~ "Empidids",
                                      TRUE ~ as.character(taxon))) 

# labelling #####

msiTS$cluster <- reLabel(msiTS$cluster)
table(msiTS$cluster)

# any to remove

#msiTS <- msiTS %>%
#          filter(!(taxon=="ShieldBugs" & cluster==3))

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
  scale_colour_brewer("Broadleaf forest association", palette = "RdYlGn", direction =-1) +
  scale_fill_brewer("Broadleaf forest association", palette = "RdYlGn", direction =-1) +
  ylab("MSI occupancy") + xlab("Year") +
  theme(legend.position="top")

ggsave("plots/msi_deriv_ts.png", width=10,height=7)

# standardardized ###

ggplot(msiTS)+
  geom_point(aes(x=year, y=msi_scaled, colour=cluster))+
  geom_line(aes(x=year, y=msi_scaled, colour=cluster))+
  geom_ribbon(aes(x=year, ymin=lowerCI_scaled, ymax=upperCI_scaled, fill=cluster),alpha=0.5)+
  facet_wrap(~taxon, scales="free")+
  theme_classic() +
  scale_colour_brewer("Broadleaf forest association", palette = "RdYlGn", direction =-1) +
  scale_fill_brewer("Broadleaf forest association", palette = "RdYlGn", direction =-1) +
  ylab("MSI occupancy") + xlab("Year") +
  theme(legend.position="top")

ggsave("plots/msi_deriv_standard_ts.png", width=9.5,height=6.5)
                                               
# select time series ######

#### ants #####

#groups:
#based on significance classification
# temp <- readRDS("outputs/forestAssociations/broadleafAssocations_mixed_linear.rds")
# temp$Trend <- ifelse(temp$estimate>0 & temp$pr_t<0.05, "positive",
#                            ifelse(temp$estimate<0 & temp$pr_t<0.05, "negative",
#                                   "none"))

#based on clusters
temp <- readRDS("outputs/clustering/deriv_classification_all.rds") %>%
  janitor::clean_names() %>%
  mutate(species = tolower(species),
         cluster = reLabel(cluster))

table(temp$cluster)

#get time series data for each
antsTS <- readRDS("outputs/antsTS.rds") 

#subset to forest species
antsTS <- antsTS %>%
  filter(species %in% tolower(temp$species)[temp$cluster %in% c("prefer forest")]) %>%
  mutate(year = as.numeric(gsub("year_", "", year)))

sort(unique(antsTS$species))

ggplot(antsTS) +
  geom_line(aes(x=year, y=meanOcc))+
  geom_ribbon(aes(x=year, ymin=lowerOcc, ymax=upperOcc), alpha=0.5)+
  facet_wrap(~species)
                                               
# end ######
