# analysis was run on datalabs

library(tidyverse)

selectTaxa <-  c("Ants", "AquaticBugs","Bees","Carabids","Centipedes","Craneflies",
                 "Dragonflies","E&D","Ephemeroptera","Gelechiids","Hoverflies",
                 "Ladybirds","Molluscs","Moths",
                 "Orthoptera","PlantBugs","ShieldBugs",
                 "Soldierflies","Spiders","Trichoptera","Wasps")

# msi indicators ######

msiTS <- list.files("outputs/msi_corr", full.names=TRUE) %>%
            str_subset("msi_timeseries") %>%
            map_dfr(readRDS) %>%
            mutate(cluster = reLabel(cluster))

ggplot(msiTS)+
  geom_point(aes(x=year, y=msi, colour=cluster))+
  geom_line(aes(x=year, y=msi, colour=cluster))+
  geom_ribbon(aes(x=year, ymin=lowerCI, ymax=upperCI, fill=cluster),alpha=0.5)+
  facet_wrap(~taxon, scales="free")+
  theme_classic() +
  scale_colour_brewer(palette = "RdYlGn", direction =-1) +
  scale_fill_brewer(palette = "RdYlGn", direction =-1) +
  ylab("MSI occupancy") + xlab("Year") +
  theme(legend.position="top")

ggsave("plots/msi_corr_ts.png", width=10,height=7)
