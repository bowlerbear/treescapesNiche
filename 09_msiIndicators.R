# analysis was run on datalabs

library(tidyverse)

### relabel clusters ####

reLabel <- function(cluster){
  
  cluster = ifelse(cluster==1, "flat",
                   ifelse(cluster==2, "humped",
                          ifelse(cluster==3, "open",
                                 ifelse(cluster==4, "forest",cluster))))
  
  return(cluster)
  
}

### msi indicators ######

msiTS <- list.files("outputs/msi", full.names=TRUE) %>%
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

ggsave("plots/msi_ts.png", width=10,height=7)

### msi change #####

msiChange <- list.files("outputs/msi", full.names=TRUE) %>%
  str_subset("msi_change") %>%
  map_dfr(readRDS) %>%
  mutate(cluster = reLabel(cluster))

ggplot(msiChange)+
  geom_crossbar(aes(x=taxon, y = median, ymin = lowerCI, ymax = upperCI,
                    fill=cluster),
                position="dodge", width =0.8) +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_fill_brewer(palette = "RdYlGn", direction =-1) +
  coord_flip() +
  facet_wrap(~cluster, nrow=1) +
  ylab("MSI growth rate")+ xlab("Taxon group") +
  theme_classic() +
  theme(legend.position = "none")

ggsave("plots/msi_change.png", width=7,height=4)
