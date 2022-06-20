
allMSI <- list.files("outputs/msi", full.names=TRUE) %>%
            str_subset("msi_timeseries") %>%
            map_dfr(readRDS)


ggplot(allMSI)+
  geom_point(aes(x=year, y=msi, colour=factor(cluster)))+
  geom_line(aes(x=year, y=msi, colour=factor(cluster)))+
  geom_ribbon(aes(x=year, ymin=lowerCI, ymax=upperCI, fill=factor(cluster)),alpha=0.5)+
  facet_wrap(~taxon)+
  theme_classic()
