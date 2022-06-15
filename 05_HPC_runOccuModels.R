#script to run occupancy models

#takes outputs produced by the occurrencedataPrep.R

library(tidyverse)
library(occuR, lib.loc = "/home/users/diabow/Rlibraries")

### HPC stuff ############################################

dataDir <- "/home/users/diabow/ConnectedTreescapes/inputs"

outputDir <- "/home/users/diabow/ConnectedTreescapes/outputs"

task.id = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", "1"))

### load objects ##########################################

taxa_data <- readRDS(paste(dataDir, "taxa_data.rds", sep="/"))
visit_data <- readRDS(paste(dataDir, "visit_data.rds", sep="/"))
occMatrix <- readRDS(paste(dataDir,"occMatrix.rds",sep="/"))
commonSpecies <- readRDS(paste(dataDir,"commonSpecies.rds",sep="/"))

### species #############################################

#choose taxa for this array task

myspecies = as.character(commonSpecies[task.id])

visit_data$Species <- occMatrix[,myspecies]

table(visit_data$Species)

### format ###############################################

#format for using the occuR package

#correlation of site occupancy over space and time is induced by allowing occupancy probability 
#to be a smooth function of space and time

#need to make visit data with columns: site, occassion and obs
visit_data <- visit_data[,c("SiteID","Year","visit","Species","X","Y",
                            "nuSpecies","yday","yday2","decidForest")]

#col names needed for the package
names(visit_data)[1:4] <- c("site","occasion","visit","obs")

#add in visit and year again
visit_data$Year <- visit_data$occasion
visit_data$visitID <- visit_data$visit
visit_data$LL <- ifelse(visit_data$nuSpecies==1,"single",
                        ifelse(visit_data$nuSpecies %in% 2:3, "short",
                               "else"))

table(visit_data$LL)

### indices ###########################################

# need to make vist be indexed from i to n within each site and occasion
# for the occuR function
visit_data <- visit_data %>%
  group_by(site, occasion) %>%
  mutate(visit = as.numeric(as.factor(visit)))%>%
  ungroup()

visit_data$occasion <- as.numeric(as.factor(visit_data$occasion))

table(visit_data$occasion)

### site data ###########################################

#need to make site data with "site" and "occasion"

site_data <- visit_data %>%
  dplyr::select(site, occasion,X,Y,everything()) %>%
  dplyr::filter(!duplicated(interaction(site,occasion)))

### plot data ###########################################

visit_data %>%
  group_by(X,Y) %>%
  summarise(maxObs = max(obs)) %>%
  ggplot()+
  geom_point(aes(X,Y,colour=factor(maxObs)))+
  scale_colour_viridis_d()

#ggsave(paste0("Rawmap_",myspecies,".png"))

### occu model ############################################

m_spline2d <- fit_occu(list(psi ~ 1 + t2(X, Y, bs = "ts", k=10),
                            p ~ nuSpecies + yday),
                       as.data.table(visit_data),
                       as.data.table(site_data))

m_spline2d

#covariate effects
covariateEffects <- summary(m_spline2d$res,"fixed", p.value=TRUE) %>%
  as_tibble() %>%
  janitor::clean_names() %>%
  add_column(para = c("psi_intercept","p_intercept","p_nuSpecies","p_yday","spline"))

saveRDS(covariateEffects, file=paste0(outputDir,"/Gam_basic_",myspecies,".rds"))

### forest occu model ##########################################

m_spline2d <- fit_occu(list(psi ~ forestcover + t2(X, Y, bs = "ts", k=10),
                            p ~ nuSpecies + yday),
                       as.data.table(visit_data),
                       as.data.table(site_data))

m_spline2d

#covariate effects
covariateEffects <- summary(m_spline2d$res,"fixed", p.value=TRUE) %>%
  as_tibble() %>%
  janitor::clean_names() %>%
  add_column(para = c("psi_intercept","psi_forestcover","p_intercept","p_nuSpecies","p_yday","spline"))

saveRDS(covariateEffects, file=paste0(outputDir,"/Gam_forestcover_",myspecies,".rds"))

### end ####################################################

