#Script to analyse the BRC records and assess the effect of forest cover on species occurrence

#this is for the broadleaf analysis

#Parameter needed:
#mytaxa

#possible values: 
#"Ants"              "AquaticBugs"       "Bees"              "Bryophytes"        "Carabids"         
#"Centipedes"        "Craneflies"        "Dragonflies"       "E&D"               "Ephemeroptera"    
#"Fish"              "FungusGnats"       "Gelechiids"        "Hoverflies"        "HypogeanCrustacea"
#"Ladybirds"         "LeafSeedBeetles"   "Lichens"           "Millipedes"        "Molluscs"         
#"Moths"             "Neuropterida"      "Orthoptera"        "PlantBugs"         "Plecoptera"       
#"RoveBeetles"       "ShieldBugs"        "SoldierBeetles"    "Soldierflies"      "Spiders"          
#"Trichoptera"       "VascPlants"        "Wasps"             "Weevils"

#this is based on the lists of the available cleaned data files

### libraries #########################################

#devtools::install_github("https://github.com/colinharrower/BRCmap", 
#                         type = "source", 
#                         lib = "/home/users/diabow/Rlibraries")

library(BRCmap, lib.loc = "/home/users/diabow/Rlibraries") 
library(tidyverse)
library(sf)
library(mgcv)
library(gratia,lib.loc = "/home/users/diabow/Rlibraries" )

### directories #################################

setwd("./ConnectedTreescapes")

dataDir <- "W:/PYWELL_SHARED/Pywell Projects/BRC/Charlie/1.c. New Model Rerun/1. Data/Cleaned Datasets"

dataDir <- "/gws/nopw/j04/ceh_generic/rboyd/inputs/species_data"

inputDir <- "./inputs"

outputDir <- "./outputs"


### choose taxa #####################################

taxaFiles <- list.files(dataDir) %>%
                str_subset("Cleaned")

length(taxaFiles)

task.id = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", "1"))

mytaxa <- taxaFiles[task.id]
#moths are 22 on HPC dir

### load data ########################################

taxaFile <- list.files(dataDir)  %>%
              str_subset("Cleaned") %>% 
              str_subset(mytaxa)

load(paste(dataDir, taxaFile,sep="/"))

message('Data has been read in')
message(paste('Taxa is:',mytaxa))

###standardize labels ################################

if("SQ_1KM" %in% names(taxa_data)){
  
  taxa_data <- taxa_data %>%
    rename(TO_GRIDREF = SQ_1KM)
  
} else {}


if(!("YEAR" %in% names(taxa_data))){
  
  taxa_data <- taxa_data %>%
    mutate(YEAR = lubridate::year(as.Date(taxa_data$TO_STARTDATE)))
  
} else {}


taxa_data$Species <- taxa_data$CONCEPT
head(taxa_data)

### add coords ############################################

coords <- OSgrid2GB_EN(taxa_data$TO_GRIDREF)
#qplot(EASTING,NORTHING, data=coords)

taxa_data <- taxa_data %>%
  add_column(X = coords[,1],
             Y = coords[,2])

#EPSG:27700
#"+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs" 

### spatial filter #########################################

#plot first set of species
# taxa_data %>%
#   filter(YEAR > 1979) %>%
#   filter(Species %in% commonSpecies[1:12]) %>%
#   ggplot() +
#   geom_point(aes(X,Y)) +
#   theme_minimal() +
#   facet_wrap(~Species, nrow=2)

#exclude northern island
GBR <- raster::getData('GADM',country='GBR',level=1) %>%
  st_as_sf() %>%
  filter(NAME_1 %in% c("England","Wales","Scotland")) %>%
  st_transform(crs = 27700) %>%
  bind_rows() %>%
  st_union()

#make spatial object
taxa_data_spatial <- taxa_data %>%
  st_as_sf(.,coords=c("X","Y"), crs=27700)

#filter on non-spatial data
taxa_data <- taxa_data %>%
  filter(st_intersects(taxa_data_spatial, GBR, sparse = FALSE)[,1]) 

message(paste('Number of records passing filter:',nrow(taxa_data)))

### subsample recs ####################################

set.seed(3)

taxa_data <- taxa_data %>% filter(YEAR > 1989 & YEAR <2016)  # this is the period of the forest cover data

taxa_data$visit <- paste(taxa_data$TO_GRIDREF,taxa_data$TO_STARTDATE,sep="_")

#option 1: simple subsample
# we dont need tonnes of data to estimate forest preference, so cap to 10000 visits
# nuVisits <- length(unique(taxa_data$visit))
# selectVisits <- sample(unique(taxa_data$visit), size = ifelse(nuVisits>10000,
#                                                               10000,
#                                                               nuVisits))
# taxa_data <- taxa_data %>% filter(visit %in% selectVisits)


#but reduce sampling at sites with many visits in the same year
#(visitsPerYear <- visit_data %>%
#    group_by(SiteID,Year) %>%
#    summarise(nuVisits = length(visit)))

#cap at 5 visits per site and year
#visit_data <- visit_data %>%
#  group_by(SiteID, X, Y, Year) %>%
#  sample_n(size = ifelse(length(visit)>5,5,length(visit))) %>%
#  ungroup()


#option 2: undersample well-sampled grids - one visit per grid
# taxa_data <- taxa_data %>%
#                 group_by(TO_GRIDREF) %>%
#                 filter(visit == sample(visit,1)) %>%
#                 ungroup()


### organize data into visits #######################

# a visit is a set of records on a given date in a given grid

visit_data <- taxa_data %>%
  group_by(visit, TO_GRIDREF, TO_STARTDATE, YEAR, X, Y) %>%
  summarise(nuSpecies = length(unique(Species))) %>%
  ungroup() %>%
  rename(SiteID = TO_GRIDREF,
         Year = YEAR,
         Date = TO_STARTDATE) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(Yday = lubridate::yday(Date)) %>%
  mutate(yday2 = as.numeric(scale(Yday^2))) %>%
  mutate(yday = as.numeric(scale(Yday))) 

#average number of sites visited per year
message(paste('Median number of sites visited per year:', median(table(visit_data$Year))))

### list length #####################################

#number of species reported on a visit, as an indicator of effort
#most records are of list length 1 and probably not a comprehensive survey

visit_data$LL <- ifelse(visit_data$nuSpecies==1,"single",
                        ifelse(visit_data$nuSpecies %in% 2:3,"short","long"))

table(visit_data$LL)

### identify species for analysis ###################

#sum number of records per species
speciesSummary <- taxa_data %>%
  group_by(Species) %>%
  summarise(nuRecs = length(Species),
            nuYears = length(unique(YEAR)),
            nuSites = length(unique(TO_GRIDREF))) %>%
  ungroup() %>%
  arrange(desc(nuRecs))

#how many species do we have at least 50 records for
commonSpecies <- speciesSummary %>%
  filter(nuRecs > 100 & nuSites > 50) %>%
  pull(Species) %>%
  as.character()

#report some feedback
message(paste('Total number of species:',nrow(speciesSummary)))
message(paste('Number of species passing threshold:',length(commonSpecies)))

### class imbalance ##################################

#subsample option 3
visit_data$Grid10km <- sapply(visit_data$SiteID,
                             function(x){substr(as.character(x),1,4)})

#write the function here to use in the species models below

#identify whether species is present in the 10 km grid and subset to those grids
subsampleData <- function(visit_data){
  
#get all detections of a species
presenceRecords <- visit_data %>%
                      filter(Species==1)

#get sample of negative detections at within 10 km grids in which a species is present
presenceGrids <- visit_data %>%
                  group_by(Grid10km) %>%
                  summarise(detectSpecies = max(Species)) %>%
                  filter(detectSpecies == 1) %>%
                  pull("Grid10km") %>% as.character()

#get when during the year the species is detectable
presenceSeason <- visit_data %>%
                      filter(Species == 1) %>%
                      summarise(minDay = min(Yday),
                                maxDay = max(Yday)) 

absenceRecords <- visit_data %>%
                    filter(Grid10km %in% presenceGrids) %>%
                    filter(Yday > presenceSeason$minDay & Yday < presenceSeason$maxDay) %>%
                    filter(Species==0) %>%
                    group_by(Grid10km, Year) %>%
                    slice_sample(n = 1) %>%
                    ungroup()

#if there are more presence than absences, subsample the presences to same number
if(nrow(presenceRecords) > nrow(absenceRecords)){
  presenceRecords <- presenceRecords %>%
                      slice_sample(n = nrow(absenceRecords))
}


#combine all together
allRecords <- bind_rows(presenceRecords,
                        absenceRecords)

}

### get forest cover #################################

# need to align the year of visit with the nearest year of forest cover data

# we have forest land cover for 1990, 2000, 2007 and 2015
# 1990 to 1995 visits - land cover data 1990
# 1995 to 2003 visits - land cover data 2000
# 2004 to 2011 visits - land cover data 2007
# 2012 to 2015 visits - land cover data 2015

visit_data$forestYear<- ifelse(visit_data$Year %in% 1990:1995, 1990,
                               ifelse(visit_data$Year %in% 1995:2003, 2000,
                                      ifelse(visit_data$Year %in% 2004:2011, 2007,
                                            2015)))

#get forest cover data
grid1km <- readRDS(paste0(inputDir,"/grid1km_decidForest_allyears.rds")) %>%
              pivot_longer(cols = contains("fc"), 
                           names_to="forestYear", 
                           values_to="decidForest") %>%
              rename(SiteID = tile_name) %>%
              mutate(forestYear = as.numeric(gsub("fc_","",forestYear)))

#merge with taxa data based on forest cover of ordnance survey grid
visit_data <- inner_join(visit_data, grid1km, by=c("SiteID","forestYear")) %>%
                  filter(!is.na(decidForest))

#hist(visit_data$decidForest)

message('Forest cover sorted')

### species occupancy matrix ##############################

#align data frames

#visit data
visit_data <- visit_data %>%
                mutate(visitID = as.numeric(as.factor(visit_data$visit))) %>%
                arrange(visitID)

#species per visit data
taxa_data <- taxa_data %>%
                filter(visit %in% visit_data$visit) %>%
                mutate(visitID = as.numeric(as.factor(visit))) 
                
#add on focal species of this array task
occMatrix <- reshape2::acast(taxa_data, visitID ~ Species, value.var = "Species",
                             fun=function(x)length(x))

#make response binary
occMatrix[occMatrix > 1] <- 1

#check things align
if(all(row.names(occMatrix) == visit_data$visitID)){
  message('And occupancy matrix aligns')
}

# ### basic glm #############################################
# 
# # I call it glm because we asssume a linear effect of forest cover
# # the gam model is only used to model space
# 
# #general function
# fitBasicGam <- function(myspecies){
# 
#   #check all aligns and add in species
#   all(row.names(occMatrix) == visit_data$visitID)
#   visit_data$Species <- occMatrix[,myspecies]
# 
#   #fit gam and pull out forest cover effect
#   require(mgcv)
#   gam1 <- gam(Species ~ decidForest + yday + yday2 + Year + LL + s(X,Y),
#               family = "binomial",
#               data = visit_data)
# 
#   summary(gam1)$p.table[2,]
# 
# }
# 
# #apply function
# gamOutput <- commonSpecies %>%
#   map_dfr(fitBasicGam) %>%
#   add_column(Species = commonSpecies) %>%
#   arrange(desc(Estimate)) %>%
#   janitor::clean_names()
# 
# saveRDS(gamOutput,file=paste0(outputDir,"/gamOutput_glm_subset_random_decid_",mytaxa,".rds"))
# 
# message('Basic glm done')
# 
# ### niche shape ###########################################
# 
# hist(visit_data$decidForest) #most of the data is
# 
# fitGamNiche <- function(myspecies){
# 
#   #check all aligns and add in species
#   all(row.names(occMatrix) == visit_data$visitID)
#   visit_data$Species <- occMatrix[,myspecies]
# 
#   #fit gam and pull out forest cover effect
#   require(mgcv)
#   gam1 <- gam(Species ~ s(decidForest,k=3) + yday + yday2 + Year + LL + s(X,Y),
#               family = "binomial",
#               data = visit_data)
# 
#   #predict the gam effect of forest cover
#   newdata = data.frame(decidForest = seq(0,100,by=1),
#                        yday = median(visit_data$yday),
#                        yday2 = median(visit_data$yday2),
#                        X = median(visit_data$X),
#                        Y = median(visit_data$Y),
#                        LL = "long",
#                        Year = median(visit_data$Year))
# 
#   newdata$preds <- predict(gam1,newdata, type="response")
#   newdata$preds_se <- predict(gam1,newdata, se.fit=TRUE, type="response")$se.fit
#   newdata$Species <- myspecies
# 
#   return(newdata)
# 
# }
# 
# gamOutput <- commonSpecies %>%
#   map_dfr(fitGamNiche)
# 
# saveRDS(gamOutput,file=paste0(outputDir,"/gamOutput_gam_shape_subset_random_decid_",mytaxa,".rds"))
# 
# message('Basic gam done')

### basic gamm ###########################################

# fitGammNiche <- function(myspecies){
# 
#   #check all aligns and add in species
#   all(row.names(occMatrix) == visit_data$visitID)
#   visit_data$Species <- occMatrix[,myspecies]
# 
#   #for subsample option 3
#   visit_data <- subsampleData(visit_data)
#   
#   #fit gam and pull out forest cover effect
#   require(mgcv)
#   gamm1 <- gamm(Species ~ decidForest + LL +  yday +yday2 + s(X,Y),
#                  family = "binomial",
#                  random = list(Year=~1),
#                  data = visit_data)
# 
#   summary(gamm1$gam)$p.table[2,]
# 
# }
# 
# #apply function
# gamOutput <- commonSpecies %>%
#   map_dfr(fitGammNiche) %>%
#   add_column(Species = commonSpecies) %>%
#   arrange(desc(Estimate)) %>%
#   janitor::clean_names()
# 
# saveRDS(gamOutput,file=paste0(outputDir,"/gamOutput_gamm_subset_random_decid_",mytaxa,".rds"))
# 
# message('Gamm done')

### gamm niche ###########################################

fitGammNiche <- function(myspecies){

  #check all aligns and add in species
  all(row.names(occMatrix) == visit_data$visitID)
  visit_data$Species <- occMatrix[,myspecies]

  #for subsample option 3
  visit_data <- subsampleData(visit_data)

  #fit gam and pull out forest cover effect
  require(mgcv)
  gamm1 <- gamm(Species ~ s(decidForest, k=3) + LL +  yday +yday2 + s(X,Y),
                 random = list(Year=~1),
                 family = "binomial",
                 data = visit_data)

  #predict the gam effect of forest cover
  newdata = data.frame(decidForest = seq(0,100,by=1),
                       yday = median(visit_data$yday),
                       yday2 = median(visit_data$yday2),
                       X = median(visit_data$X),
                       Y = median(visit_data$Y),
                       LL = "long",
                       Year = median(visit_data$Year))

  newdata$preds <- predict(gamm1$gam,newdata, type="response")
  newdata$preds_se <- predict(gamm1$gam,newdata, se.fit=TRUE, type="response")$se.fit
  newdata$Species <- myspecies

  return(newdata)

}

gamOutput <- commonSpecies %>%
  map_dfr(fitGammNiche) %>%
  janitor::clean_names()

saveRDS(gamOutput,file=paste0(outputDir,"/gamOutput_gamm_shape_subset_random_decid_",mytaxa,".rds"))

message('Gamm shape done')

### gam derivatives #####################################

fitGammNiche <- function(myspecies){

  #check all aligns and add in species
  all(row.names(occMatrix) == visit_data$visitID)
  visit_data$Species <- occMatrix[,myspecies]

  #fit gam and pull out forest cover effect
  require(mgcv)
  gamm1 <- gamm(Species ~ s(decidForest, k=3) + LL + yday +yday2 + s(X,Y),
                random = list(Year=~1),
                family = "binomial",
                data = visit_data)

  require(gratia)
  deriv1 <- derivatives(gamm1$gam, type = "central", term ="s(decidForest)", order=1) %>%
    add_column(Species = myspecies,
               edf = summary(gamm1$gam)$edf[1],
               p = summary(gamm1$gam)$s.table[1,4])

  return(deriv1)

}

gamOutput <- commonSpecies %>%
  map_dfr(fitGammNiche) %>%
  janitor::clean_names()

saveRDS(gamOutput,file=paste0(outputDir,"/gamOutput_gamm_derivatives_subset_random_decid_",mytaxa,".rds"))

message('Gamm derivatives done')

### end #######################################################
