#Script to analyse the BRC records and assess the effect of forest cover on species occurrence

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

### local directories #################################

# dataDir <- "W:/PYWELL_SHARED/Pywell Projects/BRC/Charlie/1.c. New Model Rerun/1. Data/Cleaned Datasets"
# 
# inputDir <- "inputs"
# 
# outputDir <- "outputs"

### HPC directories #################################

dataDir <- "/gws/nopw/j04/ceh_generic/rboyd/inputs/species_data"

inputDir <- "/home/users/diabow/ConnectedTreescapes/inputs"

outputDir <- "/home/users/diabow/ConnectedTreescapes/outputs/conif"

### choose taxa #####################################

taxaFiles <- list.files(dataDir) %>%
                str_subset("Cleaned")

length(taxaFiles)

task.id = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", "1"))

mytaxa <- taxaFiles[task.id]

### load data ########################################

taxaFile <- list.files(dataDir)  %>%
              str_subset("Cleaned") %>% 
              str_subset(mytaxa)

load(paste(dataDir, taxaFile,sep="/")) # check whethey they are rds files!!

message('Data has been read in')
message(paste('Taxa is:',mytaxa))

###fix grid labels ####################################

if("SQ_1KM" %in% names(taxa_data)){
  
  taxa_data <- taxa_data %>%
    rename(TO_GRIDREF = SQ_1KM)
  
} else {}


if(!("YEAR" %in% names(taxa_data))){
  
  taxa_data <- taxa_data %>%
    mutate(YEAR = lubridate::year(as.Date(taxa_data$TO_STARTDATE)))
  
} else {}


### subsample recs ####################################

#by year
taxa_data <- taxa_data %>%
                filter(YEAR > 1989 & YEAR <2016) 


taxa_data$visit <- paste(taxa_data$TO_GRIDREF,taxa_data$TO_STARTDATE,sep="_")

nuVisits <- length(unique(taxa_data$visit))

selectVisits <- sample(unique(taxa_data$visit), size = ifelse(nuVisits>10000,
                                                      10000,
                                                      nuVisits))

taxa_data <- taxa_data %>%
  filter(visit %in% selectVisits)

# run 1
# some of the data are enormous -much more than we
# # need to assess forest cover
# taxa_data <- taxa_data %>%
#   sample_n(size = ifelse(length(TO_GRIDREF)>100000,
#                          100000,
#                          length(TO_GRIDREF)))

### coords ############################################

coords <- OSgrid2GB_EN(taxa_data$TO_GRIDREF)
#qplot(EASTING,NORTHING, data=coords)

taxa_data <- taxa_data %>%
  add_column(X = coords[,1],
             Y = coords[,2])

#EPSG:27700
#"+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs" 

### identify species for analysis ###################

#for simplicity...
taxa_data$Species <- taxa_data$CONCEPT

#sum number of records per species
speciesSummary <- taxa_data %>%
  group_by(Species) %>%
  summarise(nuRecs = length(Species),
            nuYears = length(unique(YEAR))) %>%
  ungroup() %>%
  arrange(desc(nuRecs))

#how many species do we have 500 records for
commonSpecies <- speciesSummary %>%
  filter(nuRecs > 50) %>%
  pull(Species) %>%
  as.character()

#report some feedback
message(paste('Total number of species:',nrow(speciesSummary)))
message(paste('Number of species passing threshold:',length(commonSpecies)))

### filter #########################################

#plot first set of species
# taxa_data %>%
#   filter(YEAR > 1979) %>%
#   filter(Species %in% commonSpecies[1:12]) %>%
#   ggplot() +
#   geom_point(aes(X,Y)) +
#   theme_minimal() +
#   facet_wrap(~Species, nrow=2)

#exclude Scotland and northern island
England <- raster::getData('GADM',country='GBR',level=1) %>%
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
  filter(st_intersects(taxa_data_spatial, England, sparse = FALSE)[,1]) 

message(paste('Number of records passing filter:',nrow(taxa_data)))

### organize data into visits #######################

visit_data <- taxa_data %>%
  group_by(visit, TO_GRIDREF, TO_STARTDATE, YEAR, X, Y) %>%
  summarise(nuSpecies = length(unique(Species))) %>%
  ungroup() %>%
  rename(SiteID = TO_GRIDREF,
         Year = YEAR,
         Date = TO_STARTDATE) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(yday = lubridate::yday(Date)) %>%
  mutate(yday2 = as.numeric(scale(yday^2))) %>%
  mutate(yday = as.numeric(base::scale(yday))) 

### list length #####################################

visit_data$LL <- ifelse(visit_data$nuSpecies==1,"single",
                        ifelse(visit_data$nuSpecies %in% 2:3,"short","long"))

### subsample visits #################################

#reduce sampling at sites with many visits in the same year

(visitsPerYear <- visit_data %>%
                  group_by(SiteID,Year) %>%
                  summarise(nuVisits = length(visit)))

#cap at 5 visits per site and year
visit_data <- visit_data %>%
                group_by(SiteID, X, Y, Year) %>%
                sample_n(size = ifelse(length(visit)>5,5,length(visit))) %>%
                ungroup()

nrow(visit_data)

#average number of sites visited per year
message(paste('Median number of sites visited per year:', median(table(visit_data$Year))))

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
grid1km <- readRDS(paste0(inputDir,"/grid1km_conifForest_allyears.rds")) %>%
              pivot_longer(cols = contains("fc"), 
                           names_to="forestYear", 
                           values_to="conifForest") %>%
              rename(SiteID = tile_name) %>%
              mutate(forestYear = as.numeric(gsub("fc_","",forestYear)))

#merge with taxa data based on forest cover of ordnance survey grid
visit_data <- inner_join(visit_data, grid1km, by=c("SiteID","forestYear")) %>%
                  filter(!is.na(conifForest))

#hist(visit_data$conifForest)

message('Forest cover sorted')

### species occupancy matrix ##############################

#align data frames

visit_data <- visit_data %>%
                mutate(visitID = as.numeric(as.factor(visit_data$visit))) %>%
                arrange(visitID)

taxa_data <- taxa_data %>%
                filter(visit %in% visit_data$visit) %>%
                mutate(visitID = as.numeric(as.factor(visit))) 
                

occMatrix <- reshape2::acast(taxa_data, visitID ~ Species, value.var = "Species",
                             fun=function(x)length(x))

#make response binary
occMatrix[occMatrix > 1] <- 1

#check things align
all(row.names(occMatrix) == visit_data$visitID)

### subsample ############################################

#function to subsample the data in the functions below
#c. 1000 presence locations
#c. 10000 absence locations

#uniform subset
#subSample <- function(visit_data){
#  
#presence data
# presData <- visit_data %>%
#               filter(Species==1)
# 
# #all data - over a range of forest cover values
# #take 100 in each forest cover bin
# absData <- visit_data %>%
#               mutate(conifForest_floor = floor(conifForest)) %>%
#               group_by(conifForest_floor) %>%
#               sample_n(size = ifelse(length(visit)>100,100,length(visit))) %>%
#               ungroup() %>%
#               filter(Species==0)
#             
# #combine
# return(bind_rows(presData, absData))
# 
# }

#random subset
# subSample <- function(visit_data){
# 
#   #presence data - all of them
#   presData <- visit_data %>%
#     filter(Species==1)
# 
#   #all data - over a range of forest cover values
#   #take 100 in each forest cover bin
#   absData <- visit_data %>%
#     sample_n(size = size = ifelse(length(visit)<5000,length(visit),5000)) %>%
#     filter(Species==0)
# 
#   #combine
#   return(bind_rows(presData, absData))
# 
# }

### basic glm #############################################

# I call it glm because we asssume a linear effect of forest cover
# the gam model is only used to model space

#general function
fitBasicGam <- function(myspecies){

  #check all aligns and add in species
  all(row.names(occMatrix) == visit_data$visitID)
  visit_data$Species <- occMatrix[,myspecies]

  #subsample?
  #visit_dataS <- subSample(visit_data)
  visit_dataS <- visit_data


  #fit gam and pull out forest cover effect
  require(mgcv)
  gam1 <- gam(Species ~ conifForest + yday + yday2 + s(X,Y) + Year,
              family = "binomial",
              data = visit_dataS)

  summary(gam1)$p.table[2,]

}

#apply function
gamOutput <- commonSpecies %>%
  map_dfr(fitBasicGam) %>%
  add_column(Species = commonSpecies) %>%
  arrange(desc(Estimate)) %>%
  janitor::clean_names()

saveRDS(gamOutput,file=paste0(outputDir,"/gamOutput_glm_subset_random_conif_",mytaxa,".rds"))

message('Basic glm done')

### niche shape ###########################################

hist(visit_data$conifForest) #most of the data is

fitGamNiche <- function(myspecies){

  #check all aligns and add in species
  all(row.names(occMatrix) == visit_data$visitID)
  visit_data$Species <- occMatrix[,myspecies]

  #subsample?
  #visit_dataS <- subSample(visit_data)
  visit_dataS <- visit_data

  #fit gam and pull out forest cover effect
  require(mgcv)
  gam1 <- gam(Species ~ s(conifForest,k=3) + yday + yday2 + s(X,Y) + Year,
              family = "binomial",
              data = visit_dataS)

  #predict the gam effect of forest cover
  newdata = data.frame(conifForest = seq(0,100,by=1),
                       yday = median(visit_data$yday),
                       yday2 = median(visit_data$yday2),
                       X = median(visit_data$X),
                       Y = median(visit_data$Y),
                       Year = median(visit_data$Year))

  newdata$preds <- predict(gam1,newdata, type="response")
  newdata$preds_se <- predict(gam1,newdata, se.fit=TRUE, type="response")$se.fit
  newdata$Species <- myspecies

  return(newdata)

}

gamOutput <- commonSpecies %>%
  map_dfr(fitGamNiche)

saveRDS(gamOutput,file=paste0(outputDir,"/gamOutput_gam_shape_subset_random_conif_",mytaxa,".rds"))

message('Basic gam done')

### basic gamm4 ###########################################

fitGammNiche <- function(myspecies){

  #check all aligns and add in species
  all(row.names(occMatrix) == visit_data$visitID)
  visit_data$Species <- occMatrix[,myspecies]

  #subsample
  #visit_dataS <- subSample(visit_data)
  visit_dataS <- visit_data

  #fit gam and pull out forest cover effect
  require(gamm4)
  gamm1 <- gamm4(Species ~ conifForest + yday + yday2 + LL + s(X,Y),
                 family = "binomial",
                 random = ~ (1|Year),
                 data = visit_dataS)

  summary(gamm1$gam)$p.table[2,]

}

#apply function
gamOutput <- commonSpecies %>%
  map_dfr(fitGammNiche) %>%
  add_column(Species = commonSpecies) %>%
  arrange(desc(Estimate)) %>%
  janitor::clean_names()


saveRDS(gamOutput,file=paste0(outputDir,"/gamOutput_gamm4_subset_random_conif_",mytaxa,".rds"))

message('Gamm done')

### gamm niche ###########################################

fitGammNiche <- function(myspecies){

  #check all aligns and add in species
  all(row.names(occMatrix) == visit_data$visitID)
  visit_data$Species <- occMatrix[,myspecies]

  #subsample
  #visit_dataS <- subSample(visit_data)
  visit_dataS <- visit_data

  #fit gam and pull out forest cover effect
  require(gamm4)
  gamm1 <- gamm4(Species ~ s(conifForest, k=3) + yday + yday2 + LL + s(X,Y),
                 random = ~ (1|Year),
                 family = "binomial",
                 data = visit_dataS)

  #predict the gam effect of forest cover
  newdata = data.frame(conifForest = seq(0,100,by=1),
                       yday = median(visit_data$yday),
                       yday2 = median(visit_data$yday2),
                       X = median(visit_data$X),
                       Y = median(visit_data$Y),
                       Year = median(visit_data$Year))

  newdata$preds <- predict(gamm1,newdata, type="response") # check this bit
  newdata$preds_se <- predict(gamm1,newdata, se.fit=TRUE, type="response")$se.fit
  newdata$Species <- myspecies

  return(newdata)

}

gamOutput <- commonSpecies %>%
  map_dfr(fitBasicGam) %>%
  add_column(Species = commonSpecies) %>%
  arrange(desc(Estimate)) %>%
  janitor::clean_names()


saveRDS(gamOutput,file=paste0(outputDir,"/gamOutput_gamm4_shape_subset_random_conif_",mytaxa,".rds"))

message('Gamm shape done')

### end #######################################################
