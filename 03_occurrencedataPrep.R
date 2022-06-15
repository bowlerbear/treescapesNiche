#script to run occupancy models

# ### libraries #########################################

require(BRCmap)
require(tidyverse)
require(sf)

### get data ###########################################

dataDir <- "W:/PYWELL_SHARED/Pywell Projects/BRC/Charlie/1.c. New Model Rerun/1. Data/Cleaned Datasets"
load(paste(dataDir,"Carabids_170316_Cleaned_Data.rdata",sep="/"))

### coords ############################################

coords <- OSgrid2GB_EN(taxa_data$TO_GRIDREF)
#qplot(EASTING,NORTHING, data=coords)

taxa_data <- taxa_data %>%
  add_column(X = coords[,1],
             Y = coords[,2])

#EPSG:27700
#"+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"

### filter #########################################

#exclude Scotland and northern island
England <- UK_countries %>%
  st_as_sf() %>%
  filter(COUNTRY %in% c("England","Wales")) %>%
  st_transform(crs = 27700) %>%
  bind_rows() %>%
  st_union()

taxa_data_spatial <- taxa_data %>%
  st_as_sf(.,coords=c("X","Y"), crs=27700)

#filter on non-spatial data
taxa_data <- taxa_data %>%
  filter(st_intersects(taxa_data_spatial, England, sparse = FALSE)[,1]) %>%
  filter(YEAR > 1979)

message(paste('Number of records passing filter',nrow(taxa_data)))

### identify species for analysis ###################

#for simplicity...
taxa_data$Species <- taxa_data$CONCEPT

#sum number of records per species
speciesSummary <- taxa_data %>%
  filter(YEAR > 1979) %>%
  group_by(Species) %>%
  summarise(nuRecs = length(Species),
            nuYears = length(unique(YEAR))) %>%
  ungroup() %>%
  arrange(desc(nuRecs))

#how many species do we have 500 records for
commonSpecies <- speciesSummary %>%
  filter(nuRecs > 100) %>%
  pull(Species)

#report some feedback
message(paste('Total number of species',nrow(speciesSummary)))
message(paste('Number of species passing threshold',length(commonSpecies)))

### organize data into visits #######################

taxa_data$visit <- paste(taxa_data$TO_GRIDREF,taxa_data$TO_STARTDATE,sep="_")

visit_data <- taxa_data %>%
  group_by(visit, TO_GRIDREF, TO_STARTDATE, YEAR, X, Y) %>%
  summarise(nuSpecies = length(unique(Species))) %>%
  rename(SiteID = TO_GRIDREF,
         Year = YEAR,
         Date = TO_STARTDATE) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(yday = lubridate::yday(Date)) %>%
  mutate(yday2 = yday^2) %>%
  ungroup()

### get forest cover #################################

#ordnance survey grid
gridDir <- "C:/Users/diabow/OneDrive - UKCEH/Projects/General/OS-British-National-Grids-main"
#ogrListLayers(paste(gridDir,"os_bng_grids.gpkg",sep="/"))
grid1km <- st_read(paste(gridDir,"os_bng_grids.gpkg",sep="/"), layer='1km_grid')

#get forest cover (see forestcoverMaps.R script for pre-processing)
output <- readRDS("outputs/output_decidForest.rds")
grid1km$decidForest <- output$Broadleaved_percentCover.tif

#merge with taxa data based on forest cover of ordnance survey grid
visit_data$decidForest <- grid1km$decidForest[match(visit_data$SiteID,grid1km$tile_name)]

#remove those with missing data - just 3
visit_data <- subset(visit_data, !is.na(decidForest))

### get occupancy matrix ##############################

taxa_data <- subset(taxa_data, visit %in% visit_data$visit)

occMatrix <- reshape2::acast(taxa_data,visit ~ Species,value.var="Species",
                             fun=function(x)length(x))

#make response binary
occMatrix[occMatrix > 1] <- 1

#check things align
all(row.names(occMatrix) == visit_data$visit)

### save objects #########################################

#these are all inputs for the models so save in the inputs folder

saveRDS(visit_data, file="inputs/visit_data.rds")
saveRDS(taxa_data, file="inputs/taxa_data.rds")
saveRDS(occMatrix, file="inputs/occMatrix.rds")
saveRDS(commonSpecies, file="inputs/commonSpecies.rds")

### end #################################################