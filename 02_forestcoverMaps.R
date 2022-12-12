library(tidyverse)
library(raster)
library(stars)
library (rgdal)
library (RSQLite)
library(BRCmap)
library(tmap)

# get ordnance survey grid
gridDir <- "C:/Users/diabow/OneDrive - UKCEH/Projects/General/OS-British-National-Grids-main"
ogrListLayers(paste(gridDir,"os_bng_grids.gpkg",sep="/"))
grid1km <- st_read(paste(gridDir,"os_bng_grids.gpkg",sep="/"), layer='1km_grid')

### broadleaf forest ##########################################################

# choose year
year <- 2015
yearPath <- paste0("C:/Users/diabow/OneDrive - UKCEH/Projects/Treescapes/data/LandCoverMaps/lcm")
ziplocation <- paste(paste0(yearPath, year),list.files(paste0(yearPath, year)), sep="/")
utils::unzip(ziplocation, list = TRUE)
temp <- tempfile()
utils::unzip(zipfile = ziplocation, 
      exdir = temp)

#pick one of the following according to the selected year

#2015
filePath <-  paste0(temp, "/",
              tools ::file_path_sans_ext(list.files(paste0(yearPath, year))),
              "/data/","LCM2015_GB_1km_percent_cover_target_class.tif")
#1990
filePath <-  paste0(temp, "/",
                    tools ::file_path_sans_ext(list.files(paste0(yearPath, year))),
                    "/data/","gb1990lcm1km_pc.tif")

#2000 - aggregated version - target version unavailable? but should be same
filePath <-  paste0(temp, "/",
                    tools ::file_path_sans_ext(list.files(paste0(yearPath, year))),
                    "/data/","LCM2000_GB_1K_PC_AGG.tif")

#2007
filePath <-  paste0(temp, "/",
                    tools ::file_path_sans_ext(list.files(paste0(yearPath, year))),
                    "/data/","lcm2007_aggregate_classes_1km_gb.img") 


lcMap <- raster(filePath, band=1) # band 1 is deciduous forest cover


# or forest change - 1990 vs 2015# yearPath <- paste0("C:/Users/diabow/OneDrive - UKCEH/Projects/Treescapes/data/LandCoverMaps/lcm")
# ziplocation <- paste(paste0(yearPath, year),list.files(paste0(yearPath, year)), sep="/")
# utils::unzip(ziplocation, list = TRUE)
# temp <- tempfile()
# utils::unzip(zipfile = ziplocation, 
#              exdir = temp)
# filePath <-  paste0(temp, "/",
#                     tools ::file_path_sans_ext(list.files(paste0(yearPath, year))),
#                     "/data/","LCC_GB_1990_to_2015.tif")
#year <- "_1990_2015"


#### mask #######################################################################

# change value outside of great britain extent to zero
plot(lcMap)
plot(subset(UK_countries, REGION=="Great Britain"),add=TRUE)
lcMap <- mask(lcMap, subset(UK_countries, REGION=="Great Britain"))
plot(lcMap)

#reproject grid if needs be to align with land cover data
if(!st_crs(st_as_stars(lcMap))==st_crs(grid1km)){
  grid1km <- grid1km %>%
              st_transform(st_crs(lcMap))
}

# extract for each ordnance survey grid
output <- aggregate(st_as_stars(lcMap), grid1km, FUN = mean, na.rm=T)
output <- st_as_sf(output)
saveRDS(output,paste0("inputs/decidForest_",year,".rds"))

#### add all to the grid ########################################################

allyears <- list.files("inputs", full.names = TRUE) %>%
              str_subset("decidForest_") %>%
              map(readRDS) %>%
              reduce(cbind) %>%
              janitor::clean_names() %>%
              as_tibble() %>%
              dplyr::select(contains("gb")) %>%
              add_column(tile_name = grid1km$tile_name) %>%
              filter(complete.cases(.)) 

#rename headings
names(allyears)[1:4] <- c("fc_1990","fc_2000","fc_2007","fc_2015")

saveRDS(allyears,"inputs/grid1km_decidForest_allyears.rds")          

### conif trees ############################################################

# choose year
year <- 2007
yearPath <- paste0("C:/Users/diabow/OneDrive - UKCEH/Projects/Treescapes/data/LandCoverMaps/lcm")
ziplocation <- paste(paste0(yearPath, year),list.files(paste0(yearPath, year)), sep="/")
utils::unzip(ziplocation, list = TRUE)
temp <- tempfile()
utils::unzip(zipfile = ziplocation, 
             exdir = temp)

#2015
filePath <-  paste0(temp, "/",
                    tools ::file_path_sans_ext(list.files(paste0(yearPath, year))),
                    "/data/","LCM2015_GB_1km_percent_cover_target_class.tif")
#1990
filePath <-  paste0(temp, "/",
                    tools ::file_path_sans_ext(list.files(paste0(yearPath, year))),
                    "/data/","gb1990lcm1km_pc.tif")

#2000 - aggregated version - target version unavailable? but should be same
filePath <-  paste0(temp, "/",
                    tools ::file_path_sans_ext(list.files(paste0(yearPath, year))),
                    "/data/","LCM2000_GB_1K_PC_AGG.tif")

#2007
filePath <-  paste0(temp, "/",
                    tools ::file_path_sans_ext(list.files(paste0(yearPath, year))),
                    "/data/","lcm2007_aggregate_classes_1km_gb.img") 


lcMap <- raster(filePath, band=2) # band 1 is deciduous forest cover, 2 is conif

#### mask #######################################################################

# change value outside of great britain extent to zero
plot(lcMap)
plot(subset(UK_countries, REGION=="Great Britain"),add=TRUE)
lcMap <- mask(lcMap, subset(UK_countries, REGION=="Great Britain"))
plot(lcMap)

#reproject grid if needs be to align with land cover data
if(!st_crs(st_as_stars(lcMap))==st_crs(grid1km)){
  grid1km <- grid1km %>%
    st_transform(st_crs(lcMap))
}

# extract for each ordnance survey grid
output <- aggregate(st_as_stars(lcMap), grid1km, FUN = mean, na.rm=T)
output <- st_as_sf(output)
saveRDS(output,paste0("inputs/conifForest_",year,".rds"))

#### add all to the grid ########################################################

allyears <- list.files("inputs", full.names = TRUE) %>%
  str_subset("conifForest_") %>%
  map(readRDS) %>%
  reduce(cbind) %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  dplyr::select(contains("gb")) %>%
  add_column(tile_name = grid1km$tile_name) %>%
  filter(complete.cases(.)) 

#rename headings
names(allyears)[1:4] <- c("fc_1990","fc_2000","fc_2007","fc_2015")

saveRDS(allyears,"inputs/grid1km_conifForest_allyears.rds")          

### plotting #############################################################

GBR <- UK_countries %>%
  st_as_sf() %>%
  filter(REGION %in% c("Great Britain")) %>%
  st_transform(crs = 27700) %>%
  bind_rows() %>%
  st_union()

#### forest cover change #####

fcLoss <- list.files("C:/Users/diabow/OneDrive - UKCEH/Projects/Treescapes/data/LandCoverMaps/lcm_1990_2015/data",
                       full.names = TRUE) %>%
                        terra::rast(.,lyrs=4) %>%
                        terra::app(., fun = function(x) ifelse(x==1,1,0)) %>%
                        aggregate(., fact=10, fun=mean)
                        

fcGain <- list.files("C:/Users/diabow/OneDrive - UKCEH/Projects/Treescapes/data/LandCoverMaps/lcm_1990_2015/data",
                     full.names = TRUE) %>%
                      terra::rast(.,lyrs=5) %>%
                      terra::app(., fun = function(x) ifelse(x==1,1,0)) %>%
                      aggregate(., fact=10, fun=mean)

t1 <- tm_shape(fcLoss) +
  tm_raster(title="% Woodland loss", col="values", palette="Greys", style="cont") +
  tm_shape(GBR) +
  tm_borders()

t2 <- tm_shape(fcGain) +
  tm_raster(title="% Woodland gain", col="values", palette="Greys", style="cont") +
  tm_shape(GBR) +
  tm_borders()

tmap_arrange(t1,t2)

#### biodiversity records ######

#run HPC_forestNiche_broadleaf for moths to get visit data

gridSummary <- visit_data %>%
                group_by(SiteID) %>%
                summarise(nuVisits = length(unique(visit))) %>%
                ungroup()

grid1km$nuVisits <- gridSummary$nuVisits[match(grid1km$tile_name,
                                               gridSummary$SiteID)]
grid1km$nuVisits[is.na(grid1km$nuVisits)] <- 0

tm_shape(grid1km) +
  tm_fill("nuVisits") 

### other data #############################################################

# # get forest data from Merryn
# forestDir <- "P:/07817 Connected treescapes/Data/Fragstats/LCM2020/Raster"
# decidRaster <- read_stars(paste(forestDir,"Broadleaved_percentCover.tif",sep="/")) 
# decidRaster[is.na(decidRaster)] <- 0
# plot(decidRaster)

# # data of Robin
# landcoverDir <- "W:/PYWELL_SHARED/Pywell Projects/BRC/Rob Boyd/TSDA/SDMs/Data/targetLandCover2007"
# blMap <- raster(paste(landcoverDir, "lc__bl_wood.asc", sep="/"))
# cMap <- raster(paste(landcoverDir, "lc__c_wood.asc", sep="/"))
# blData <- as.data.frame(blMap,xy=T)
# blData$GridRef <- gr_num2let(blData$x, blData$y, keep_precision = FALSE)
# blData$GridSimple <- sapply(blData$GridRef,function(x)substr(x,1,6))

### end ###################################################################
