require(tidyverse)
require(BRCmap)
require(sf)
require(tmap)

### choose data folder ######################################

#folders that contain cleaned BRC records:

# from Rob Boyd (should be versions of Charlies data)
#dataDir <- "W:/PYWELL_SHARED/Pywell Projects/BRC/Rob Boyd/TSDA/SDMs/Data/BRCRecords"

#or 

#direct from Charlies folder
dataDir <- "W:/PYWELL_SHARED/Pywell Projects/BRC/Charlie/1.c. New Model Rerun/1. Data/Cleaned Datasets"

### check folder #######################################

#get list of taxa files
allFiles <- list.files(dataDir) %>%
                str_subset(".rdata")

# read each one and get the names of the variables each data frame
# to check whether they are all standardized
lapply(allFiles, function(x){
  assign('newname', get(load(paste(dataDir,x,sep="/"))))
  print(x)
  print(names(newname))
})

#from Robin's folder:
# exclude file names that dont match
#"Ants_cleaned_data.rdata", "Birds_Cleaned_Data.rdata","Dragonflies_2020_Cleaned_Data.rdata",
#"Herptiles.rdata","Herptiles_Cleaned_Data.rdata","Moths_cleaned_data.rdata","VascularPlants_Cleaned_Data.rdata",
#"Woodpecker_Cleaned_Data.rdata"
#"Ants_cleaned_data.rdata" - just need to relabel SQ_1KM
#change "SQ_1KM" to "TO_GRIDREF"

#from Charlie's folder: 
# all look good - all have same column headings

### get all data #################################

allData <- list.files(dataDir, pattern = ".rdata") %>%
            str_subset("Plants",negate=TRUE) %>%
            lapply(.,function(x){
              get(load(paste(dataDir,x,sep="/"))) %>%
                mutate(File = x)}) 

#combine into one
allData <- allData %>%
            reduce(bind_rows) %>%
            mutate(TO_GRIDREF = as.character(TO_GRIDREF))

#fix grid labels
allData$TO_GRIDREF[is.na(allData$TO_GRIDREF)] <- as.character(allData$SQ_1KM)[is.na(allData$TO_GRIDREF)]

#check C3703 - previously a problem
#should now be stated also in TO_GRIDREF
subset(allData, SQ_1KM=="C3703")

### species ######################################

#extract from file name
allData$Taxa <- sapply(allData$File, function(x){
                            strsplit(as.character(x), "_")[[1]][1]
})

### time ########################################

floor_decade    = function(value){ return(value - value %% 10) }

allData$TO_STARTDATE <- as.Date(allData$TO_STARTDATE)
allData$YEAR <- lubridate::year(allData$TO_STARTDATE)
allData$Decade <- floor_decade(allData$YEAR)

### space #######################################

#get xy coordinates of the ordnance survey grids
allGrids <- sort(unique(allData$TO_GRIDREF))

coords <- OSgrid2GB_EN(gridref = allGrids) %>%
            as_tibble() %>%
            mutate(TO_GRIDREF = allGrids)
  
#add on to taxa data
allData <- allData %>%
  left_join(.,coords,by = "TO_GRIDREF")

summary(allData)

### region ####################################

#this step limits it to Britain

#get ordnance survey grid
# gridDir <- "C:/Users/diabow/OneDrive - UKCEH/Projects/General/OS-British-National-Grids-main"
# grid1km <- st_read(paste(gridDir,"os_bng_grids.gpkg",sep="/"), layer='1km_grid')
# 
# #get GBR shape
# GBR <- UK_countries %>%
#   st_as_sf() %>%
#   st_transform(crs = 27700)
# 
# #check they overlap
# tm_shape(GBR)+
#   tm_borders()+
# tm_shape(grid1km)+
#   tm_fill()
# 
# #get region of each grid
# grid1km <- st_join(grid1km, GBR) %>%
#             filter(!duplicated(tile_name)) %>%
#             rename(TO_GRIDREF = tile_name) %>%
#             as_tibble() %>%
#             select(TO_GRIDREF,COUNTRY)
# 
# 
# #add on to taxa data
# allData <- allData %>%
#   left_join(.,grid1km,by = "TO_GRIDREF")


#get GBR shape
GBR <- UK_countries %>%
  st_as_sf() %>%
  st_transform(crs = 27700)

#get region of each coord
coordsRegion <- st_as_sf(coords,coords=c("EASTING","NORTHING"),crs = 27700) %>%
  st_join(., GBR)

#where are the NAs? coastal
coordsRegion %>%
  filter(is.na(COUNTRY)) %>%
tm_shape()+
  tm_dots()

#buffer these points with missing data
coordsRegionBuffer <- st_as_sf(coords,coords=c("EASTING","NORTHING"),crs = 27700) %>%
                  st_buffer(dist = 500) %>%
                  filter(TO_GRIDREF %in% subset(coordsRegion,is.na(COUNTRY))$TO_GRIDREF) %>%
                  st_join(., GBR) #all now matchins

#combine all
allRegions <- coordsRegion %>%
                filter(!is.na(COUNTRY)) %>%
                bind_rows(coordsRegionBuffer) %>%
                as_tibble() %>%
                select(TO_GRIDREF,COUNTRY)

#add on to taxa data
allData <- allData %>%
  left_join(., allRegions, by = "TO_GRIDREF")

### summarise data ###############################

allData %>%
  filter(YEAR > 1969) %>%
  group_by(Taxa) %>%
  summarise(nuRecs = length(CONCEPT), 
            nuSpecies = length(unique(CONCEPT)),
            nuYears = length(unique(YEAR)),
            nu1kmGrids = length(unique(TO_GRIDREF))) %>%
  arrange(desc(nuRecs))

taxaSummary <- allData %>%
                filter(YEAR > 1969) %>%
                group_by(Taxa, COUNTRY) %>%
                summarise(nuRecs = length(CONCEPT), 
                          nuSpecies = length(unique(CONCEPT)),
                          nuYears = length(unique(YEAR)),
                          nu1kmGrids = length(unique(TO_GRIDREF))) %>%
                filter(!is.na(COUNTRY))

speciesSummary <- allData %>%
                    filter(YEAR > 1969) %>%
                    group_by(Taxa, CONCEPT, COUNTRY) %>%
                    summarise(nuRecs = length(CONCEPT), 
                    nuYears = length(unique(YEAR))) %>%
                    filter(nuRecs > 50) %>%
                    group_by(Taxa, COUNTRY) %>%
                    summarise(nuSpecies_least50recs = length(unique(CONCEPT)))
                    
taxaSummary <- left_join(taxaSummary,speciesSummary)     

write.table(taxaSummary, file="inputs/taxaSummary.txt",sep="\t",row.names=FALSE)
