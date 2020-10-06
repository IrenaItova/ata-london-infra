# SET UP
rm(list = ls())
library(geojsonio)
library(rgdal)
library(stplanr)
library(tidyverse)

source("00_Functions/graphhopper_walkdist.R")

proj_27700 <- CRS("+init=epsg:27700")               # UK easting/northing projection - 'projected' (need if working in metres)
proj_4326 <- CRS("+proj=longlat +init=epsg:4326")   # global projection - lat/long.


####################
# PART 0: DEFINE LSOA IN LONDON
####################

# Open file, convert easting/northing
cents_lsoa <- readOGR(file.path("01_DataInput/london_lsoa_cents/0_downloaded/Lower_Layer_Super_Output_Areas_December_2011_Population_Weighted_Centroids.shp"))
cents_lsoa@data <- cents_lsoa@data[,c("lsoa11cd", "lsoa11nm")]

# Restrict to London
londonlsoa <- read_csv(file.path("01_DataInput/london_lsoa_cents/London_lsoa_list.csv")) # created by Anna
cents_lsoa <- cents_lsoa[cents_lsoa@data$lsoa11cd %in% londonlsoa$lsoa11cd,]

# Save
geojson_write(cents_lsoa, file = file.path("01_DataInput/london_lsoa_cents/LSOA_2011_London.geojson"))
cents_lsoa <- spTransform(cents_lsoa, proj_27700)
geojson_write(cents_lsoa, file = file.path("01_DataInput/london_lsoa_cents/LSOA_2011_London_eastnorth.geojson"))


####################
# PART 1: DEFINE OF PAIRS WITH CROW FLIES DISTANCE BELOW THRESHOLD
####################

# INPUT PARAMETERS
maxkm <- 2 # maximum distance do routing

# LOAD THE CSV EASTING-NORTHING COORDINATES OF ORIGINS AND DESTINATIONS
cents_lsoa_p <- read_csv(file.path("01_DataInput/london_lsoa_cents/LSOA_2011_London_eastnorth.csv")) # made manually by exporting the points in qgis; https://gis.stackexchange.com/questions/8844/getting-list-of-coordinates-for-points-in-layer-using-qgis
cents_filter_p <- read_csv(file.path("01_DataInput/pre2020_modal_filters/TfL_CID_modal_filter_EPSG27700.csv"))

# MAKE A MATRIX OF OD PAIRS TO ROUTE
edistdf<- data.frame(startid=character(), endid=character(), edist=numeric())
for (i in 1:nrow(cents_lsoa_p)){
  for (j in 1:nrow(cents_filter_p)){
    edist <- (((cents_lsoa_p$easting[i]/1000 - cents_filter_p$easting[j]/1000)^2) + ((cents_lsoa_p$northing[i]/1000 - cents_filter_p$northing[j]/1000)^2))^0.5
    if (edist < maxkm) {
      startid <- cents_lsoa_p$lsoa11cd[i]
      endid <- cents_filter_p$FEATURE_ID[j]
      df <- data.frame(startid = startid, endid = endid, edist = edist)
      edistdf <- rbind(edistdf, df)
      } else {
      }
  }
  print(paste0(i," at ",Sys.time()))
}
edistdf <- edistdf[order(edistdf$edist),]
write_csv(edistdf, file.path("02_DataCreated/1_route-existing-filters/1_edistance_to_filter.csv"))

####################
# PART 2: USE GRAPHHOPPER TO ROUTE SELECTED PAIRS
####################

# LOAD SPATIAL POINT FILES, CONVERT TO LAT/LONG
cents_lsoa <- readOGR(file.path("01_DataInput/london_lsoa_cents/LSOA_2011_London.geojson"))

cents_filter <- readOGR(file.path("01_DataInput/pre2020_modal_filters/TfL_CID_modal_filter_EPSG27700.geojson"))
proj4string(cents_filter) <- proj_27700 # error in CRG code
cents_filter <- spTransform(cents_filter, proj_4326)

# OPEN CROW FLIES DISTANCE 
edistdf <- read_csv(file.path("02_DataCreated/1_route-existing-filters/1_edistance_to_filter.csv"))

# GRAPH HOPPER ROUTING (NB below min distance could fail?)
routedistdf<- data.frame(startid=character(), endid=character(), routedist=numeric())
for (i in 1:nrow(edistdf)){
  startid = edistdf$startid[i]
  endid = edistdf$endid[i]
  startpoint <- coordinates(cents_lsoa[cents_lsoa@data$lsoa11cd==startid,]) %>% as.numeric()
  endpoint <- coordinates(cents_filter[cents_filter@data$FEATURE_ID==endid,]) %>% as.numeric()
  df <- graphhopper_walkdist(from = startpoint, to = endpoint, vehicle = "foot", startid = startid, endid =  endid)
  routedistdf <- rbind(routedistdf, df)
  if (i %in% seq(1,100000,250)) {
    print(paste0(i," at ",Sys.time()))
  }
}
write_csv(routedistdf, file.path("02_DataCreated/1_route-existing-filters/2_routedistance_to_filter.csv"))
