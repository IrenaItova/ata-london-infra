# SET UP
rm(list = ls())
library(geojsonio)
library(rgdal)
library(stplanr)
library(tidyverse)
#library(mapview)

source("00_Functions/graphhopper_routedist.R")

proj_27700 <- CRS("+init=epsg:27700")               # UK easting/northing projection - 'projected' (need if working in metres)
proj_4326 <- CRS("+proj=longlat +init=epsg:4326")   # global projection - lat/long.


####################
# PART 0: DEFINE OA IN LONDON
####################

# Open file
cents_oa <- readOGR(file.path("01_DataInput/cents_london_oa/0_downloaded/Output_Areas__December_2011__Population_Weighted_Centroids.shp"))

# Restrict to London
london_oa <- read_csv(file.path("01_DataInput/cents_london_oa/London_OA_list.csv")) # list given by nomis e.g. https://www.nomisweb.co.uk/census/2011/ks101ew
cents_oa <- cents_oa[cents_oa@data$OA11CD %in% london_oa$OA11CD,]

# Save in lat/long and in easting/northing
geojson_write(cents_oa, file = file.path("01_DataInput/cents_london_oa/OA_2011_London_eastnorth.geojson"))
cents_oa <- spTransform(cents_oa, proj_4326)
geojson_write(cents_oa, file = file.path("01_DataInput/cents_london_oa/OA_2011_London.geojson"))


####################
# PART 1: DEFINE OF PAIRS WITH CROW FLIES DISTANCE BELOW THRESHOLD
####################

# INPUT PARAMETERS
maxkm <- 1 # maximum distance do routing

# LOAD THE CSV EASTING-NORTHING COORDINATES OF ORIGINS AND DESTINATIONS
cents_oa_p <- read_csv(file.path("01_DataInput/cents_london_oa/OA_2011_London_eastnorth.csv")) # made manually by exporting the points in qgis; https://gis.stackexchange.com/questions/8844/getting-list-of-coordinates-for-points-in-layer-using-qgis
cents_filter_p <- read_csv(file.path("01_DataInput/modal_filters_2020/ModalFilters_Mar-Sept.csv"))

# MAKE A MATRIX OF OD PAIRS TO ROUTE
edistdf<- data.frame(startid=character(), endid=character(), edist=numeric())
for (i in 1:nrow(cents_oa_p)){
  for (j in 1:nrow(cents_filter_p)){
    edist <- (((cents_oa_p$easting[i]/1000 - cents_filter_p$X[j]/1000)^2) + ((cents_oa_p$northing[i]/1000 - cents_filter_p$Y[j]/1000)^2))^0.5
    if (edist < maxkm) {
      startid <- cents_oa_p$OA11CD[i]
      endid <- cents_filter_p$UID[j]
      df <- data.frame(startid = startid, endid = endid, edist = edist)
      edistdf <- rbind(edistdf, df)
      } else {
      }
  }
  print(paste0(i," at ",Sys.time()))
}
edistdf <- edistdf[order(edistdf$edist),]
write_csv(edistdf, file.path("02_DataCreated/1-route-mf-2020/1_edistance_to_filter.csv"))

####################
# PART 2: USE GRAPHHOPPER TO ROUTE SELECTED PAIRS
####################

# LOAD SPATIAL POINT FILES, CONVERT TO LAT/LONG
cents_oa <- readOGR(file.path("01_DataInput/cents_london_oa/OA_2011_London.geojson"))

cents_filter <- readOGR(file.path("01_DataInput/modal_filters_2020/ModalFilters_Mar-Sept.geojson"))
proj4string(cents_filter) <- proj_27700 # error in CRG code
cents_filter <- spTransform(cents_filter, proj_4326)

# OPEN CROW FLIES DISTANCE 
edistdf <- read_csv(file.path("02_DataCreated/1-route-mf-2020/1_edistance_to_filter.csv"))

# GRAPH HOPPER ROUTING (NB below min distance could fail?) (NB may need to do this in chunks to keep below day limit, then manually join together)
routedistdf<- data.frame(startid=character(), endid=character(), routedist=numeric())
for (i in 1:nrow(edistdf)){
  startid = edistdf$startid[i]
  endid = edistdf$endid[i]
  startpoint <- coordinates(cents_oa[cents_oa@data$OA11CD==startid,]) %>% as.numeric()
  endpoint <- coordinates(cents_filter[cents_filter@data$UID==endid,]) %>% as.numeric()
  df <- graphhopper_routedist(from = startpoint, to = endpoint, vehicle = "foot", startid = startid, endid =  endid)
  routedistdf <- rbind(routedistdf, df)
  if (i %in% seq(1,100000,500)) {  # give a message every 500 rows
    print(paste0(i," at ",Sys.time()))
  }
}
write_csv(routedistdf, file.path("02_DataCreated/1-route-mf-2020/2_routedistance_to_filter.csv"))
