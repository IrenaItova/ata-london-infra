# SET UP
rm(list = ls())
library(dplyr)
library(geojsonio)
library(rgdal)
library(stplanr)
library(tidyverse)
#library(mapview)

source("00_Functions/graphhopper_routedist.R")

proj_27700 <- CRS("+init=epsg:27700")               # UK easting/northing projection - 'projected' (need if working in metres)
proj_4326 <- CRS("+proj=longlat +init=epsg:4326")   # global projection - lat/long.

####################
# PART 1: DEFINE OF PAIRS WITH CROW FLIES DISTANCE BELOW THRESHOLD
####################

# INPUT PARAMETERS
maxkm <- 2 # maximum distance do routing

# LOAD THE CSV EASTING-NORTHING COORDINATES OF OAs
cents_oa <- read_csv(file.path("01_DataInput/cents_london_oa/OA_2011_London_eastnorth.csv")) # made manually by exporting the points in qgis; https://gis.stackexchange.com/questions/8844/getting-list-of-coordinates-for-points-in-layer-using-qgis
# TEMPORARY! Restrict to HACKNEY
hackney_oa <- read_csv(file.path("01_DataInput/cents_london_oa/Hackney_oa_list.csv")) # made manually by Anna
cents_oa <- cents_oa[cents_oa$OA11CD %in% hackney_oa$OA11CD,]

# MAKE A MATRIX OF OD PAIRS TO ROUTE
edistdf<- data.frame(startid=character(), endid=character(), edist=numeric())
#for (i in 1:nrow(cents_oa)){
for (i in 1:10){ # TRY JUST 50 ORIGINS
  for (j in 1:nrow(cents_oa)){
    edist <- (((cents_oa$easting[i]/1000 - cents_oa$easting[j]/1000)^2) + ((cents_oa$northing[i]/1000 - cents_oa$northing[j]/1000)^2))^0.5
    if (edist < maxkm) {
      startid <- cents_oa$OA11CD[i]
      endid <- cents_oa$OA11CD[j]
      df <- data.frame(startid = startid, endid = endid, edist = edist)
      edistdf <- rbind(edistdf, df)
      } else {
      }
  }
  print(paste0(i," at ",Sys.time()))
}
edistdf <- edistdf[order(edistdf$edist),]
edistdf <- edistdf[edistdf$edist!=0,]
write_csv(edistdf, file.path("02_DataCreated/2-route-different-modes/1-edistance-between-pair.csv"))

####################
# PART 2: USE GRAPHHOPPER TO ROUTE SELECTED PAIRS
####################

# LOAD SPATIAL POINT FILES, CONVERT TO LAT/LONG
cents_oa <- readOGR(file.path("01_DataInput/cents_london_oa/OA_2011_London.geojson"))

# OPEN CROW FLIES DISTANCE 
edistdf <- read_csv(file.path("02_DataCreated/2-route-different-modes/1-edistance-between-pair.csv"))

# GRAPH HOPPER ROUTING (NB below min distance could fail?) (NB may need to do this in chunks to keep below day limit, then manually join together)
for (modename in c("foot", "bike", "car")) {
routelist <- list()
for (i in 1:nrow(edistdf)){
  startid = edistdf$startid[i]
  endid = edistdf$endid[i]
  startpoint <- coordinates(cents_oa[cents_oa@data$OA11CD==startid,]) %>% as.numeric()
  endpoint <- coordinates(cents_oa[cents_oa@data$OA11CD==endid,]) %>% as.numeric()
  routelist[[i]] <- graphhopper_routedist(from = startpoint, to = endpoint, vehicle = modename, startid = startid, endid =  endid, routeshape = T)
  if (i %in% seq(1,100000,500)) {  # give a message every 500 rows
    print(paste0(i," at ",Sys.time()))
  }
}
routes <- do.call(rbind, routelist) 
routes <- spTransform(routes, proj_27700)
#mapview::mapview(routes)
write_csv(routes@data, file.path(paste0("02_DataCreated/2-route-different-modes/2-routedistance-between-pair-",modename,".csv")))
geojson_write(routes, file = file.path(paste0("02_DataCreated/2-route-different-modes/2-routes-between-pair-",modename,".geojson")))
}

####################
# PART 3: COMBINE TOGETHER - CSVS
####################
combdist <- read_csv(file.path("02_DataCreated/2-route-different-modes/1-edistance-between-pair.csv"))
footdist <-  read_csv(file.path("02_DataCreated/2-route-different-modes/2-routedistance-between-pair-foot.csv"))
 combdist <- left_join(combdist, footdist, by = c("startid" = "startid", "endid" = "endid"))
 combdist <- dplyr::rename(combdist, footdistkm = `routedist`)
bikedist <-  read_csv(file.path("02_DataCreated/2-route-different-modes/2-routedistance-between-pair-bike.csv"))
 combdist <- left_join(combdist, bikedist, by = c("startid" = "startid", "endid" = "endid"))
 combdist <- dplyr::rename(combdist, bikedistkm = `routedist`)
cardist <-  read_csv(file.path("02_DataCreated/2-route-different-modes/2-routedistance-between-pair-car.csv"))
 combdist <- left_join(combdist, cardist, by = c("startid" = "startid", "endid" = "endid"))
 combdist <- dplyr::rename(combdist, cardistkm = `routedist`)
write_csv(combdist, file.path("02_DataCreated/2-route-different-modes/3-routedistance-between-pair-combined.csv"))
 