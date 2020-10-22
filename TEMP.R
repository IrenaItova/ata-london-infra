# SET UP
rm(list = ls())
library(geojsonio)
library(rgdal)
library(stplanr)
library(tidyverse)
#library(mapview)

source("00_Functions/graphhopper_walkdist.R")

proj_27700 <- CRS("+init=epsg:27700")               # UK easting/northing projection - 'projected' (need if working in metres)
proj_4326 <- CRS("+proj=longlat +init=epsg:4326")   # global projection - lat/long.


# LOAD SPATIAL POINT FILES, CONVERT TO LAT/LONG
cents_oa <- readOGR(file.path("01_DataInput/cents_london_oa/OA_2011_London.geojson"))

cents_filter <- readOGR(file.path("01_DataInput/modal_filters_2020/ModalFilters_Mar-Sept.geojson"))
proj4string(cents_filter) <- proj_27700 # error in CRG code
cents_filter <- spTransform(cents_filter, proj_4326)

# OPEN CROW FLIES DISTANCE 
edistdf <- read_csv(file.path("02_DataCreated/1-route-mf-2020/1_edistance_to_filter.csv"))


#TOMORROW
routedistdf<- read_csv(file.path("02_DataCreated/1-route-mf-2020/2_routedistance_to_filter.csv"))
#CHECK!
#routedistdf<- data.frame(startid=character(), endid=character(), routedist=numeric())
for (i in 20001:nrow(edistdf)){
  startid = edistdf$startid[i]
  endid = edistdf$endid[i]
  startpoint <- coordinates(cents_oa[cents_oa@data$OA11CD==startid,]) %>% as.numeric()
  endpoint <- coordinates(cents_filter[cents_filter@data$UID==endid,]) %>% as.numeric()
  df <- graphhopper_walkdist(from = startpoint, to = endpoint, vehicle = "foot", startid = startid, endid =  endid)
  routedistdf <- rbind(routedistdf, df)
  if (i %in% seq(1,100000,500)) {  # give a message every 500 rows
    print(paste0(i," at ",Sys.time()))
  }
}
write_csv(routedistdf, file.path("02_DataCreated/1_route-mf-2020/2_routedistance_to_filter_from20001.csv"))
write_csv(routedistdf, file.path("02_DataCreated/1_route-mf-2020/2_routedistance_to_filter.csv"))
