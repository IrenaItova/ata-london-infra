routedistdf<- data.frame(startid=character(), endid=character(), routedist=numeric())
for (i in 1:20000){
  startid = edistdf$startid[i]
  endid = edistdf$endid[i]
  startpoint <- coordinates(cents_oa[cents_oa@data$OA11CD==startid,]) %>% as.numeric()
  endpoint <- coordinates(cents_filter[cents_filter@data$UID==endid,]) %>% as.numeric()
  df <- graphhopper_walkdist(from = startpoint, to = endpoint, vehicle = "foot", startid = startid, endid =  endid)
  routedistdf <- rbind(routedistdf, df)
  if (i %in% seq(1,100000,500)) {
    print(paste0(i," at ",Sys.time()))
  }
}
write_csv(routedistdf, file.path("02_DataCreated/1_route-mf-2020/2_routedistance_to_filter_to20000.csv"))

#TOMORROW
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
