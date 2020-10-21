# Copy of stplanr's route_graphhopper function
# https://graphhopper.com/api/1/route?point=51.5196408546529%2C-0.0963020630915096&point=51.5061935192932%2C-0.240975652046149&vehicle=foot&locale=en-US&debug=true&points_encoded=false&key=9a5b0e50-7a25-4c03-8eba-b086a3901fe2

graphhopper_walkdist <- function(from, to, vehicle = NULL, startid = NULL, endid = NULL, pat = NULL, base_url = "https://graphhopper.com" ) {
  
  # Convert character strings to lon/lat if needs be
  coords <- od_coords(from, to)
  
  if (is.null(pat)) {
    pat <- api_pat("graphhopper")
  }
  
  httrmsg <- httr::modify_url(
    base_url,
    path = "/api/1/route",
    query = list(
      point = paste0(coords[1, c("fy", "fx")], collapse = ","),
      point = paste0(coords[1, c("ty", "tx")], collapse = ","),
      vehicle = vehicle,
      details = "road_class", 
      #details = "road_environment",
      locale = "en-US",
      debug = "true",
      points_encoded = "false",
      key = pat
    )
  )

  httrreq <- httr::GET(httrmsg)
  httr::stop_for_status(httrreq)
  
  if (grepl("application/json", httrreq$headers$`content-type`) == FALSE) {
    stop("Error: Graphhopper did not return a valid result")
  }
  
  txt <- httr::content(httrreq, as = "text", encoding = "UTF-8")
  if (txt == "") {
    stop("Error: Graphhopper did not return a valid result")
  }
  
  obj <- jsonlite::fromJSON(txt)
  
  if (is.element("message", names(obj))) {
    if (grepl("Wrong credentials", obj$message) == TRUE) {
      stop("Invalid API key")
    }
  }
  
  # Attribute data for the route
    df <- data.frame(
      startid = startid,
      endid = endid,
      routedist = obj$paths$distance / 1000
    )

  return(df)
  
}
