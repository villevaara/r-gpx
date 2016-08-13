library(plotKML)
library(ggmap)

# GPX files downloaded from Runkeeper
files <- dir(pattern = "\\.gpx")

# Consolidate routes in one drata frame
index <- c()
latitude <- c()
longitude <- c()

for (i in 1:length(files)) {
  route <- readGPX(files[i])
  location <- route$tracks[[1]][[1]]
  index <- c(index, rep(i, dim(location)[1]))
  latitude <- c(latitude, location$lat)
  longitude <- c(longitude, location$lon)
}

routes <- data.frame(cbind(index, latitude, longitude))

# function to return map location based on routes
get_map_bounds <-function(routes){
  max_lat <- max(routes[2])
  min_lat <- min(routes[2])
  max_lon <- max(routes[3])
  min_lon <- min(routes[3])

  mapBounds <- c(min_lon, min_lat, max_lon, max_lat)
  return(mapBounds)
}


# function to get right zoom level
get_map_zoom <-function(mapBounds) {

  width <- mapBounds[4] - mapBounds[2]
  height <- mapBounds[3] - mapBounds[1]
  longest_side <- max(width, height)
  # just assuming it's along equator :D
  longest_side_kilometers <- longest_side / 360 * 40075
  # getting closest zoom for 640px
  # seems to be about 1km/100px at zoom 13, 2/100@12 , etc ...
  # 15 - 1.5
  # 14 - 3   
  # 13 - 6  
  # 12 - 12 
  # 11 - 24 
  # 10 - 48

  zoomSides = c(1.5,3,6,12,24,48)
  zoomLevel = 15

  for (zoomSide in zoomSides){
    if(longest_side_kilometers < zoomSide) {
      break
    }
    zoomLevel <- zoomLevel - 1
  }

  return(zoomLevel)
}

mapBounds <- get_map_bounds(routes)
# print(mapBounds)
# print(get_map_zoom(mapBounds))
mapZoom = get_map_zoom(mapBounds)

# Map the routes
# ids <- unique(index)
# plot(routes$longitude, routes$latitude, type="n", axes=FALSE, xlab="", ylab="", main="", asp=1)
# for (i in 1:length(ids)) {
#   currRoute <- subset(routes, index==ids[i])
#   lines(currRoute$longitude, currRoute$latitude, col="#00000020")
# }

tapiolaMap <- qmap(location = mapBounds, zoom = mapZoom, color = 'bw')


mapToPrint <- tapiolaMap +
  geom_path(aes(x = longitude, y = latitude, group = factor(index)), 
                colour="red", linetype = 1, size = 3, data = routes, alpha=0.3)

print(mapToPrint)