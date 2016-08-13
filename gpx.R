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
GetMapBounds <-function(routes){
  max.lat <- max(routes[2])
  min.lat <- min(routes[2])
  max.lon <- max(routes[3])
  min.lon <- min(routes[3])

  map.bounds <- c(min.lon, min.lat, max.lon, max.lat)
  return(map.bounds)
}


# function to get right zoom level
GetMapZoom <-function(map.bounds) {

  width <- map.bounds[4] - map.bounds[2]
  height <- map.bounds[3] - map.bounds[1]
  longest.side <- max(width, height)
  # just assuming it's along equator :D
  longest.side.km <- longest.side / 360 * 40075
  # getting closest zoom for 640px
  # seems to be about 1km/100px at zoom 13, 2/100@12 , etc ...
  # 15 - 1.5
  # 14 - 3   
  # 13 - 6  
  # 12 - 12 
  # 11 - 24 
  # 10 - 48

  zoom.sides <- c(1.5,3,6,12,24,48)
  zoom.level <- 15

  for (zoom.side in zoom.sides){
    if(longest.side.km < zoom.side) {
      break
    }
    zoom.level <- zoom.level - 1
  }

  return(zoom.level)
}

map.bounds <- GetMapBounds(routes)
# print(mapBounds)
# print(get_map_zoom(mapBounds))
map.zoom <- GetMapZoom(map.bounds)

# Map the routes
# ids <- unique(index)
# plot(routes$longitude, routes$latitude, type="n", axes=FALSE, xlab="", ylab="", main="", asp=1)
# for (i in 1:length(ids)) {
#   currRoute <- subset(routes, index==ids[i])
#   lines(currRoute$longitude, currRoute$latitude, col="#00000020")
# }

tapiola.map <- qmap(location = map.bounds, zoom = map.zoom, color = 'bw')


map.to.print <- tapiola.map +
  geom_path(aes(x = longitude, y = latitude, group = factor(index)), 
                colour="red", linetype = 1, size = 3, data = routes, alpha=0.3)

print(map.to.print)