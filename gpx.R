library(plotKML)
library(ggmap)

# GPX files downloaded from Runkeeper
files <- dir(pattern = "\\.gpx")
# lets test with a single file first
# file <- "./testrun.gpx"

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

# Map the routes
# ids <- unique(index)
# plot(routes$longitude, routes$latitude, type="n", axes=FALSE, xlab="", ylab="", main="", asp=1)
# for (i in 1:length(ids)) {
# 	currRoute <- subset(routes, index==ids[i])
# 	lines(currRoute$longitude, currRoute$latitude, col="#00000020")
# }

TapiolaMap <- qmap(location = 'tapiola, espoo', zoom = 13, color = 'bw')


print("debug1")
# testi
mapToPrint <- TapiolaMap +
	geom_path(aes(x = longitude, y = latitude, group = factor(index)), 
				colour="#1E2B6A", data = routes, alpha=0.3)
print("debug2")
print(mapToPrint)