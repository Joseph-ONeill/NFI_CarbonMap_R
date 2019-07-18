install.packages("sf")
print(Cleaned_Data)
library(dplyr)
library("sp")
library("sf")
library("rgdal")


Grid46 <- Cleaned_Data %>% filter(Zone=="46")

Grid47 <- Cleaned_Data %>% filter(Zone=="47")

coords46 = SpatialPoints(cbind(Grid46$EAST, Grid46$NORTH))
proj4string(coords46) = CRS("+init=epsg:32646")
coords46<-spTransform(coords46, "+init=epsg:4326")

coords47 = SpatialPoints(cbind(Grid47$EAST, Grid47$NORTH))
proj4string(coords47) = CRS("+init=epsg:32647")
coords47<-spTransform(coords47, "+init=epsg:4326")

Plots_location <- rbind(coords46,coords47) 

spdf = SpatialPointsDataFrame(Plots_location,data = Cleaned_Data)

writeOGR(spdf,"locations_WGS84.shp", layer="plot_locations", driver = "ESRI Shapefile")
