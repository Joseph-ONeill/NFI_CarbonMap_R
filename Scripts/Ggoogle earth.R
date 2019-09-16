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

# Extracting NDvi values for NFI Plots from the remote sensing image

library(raster)
library(sf)
library(rgdal)

# NDVI Values for 2018----
NFI2018_PLT <-raster("Data/NDVI2018_NFI.tif")
mypts <-readOGR("Plot_points/locations_WGS84.shp")
NFI2018_PLT <- crop(NFI2018_PLT, mypts)
plot(NFI2018_PLT)
plot(mypts, add = TRUE, cex=0.1)

mydata<-extract(NFI2018_PLT, mypts)
class(mydata)
NDVI2018_values <- as.data.frame(mydata)

colnames(NDVI2018_values)[colnames(NDVI2018_values)=="mydata"] <- "NDVI2018"

write.csv(NDVI_values, file = "Data_Output/NDVI2018.csv")

# 2010 NDVI Values----

NFI_PLT2010 <-raster("Data/NDVI2010_NFI.tif")
mypts <-readOGR("Plot_points/locations_WGS84.shp")
NFI_PLT2010 <- crop(NFI2010_PLT, mypts)
plot(NFI_PLT2010)
plot(mypts, add = TRUE, cex=0.1)

mydata<-extract(NFI_PLT2010, mypts)
class(mydata)
NDVI2010_values <- as.data.frame(mydata)

colnames(NDVI2010_values)[colnames(NDVI2010_values)=="mydata"] <- "NDVI2010"

write.csv(NDVI2010_values, file = "Data_Output/NDVI2010.csv")



# 2011 NDVI Values----

NFI_PLT2011 <-raster("Data/NDVI2011_NFI.tif")
mypts <-readOGR("Plot_points/locations_WGS84.shp")
NFI_PLT2011 <- crop(NFI_PLT2011, mypts)
plot(NFI_PLT2011)
plot(mypts, add = TRUE, cex=0.1)

mydata<-extract(NFI_PLT2011, mypts)
class(mydata)
NDVI2011_values <- as.data.frame(mydata)

colnames(NDVI2011_values)[colnames(NDVI2011_values)=="mydata"] <- "NDVI2011"

write.csv(NDVI2011_values, file = "Data_Output/NDVI2011.csv")

# 2012 NDVI Values----

NFI_PLT2012 <-raster("Data/NDVI2012_NFI.tif")
mypts <-readOGR("Plot_points/locations_WGS84.shp")
NFI_PLT2012 <- crop(NFI_PLT2012, mypts)
plot(NFI_PLT2012)
plot(mypts, add = TRUE, cex=0.1)

mydata<-extract(NFI_PLT2012, mypts)
class(mydata)
NDVI2012_values <- as.data.frame(mydata)

colnames(NDVI2012_values)[colnames(NDVI2012_values)=="mydata"] <- "NDVI2012"

write.csv(NDVI2012_values, file = "Data_Output/NDVI2012.csv")



# 2013 NDVI Values----

NFI_PLT2013 <-raster("Data/NDVI2013_NFI.tif")
mypts <-readOGR("Plot_points/locations_WGS84.shp")
NFI_PLT2013 <- crop(NFI_PLT2013, mypts)
plot(NFI_PLT2013)
plot(mypts, add = TRUE, cex=0.1)

mydata<-extract(NFI_PLT2013, mypts)
class(mydata)
NDVI2013_values <- as.data.frame(mydata)

colnames(NDVI2013_values)[colnames(NDVI2013_values)=="mydata"] <- "NDVI2013"

write.csv(NDVI2013_values, file = "Data_Output/NDVI2013.csv")


# 2014 NDVI Values----

NFI_PLT2014 <-raster("Data/NDVI2014_NFI.tif")
mypts <-readOGR("Plot_points/locations_WGS84.shp")
NFI_PLT2014 <- crop(NFI_PLT2014, mypts)
plot(NFI_PLT2014)
plot(mypts, add = TRUE, cex=0.1)

mydata<-extract(NFI_PLT2014, mypts)
class(mydata)
NDVI2014_values <- as.data.frame(mydata)

colnames(NDVI2014_values)[colnames(NDVI2014_values)=="mydata"] <- "NDVI2014"

write.csv(NDVI2014_values, file = "Data_Output/NDVI2014.csv")

# 2015 NDVI Values----

NFI_PLT2015 <-raster("Data/NDVI2015_NFI.tif")
mypts <-readOGR("Plot_points/locations_WGS84.shp")
NFI_PLT2015 <- crop(NFI_PLT2015, mypts)
plot(NFI_PLT2015)
plot(mypts, add = TRUE, cex=0.1)

mydata<-extract(NFI_PLT2015, mypts)
class(mydata)
NDVI2015_values <- as.data.frame(mydata)

colnames(NDVI2015_values)[colnames(NDVI2015_values)=="mydata"] <- "NDVI2015"

write.csv(NDVI2015_values, file = "Data_Output/NDVI2015.csv")


# 2016 NDVI Values----

NFI_PLT2016 <-raster("Data/NDVI2016_NFI.tif")
mypts <-readOGR("Plot_points/locations_WGS84.shp")
NFI_PLT2016 <- crop(NFI_PLT2016, mypts)
plot(NFI_PLT2016)
plot(mypts, add = TRUE, cex=0.1)

mydata<-extract(NFI_PLT2016, mypts)
class(mydata)
NDVI2016_values <- as.data.frame(mydata)

colnames(NDVI2016_values)[colnames(NDVI2016_values)=="mydata"] <- "NDVI2016"

write.csv(NDVI2016_values, file = "Data_Output/NDVI2016.csv")

# 2017 NDVI Values-------- 
NFI_PLT2017 <-raster("Data/NDVI2017_NFI.tif")
mypts <-readOGR("Plot_points/locations_WGS84.shp")
NFI_PLT2017 <- crop(NFI_PLT2017, mypts)
plot(NFI_PLT2017)
plot(mypts, add = TRUE, cex=0.1)

mydata<-extract(NFI_PLT2017, mypts)
class(mydata)
NDVI2017_values <- as.data.frame(mydata)

colnames(NDVI2017_values)[colnames(NDVI2017_values)=="mydata"] <- "NDVI2017"

write.csv(NDVI2017_values, file = "Data_Output/NDVI2017.csv")

# 2018 NDVI Values

NFI_PLT2018 <-raster("Data/NDVI2018_NFI_N.tif")
mypts <-readOGR("Plot_points/locations_WGS84.shp")
NFI_PLT2018 <- crop(NFI_PLT2018, mypts)
plot(NFI_PLT2017)
plot(mypts, add = TRUE, cex=0.1)

mydata<-extract(NFI_PLT2018, mypts)
class(mydata)
NDVI2018_values <- as.data.frame(mydata)

colnames(NDVI2018_values)[colnames(NDVI2018_values)=="mydata"] <- "NDVI2018"

write.csv(NDVI2018_values, file = "Data_Output/NDVI2018.csv")

