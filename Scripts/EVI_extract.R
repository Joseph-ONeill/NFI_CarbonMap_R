
install.packages("sf")
install.packages("raster")
library(tidyverse)
library(dplyr)
library("sp")
library("sf")
library("rgdal")
library(raster)

# 2010 EVI Values----

NFI_PLT2010 <-raster("Data/EVI2010_NFI.tif")
mypts <-readOGR("Plot_points/locations_WGS84.shp")
NFI_PLT2010 <- crop(NFI2010_PLT, mypts)
plot(NFI_PLT2010)
plot(mypts, add = TRUE, cex=0.1)

mydata<-extract(NFI_PLT2010, mypts)
class(mydata)
EVI2010_values <- as.data.frame(mydata)

colnames(EVI2010_values)[colnames(EVI2010_values)=="mydata"] <- "EVI2010"

write.csv(EVI2010_values, file = "Data_Output/EVI2010.csv")



# 2011 EVI Values----

NFI_PLT2011 <-raster("Data/EVI2011_NFI.tif")
mypts <-readOGR("Plot_points/locations_WGS84.shp")
NFI_PLT2011 <- crop(NFI_PLT2011, mypts)
plot(NFI_PLT2011)
plot(mypts, add = TRUE, cex=0.1)

mydata<-extract(NFI_PLT2011, mypts)
class(mydata)
EVI2011_values <- as.data.frame(mydata)

colnames(EVI2011_values)[colnames(EVI2011_values)=="mydata"] <- "EVI2011"

write.csv(EVI2011_values, file = "Data_Output/EVI2011.csv")

# 2012 EVI Values----

NFI_PLT2012 <-raster("Data/EVI2012_NFI.tif")
mypts <-readOGR("Plot_points/locations_WGS84.shp")
NFI_PLT2012 <- crop(NFI_PLT2012, mypts)
plot(NFI_PLT2012)
plot(mypts, add = TRUE, cex=0.1)

mydata<-extract(NFI_PLT2012, mypts)
class(mydata)
EVI2012_values <- as.data.frame(mydata)

colnames(EVI2012_values)[colnames(EVI2012_values)=="mydata"] <- "EVI2012"

write.csv(EVI2012_values, file = "Data_Output/EVI2012.csv")



# 2013 EVI Values----

NFI_PLT2013 <-raster("Data/EVI2013_NFI.tif")
mypts <-readOGR("Plot_points/locations_WGS84.shp")
NFI_PLT2013 <- crop(NFI_PLT2013, mypts)
plot(NFI_PLT2013)
plot(mypts, add = TRUE, cex=0.1)

mydata<-extract(NFI_PLT2013, mypts)
class(mydata)
EVI2013_values <- as.data.frame(mydata)

colnames(EVI2013_values)[colnames(EVI2013_values)=="mydata"] <- "EVI2013"

write.csv(EVI2013_values, file = "Data_Output/EVI2013.csv")


# 2014 EVI Values----

NFI_PLT2014 <-raster("Data/EVI2014_NFI.tif")
mypts <-readOGR("Plot_points/locations_WGS84.shp")
NFI_PLT2014 <- crop(NFI_PLT2014, mypts)
plot(NFI_PLT2014)
plot(mypts, add = TRUE, cex=0.1)

mydata<-extract(NFI_PLT2014, mypts)
class(mydata)
EVI2014_values <- as.data.frame(mydata)

colnames(EVI2014_values)[colnames(EVI2014_values)=="mydata"] <- "EVI2014"

write.csv(EVI2014_values, file = "Data_Output/EVI2014.csv")

# 2015 EVI Values----

NFI_PLT2015 <-raster("Data/EVI2015_NFI.tif")
mypts <-readOGR("Plot_points/locations_WGS84.shp")
NFI_PLT2015 <- crop(NFI_PLT2015, mypts)
plot(NFI_PLT2015)
plot(mypts, add = TRUE, cex=0.1)

mydata<-extract(NFI_PLT2015, mypts)
class(mydata)
EVI2015_values <- as.data.frame(mydata)

colnames(EVI2015_values)[colnames(EVI2015_values)=="mydata"] <- "EVI2015"

write.csv(EVI2015_values, file = "Data_Output/EVI2015.csv")


# 2016 EVI Values----

NFI_PLT2016 <-raster("Data/EVI2016_NFI.tif")
mypts <-readOGR("Plot_points/locations_WGS84.shp")
NFI_PLT2016 <- crop(NFI_PLT2016, mypts)
plot(NFI_PLT2016)
plot(mypts, add = TRUE, cex=0.1)

mydata<-extract(NFI_PLT2016, mypts)
class(mydata)
EVI2016_values <- as.data.frame(mydata)

colnames(EVI2016_values)[colnames(EVI2016_values)=="mydata"] <- "EVI2016"

write.csv(EVI2016_values, file = "Data_Output/EVI2016.csv")

# 2017 EVI Values-------- 
NFI_PLT2017 <-raster("Data/EVI2017_NFI.tif")
mypts <-readOGR("Plot_points/locations_WGS84.shp")
NFI_PLT2017 <- crop(NFI_PLT2017, mypts)
plot(NFI_PLT2017)
plot(mypts, add = TRUE, cex=0.1)

mydata<-extract(NFI_PLT2017, mypts)
class(mydata)
EVI2017_values <- as.data.frame(mydata)

colnames(EVI2017_values)[colnames(EVI2017_values)=="mydata"] <- "EVI2017"

write.csv(EVI2017_values, file = "Data_Output/EVI2017.csv")

# 2018 EVI Values

NFI_PLT2018 <-raster("Data/EVI2018_NFI.tif")
mypts <-readOGR("Plot_points/locations_WGS84.shp")
NFI_PLT2018 <- crop(NFI_PLT2018, mypts)
plot(NFI_PLT2017)
plot(mypts, add = TRUE, cex=0.1)

mydata<-extract(NFI_PLT2018, mypts)
class(mydata)
EVI2018_values <- as.data.frame(mydata)

colnames(EVI2018_values)[colnames(EVI2018_values)=="mydata"] <- "EVI2018"

write.csv(EVI2018_values, file = "Data_Output/EVI2018.csv")


Final_Dataframe <- read.csv("Data_Output/Cleaned_Data.csv")

# Loading the EVI data for 7 years----
EVI2018_values <- read.csv("Data_Output/EVI2018.csv")
EVI2017_values <- read.csv("Data_Output/EVI2017.csv")
EVI2016_values <- read.csv("Data_Output/EVI2016.csv")
EVI2015_values <- read.csv("Data_Output/EVI2015.csv")
EVI2014_values <- read.csv("Data_Output/EVI2014.csv")
EVI2013_values <- read.csv("Data_Output/EVI2013.csv")
EVI2012_values <- read.csv("Data_Output/EVI2012.csv")
EVI2011_values <- read.csv("Data_Output/EVI2011.csv")
EVI2010_values <- read.csv("Data_Output/EVI2010.csv")


# Attached EVI values to the Data Frame----
Final_Dataframe <- read.csv("Data_Output/Cleaned_Data.csv")
Final_Dataframe$EVI2018 <- EVI2018_values$EVI
Final_Dataframe$EVI2017 <- EVI2017_values$EVI2017
Final_Dataframe$EVI2016 <- EVI2016_values$EVI2016
Final_Dataframe$EVI2015 <- EVI2015_values$mydata
Final_Dataframe$EVI2014 <- EVI2014_values$EVI2014
Final_Dataframe$EVI2013 <- EVI2013_values$EVI2013
Final_Dataframe$EVI2012 <- EVI2012_values$EVI2012
Final_Dataframe$EVI2011 <- EVI2011_values$EVI2011
Final_Dataframe$EVI2010 <- EVI2010_values$EVI2010
Final_Dataframe <- Final_Dataframe


# NFI Carbon data and EVI relationship-----
par(mfrow=c(3,3))

# Year2017---------------
NFI_2017 <- Final_Dataframe %>% filter(Year==2017)
plot(NFI_2017$Total_C_ha, (NFI_2017$EVI2017)/10000)

#  Year 2016---------
NFI_2016 <- Final_Dataframe %>% filter(Year==2016)
plot(NFI_2016$Total_C_ha, (NFI_2016$EVI2016)/10000)

#  Year 2015----
NFI_2015 <- Final_Dataframe %>% filter(Year==2015)
plot(NFI_2015$Total_C_ha, (NFI_2015$EVI2015/10000))

#  Year 2014----
NFI_2014 <- Final_Dataframe %>% filter(Year==2014)
plot(NFI_2014$Total_C_ha, (NFI_2014$EVI2014)/10000)


#  Year 2013----
NFI_2013 <- Final_Dataframe %>% filter(Year==2013)
plot(NFI_2013$Total_C_ha, (NFI_2013$EVI2013)/10000)


#  Year 2012----
NFI_2012 <- Final_Dataframe %>% filter(Year==2012)
plot(NFI_2012$Total_C_ha, (NFI_2012$EVI2012)/10000)


#  Year 2011----
NFI_2011 <- Final_Dataframe %>% filter(Year==2011)
plot(NFI_2011$Total_C_ha, (NFI_2011$EVI2011)/10000)

#  Year 2010----
NFI_2010 <- Final_Dataframe %>% filter(Year==2010)
plot(NFI_2010$Total_C_ha, (NFI_2010$EVI2010)/10000)

# 7 Year NFI carbon and EVI----
Final_Dataframe$EVI_cyr<-NA
Final_Dataframe$EVI_cyr[Final_Dataframe$Year==2010]<-Final_Dataframe$EVI2010[Final_Dataframe$Year==2010]
Final_Dataframe$EVI_cyr[Final_Dataframe$Year==2011]<-Final_Dataframe$EVI2011[Final_Dataframe$Year==2011]
Final_Dataframe$EVI_cyr[Final_Dataframe$Year==2012]<-Final_Dataframe$EVI2012[Final_Dataframe$Year==2012]
Final_Dataframe$EVI_cyr[Final_Dataframe$Year==2013]<-Final_Dataframe$EVI2013[Final_Dataframe$Year==2013]
Final_Dataframe$EVI_cyr[Final_Dataframe$Year==2014]<-Final_Dataframe$EVI2014[Final_Dataframe$Year==2014]
Final_Dataframe$EVI_cyr[Final_Dataframe$Year==2015]<-Final_Dataframe$EVI2015[Final_Dataframe$Year==2015]
Final_Dataframe$EVI_cyr[Final_Dataframe$Year==2016]<-Final_Dataframe$EVI2016[Final_Dataframe$Year==2016]
Final_Dataframe$EVI_cyr[Final_Dataframe$Year==2017]<-Final_Dataframe$EVI2017[Final_Dataframe$Year==2017]
Final_Dataframe$EVI_cyr[Final_Dataframe$Year==2018]<-Final_Dataframe$EVI2018[Final_Dataframe$Year==2018]

Final_Dataframe %>%
  ggplot(aes(x = log(Total_C_ha), y = EVI_cyr, color = Year)) +
  geom_point() +
  geom_smooth(method = 'lm')


lm1 <- lm(log(Total_C_ha)~EVI_cyr,data = Final_Dataframe)
summary(lm1)

ggplot(data = Final_Dataframe, mapping = aes(x = EVI_cyr, y = Year)) +
  geom_point() +
  facet_wrap(facets = vars(District))

# 2 Year Avitabile and EVI as Avitabile used 2011 and 2012 AGB maps to produce his map-----------------------------------
Data_AVT <- read.csv("Data/AGB_AVT_plots.csv")
Data_AVT <- Data_AVT %>% mutate(AVT_Carbon= 0.47*(Data_AVT$mean))
Data_AVT$EVI2012 <- EVI2012_values$EVI2012
Data_AVT$EVI2011 <- EVI2011_values$EVI2011

Data_AVT <- Data_AVT
Data_AVT %>% ggplot(aes(x=Total_C_ha, y=AVT_Carbon))+
  geom_point(lm)

# 2 Year AVT carbon and EVI----
#  Year 2012----
AVT_2012 <- Data_AVT %>% filter(Year==2012)
plot(AVT_2012$AVT_Carbon, (AVT_2012$EVI2012)/10000)


#  Year 2011----
AVT_2011 <- Data_AVT %>% filter(Year==2011)
plot(AVT_2011$AVT_Carbon, (AVT_2011$EVI2011)/10000)


# AVT carbon and EVI correlation according to map production years 2011 and 2012----
Data_AVT$EVI_cyr<-NA
Data_AVT$EVI_cyr <- Final_Dataframe$EVI2011
EVI_AVT2011 <- Data_AVT
class(EVI_AVT2011)

Data_AVT$EVI_cyr2<-NA
Data_AVT$EVI_cyr <- Final_Dataframe$EVI2012
EVI_AVT2012 <- Data_AVT


EVI_AVT2011 %>%
  ggplot(aes(x = log(AVT_Carbon), y = EVI_cyr/10000, color = Year)) +
  geom_smooth(method = "lm") 




