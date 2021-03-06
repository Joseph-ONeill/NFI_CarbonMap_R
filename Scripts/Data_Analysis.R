# Data Analysis
# Clear R's Brain
rm(list = ls())

# Loading the required Packages---------

library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
library(maps)
library(mapdata)
library(maptools)
library(sp)
library(raster)
library(BIOMASS)
library(ape)
library(magrittr)
#
Trees <- read.csv("Data_Output/Main_DataFrame.csv")

# Omitting the EAST and NORT with NA values and taxonomy of trees are corrected by using the to correct typos in scientific names using the Taxonomic Name
# Resolution Service 

Trees <- Trees[!is.na(Trees$NORTH),]
sum(is.na(Trees$EAST))
summary(Trees)

Taxo <- correctTaxo(genus= Trees$Genus, species = Trees$Species)

Trees$genusCorr <- Taxo$genusCorrected
Trees$speciesCorr <- Taxo$speciesCorrected

APG <- getTaxonomy(Trees$genusCorr, findOrder =T)

Trees$familyAPG <- APG$family

Trees$orderAPG <- APG$order


# Obtaining Wood density - getWoodDensity assigns to each taxon a species- or genus-level average if at least one wood density value in the
# same genus as the focal taxon is available in the reference database. For unidentified trees or if the genus is missing in the reference database, the stand-level mean wood density is assigned
# to the tree (based on trees for which a value was attributed). 

dataWD <- getWoodDensity(genus=Trees$genusCorr,
                         species=Trees$speciesCorr,
                         stand=NULL, family = Trees$familyAPG, region = "World")

Trees <- Trees %>% mutate(DBH_cm=DBH_mm/10) %>% mutate(WD=dataWD$meanWD)

# Building a Diameter Height Model based on the first year data----------
Forests <- read.csv("Data/All_data_Final.csv")
HDmodels <- modelHD(D=Forests$Dbh_cm,
                    H = Forests$H_m,
                    drawGraph=TRUE,
                    useWeight=TRUE)

Best_HDmodel<- modelHD(D=Forests$Dbh_cm,
                       H=Forests$H_m,
                       method="log2",
                       useWeight =TRUE)

# Getting tree Height based on the best model -----------------------------------------------------------------------------


dataHlocal <-retrieveH(Trees$DBH_mm, Best_HDmodel)

Trees$H_m <- dataHlocal$H
Trees$WD_gcm3 <-  dataWD$meanWD

# Omitting trees having DBH less than 5cm-----
Trees <- Trees %>% filter(DBH_cm >= 5)

# Calculating the biomass of each tree----
# Where, D = Tree diameter (in cm), either a vector or a single value
# Wood density (in g/cm3), either a vector or a single value.
# Tree height (H in m), either a vector or a single value.
CoordsH <- cbind(Trees$EAST, Trees$NORTH)
Trees <- Trees %>% mutate(AGB=computeAGB(DBH_cm, WD_gcm3, H = H_m, coord = CoordsH, Dlim = NULL))
names(Trees)

Trees <- Trees %>% mutate(Carbon_Mg= AGB*0.471)
class(Trees)

head(Trees)
str(Trees)
Trees$genusCorr <- as.character(Trees$genusCorr)

write.csv(Trees, file = "Data/Trees_data.csv")



# Carbon_ha (assumed 1ha sample plots)--------
Trees <- read.csv("Data/Trees_data.csv")
Plot_carbon <- Trees %>% group_by(Plot_names,Year) %>% summarise(Carbon_ha = sum(Carbon_Mg))
summary(Plot_carbon$Carbon_ha)
class(Plot_carbon$Carbon_ha)
attach(Plot_carbon)
hist(Plot_carbon)

hist(Trees$DBH_cm, breaks = 50)
hist(Trees$H_m, breaks = 10)
plot(Trees$DBH_cm, Trees$H_m)


# Carbon_ha (1 ha sample plot) with different nested sampling method------

# Splitting the dataframe for large 50 m* 50m and medium plots 25m*25m-----------------------------------------------------------------------------------------

Trees_data <- read.csv("Data/Trees_data.csv")

head(Trees_data)
str(Trees_data)
Trees_50M <- Trees_data %>% filter(DBH_mm >= 200)
Trees_25M <- Trees_data %>% filter(DBH_mm < 200)

# Calculating the carbon per ha for the large plots------
Plot_50M_Cha <- Trees_50M %>%
  group_by(Plot_names, Year, District,EAST,NORTH, Zone) %>%
  dplyr::summarise(C_Tree_total = sum(Carbon_Mg)) %>%
  mutate(C_ha=C_Tree_total/0.25)

# Calculating the carbon per ha for the medium plots----------
Plot_25M_Cha <- Trees_25M %>%
  group_by(Plot_names, Year, District, EAST, NORTH, Zone) %>%
  dplyr::summarise(C_Tree_total = sum(Carbon_Mg)) %>%
  mutate(C_ha=C_Tree_total/0.0625)

# Combining dataframes of different plot size into one by replacing the NAs values with 0 so as to combine-----

Plot_Cha <- full_join(Plot_50M_Cha,Plot_25M_Cha, by= "Plot_names")

Plot_Cha$Plot_names <- as.character(Plot_Cha$Plot_names)
Plot_Cha$District.x <- as.character(Plot_Cha$District.x)
Plot_Cha$District.y <- as.character(Plot_Cha$District.y)

is.na(Plot_Cha)
str(Plot_Cha)
Plot_Cha[is.na(Plot_Cha)] <- 0

# As total trees number is for 1 ha plot.
# to get the mean carbon per ha
Plot_Cha <- Plot_Cha %>% mutate(Total_C_ha = C_ha.x+C_ha.y)

hist(Plot_Cha$Total_C_ha, breaks=100)

# upper 1% of the data was omitted due to uncertainities associated-----

Cut_Out <- Plot_Cha %>%
  filter(Total_C_ha <= 625.2924) 


# Filling out missing Districts, Years, NORTH and EAST-----

names(Cut_Out)
str(Cut_Out)

x <- which(Cut_Out$District.x==0)
Cut_Out$District.x[x]<- Cut_Out$District.y[x]

y <- which(Cut_Out$District.y==0)
Cut_Out$District.y[y] <- Cut_Out$District.x[y]


Missing_Year_X <- which(Cut_Out$Year.x==0)
Cut_Out$Year.x[Missing_Year_X] <- Cut_Out$Year.y[Missing_Year_X]

Missing_Year_Y <- which(Cut_Out$Year.y==0)
Cut_Out$Year.y[Missing_Year_Y] <- Cut_Out$Year.x[Missing_Year_Y]


y <- which(Cut_Out$District.y==0)
Cut_Out$District.y[y] <- Cut_Out$District.x[y]


Missing_EAST_X <- which(Cut_Out$EAST.x==0)
Cut_Out$EAST.x[Missing_EAST_X] <- Cut_Out$EAST.y[Missing_EAST_X]

Missing_EAST_Y <- which(Cut_Out$EAST.y==0)
Cut_Out$EAST.y[Missing_EAST_Y] <- Cut_Out$EAST.x[Missing_EAST_Y]


Missing_NORTH_X <- which(Cut_Out$NORTH.x==0)
Cut_Out$NORTH.x[Missing_NORTH_X] <- Cut_Out$NORTH.y[Missing_NORTH_X]

Missing_NORTH_Y <- which(Cut_Out$NORTH.y==0)
Cut_Out$NORTH.y[Missing_NORTH_Y] <- Cut_Out$NORTH.x[Missing_NORTH_Y]



Missing_Zone_X <- which(Cut_Out$Zone.x==0)
Cut_Out$Zone.x[Missing_Zone_X] <- Cut_Out$Zone.y[Missing_Zone_X]

Missing_Zone_Y <- which(Cut_Out$Zone.y==0)
Cut_Out$Zone.y[Missing_Zone_Y] <- Cut_Out$Zone.x[Missing_Zone_Y]


PYAY <- which(Cut_Out$District.x == "PYAY")
Cut_Out$District.x[PYAY] <- "Pyay"

PYAY2 <- which(Cut_Out$District.y == "PYAY")
Cut_Out$District.y[PYAY2] <- "Pyay"

names(Cut_Out)

# Producing a clean dataframe----
Cleaned_Dataframe <- Cut_Out %>% dplyr::select(Plot_names, Year.x, District.x, Total_C_ha, EAST.x, NORTH.x, Zone.x)


Cleaned_Dataframe <- Cleaned_Dataframe %>% rename(Year = Year.x) %>% rename(District = District.x) %>% 
  rename(EAST = EAST.x) %>% rename(NORTH = NORTH.x) %>% 
  rename(Zone = Zone.x)

which(Cleaned_Dataframe$EAST==512000)
Wrong_Plot <- Cleaned_Dataframe %>% filter(Plot_names=="N512000E2077000")

Cleaned_Dataframe[7289, 5] = 215000

write.csv(Cleaned_Dataframe,file = "Data_Output/Cleaned_Data.csv",row.names = FALSE)

Final_Dataframe <- read.csv("Data_Output/Cleaned_Data.csv")

# Loading the NDVI data for 7 years----
NDVI2018_values <- read.csv("Data_Output/NDVI2018.csv")
NDVI2017_values <- read.csv("Data_Output/NDVI2017.csv")
NDVI2016_values <- read.csv("Data_Output/NDVI2016.csv")
NDVI2015_values <- read.csv("Data_Output/NDVI2015.csv")
NDVI2014_values <- read.csv("Data_Output/NDVI2014.csv")
NDVI2013_values <- read.csv("Data_Output/NDVI2013.csv")
NDVI2012_values <- read.csv("Data_Output/NDVI2012.csv")
NDVI2011_values <- read.csv("Data_Output/NDVI2011.csv")
NDVI2010_N_values <- read.csv("Data_Output/NDVI2010.csv")


# Attached NDVI values to the Data Frame----
Final_Dataframe <- read.csv("Data_Output/Cleaned_Data.csv")
Final_Dataframe$NDVI2018 <- NDVI2018_values$NDVI
Final_Dataframe$NDVI2017 <- NDVI2017_values$NDVI2017
Final_Dataframe$NDVI2016 <- NDVI2016_values$NDVI2016
Final_Dataframe$NDVI2015 <- NDVI2015_values$NDVI2015
Final_Dataframe$NDVI2014 <- NDVI2014_values$NDVI2014
Final_Dataframe$NDVI2013 <- NDVI2013_values$NDVI2013
Final_Dataframe$NDVI2012 <- NDVI2012_values$NDVI2012
Final_Dataframe$NDVI2011 <- NDVI2011_values$NDVI2011
Final_Dataframe$NDVI2010 <- NDVI2010_values$NDVI2010
Final_Dataframe <- Final_Dataframe

#  Histrograms of NDVI and Carbon for 7 years------

hist(Final_Dataframe$Total_C_ha, breaks = 100)
hist(Final_Dataframe$NDVI2018, breaks = 100)
hist(Final_Dataframe$NDVI2017, breaks = 100)
hist(Final_Dataframe$NDVI2016, breaks = 100)
hist(Final_Dataframe$NDVI2015, breaks = 100)
hist(Final_Dataframe$NDVI2014, breaks = 100)
hist(Final_Dataframe$NDVI2013, breaks = 100)
hist(Final_Dataframe$NDVI2012, breaks = 100)
hist(Final_Dataframe$NDVI2011, breaks = 100)
hist(Final_Dataframe$NDVI2010, breaks = 100)

# NFI Carbon data and NDVI relationship-----

# Year2017---------------
NFI_2017 <- Final_Dataframe %>% filter(Year==2017)
plot(NFI_2017$Total_C_ha, (NFI_2017$NDVI2017)/10000)

#  Year 2016---------
NFI_2016 <- Final_Dataframe %>% filter(Year==2016)
plot(NFI_2016$Total_C_ha, (NFI_2016$NDVI2016)/10000)

#  Year 2015----
NFI_2015 <- Final_Dataframe %>% filter(Year==2015)
plot(NFI_2016$Total_C_ha, (NFI_2016$NDVI2015)/10000)

#  Year 2014----
NFI_2014 <- Final_Dataframe %>% filter(Year==2014)
plot(NFI_2014$Total_C_ha, (NFI_2014$NDVI2014)/10000)


#  Year 2013----
NFI_2013 <- Final_Dataframe %>% filter(Year==2013)
plot(NFI_2013$Total_C_ha, (NFI_2013$NDVI2013)/10000)


#  Year 2012----
NFI_2012 <- Final_Dataframe %>% filter(Year==2012)
plot(NFI_2012$Total_C_ha, (NFI_2012$NDVI2012)/10000)


#  Year 2011----
NFI_2011 <- Final_Dataframe %>% filter(Year==2011)
plot(NFI_2011$Total_C_ha, (NFI_2011$NDVI2011)/10000)

#  Year 2010----
NFI_2010 <- Final_Dataframe %>% filter(Year==2010)
plot(NFI_2010$Total_C_ha, (NFI_2010$NDVI2010)/10000)

# 7 Year NFI carbon and NDVI----
plot(Final_Dataframe$Total_C_ha,Final_Dataframe$NDVI2018)
plot(Final_Dataframe$Total_C_ha,Final_Dataframe$NDVI2017)
plot(Final_Dataframe$Total_C_ha,Final_Dataframe$NDVI2016)
plot(Final_Dataframe$Total_C_ha,Final_Dataframe$NDVI2015)
plot(Final_Dataframe$Total_C_ha,Final_Dataframe$NDVI2014)
plot(Final_Dataframe$Total_C_ha,Final_Dataframe$NDVI2013)
plot(Final_Dataframe$Total_C_ha,Final_Dataframe$NDVI2012)
plot(Final_Dataframe$Total_C_ha,Final_Dataframe$NDVI2011)
plot(Final_Dataframe$Total_C_ha,Final_Dataframe$NDVI2010)

Final_Dataframe$NDVI_cyr<-NA
Final_Dataframe$NDVI_cyr[Final_Dataframe$Year==2010]<-Final_Dataframe$NDVI2010[Final_Dataframe$Year==2010]
Final_Dataframe$NDVI_cyr[Final_Dataframe$Year==2011]<-Final_Dataframe$NDVI2011[Final_Dataframe$Year==2011]
Final_Dataframe$NDVI_cyr[Final_Dataframe$Year==2012]<-Final_Dataframe$NDVI2012[Final_Dataframe$Year==2012]
Final_Dataframe$NDVI_cyr[Final_Dataframe$Year==2013]<-Final_Dataframe$NDVI2013[Final_Dataframe$Year==2013]
Final_Dataframe$NDVI_cyr[Final_Dataframe$Year==2014]<-Final_Dataframe$NDVI2014[Final_Dataframe$Year==2014]
Final_Dataframe$NDVI_cyr[Final_Dataframe$Year==2015]<-Final_Dataframe$NDVI2015[Final_Dataframe$Year==2015]
Final_Dataframe$NDVI_cyr[Final_Dataframe$Year==2016]<-Final_Dataframe$NDVI2016[Final_Dataframe$Year==2016]
Final_Dataframe$NDVI_cyr[Final_Dataframe$Year==2017]<-Final_Dataframe$NDVI2017[Final_Dataframe$Year==2017]
Final_Dataframe$NDVI_cyr[Final_Dataframe$Year==2018]<-Final_Dataframe$NDVI2018[Final_Dataframe$Year==2018]

Final_Dataframe %>%
ggplot(aes(x = log(Total_C_ha), y = NDVI_cyr, color = Year)) +
  geom_point() +
  geom_smooth(method = 'lm')


lm1 <- lm(log(Total_C_ha)~NDVI_cyr,data = Final_Dataframe)
summary(lm1)

# 2 Year Avitabile and NDVI as Avitabile used 2011 and 2012 AGB maps to produce his map-----------------------------------
Data_AVT <- read.csv("Data/AGB_AVT_plots.csv")
Data_AVT <- Data_AVT %>% mutate(AVT_Carbon= 0.47*(Data_AVT$mean))
Data_AVT$NDVI2012 <- NDVI2012_values$NDVI2012
Data_AVT$NDVI2011 <- NDVI2011_values$NDVI2011

Data_AVT <- Data_AVT
plot(Data_AVT$Total_C_ha, Data_AVT$AVT_Carbon)

#  Year 2012----
AVT_2012 <- Data_AVT %>% filter(Year==2012)
plot(AVT_2012$AVT_Carbon, (AVT_2012$NDVI2012)/10000)


#  Year 2011----
AVT_2011 <- Data_AVT %>% filter(Year==2011)
plot(AVT_2011$AVT_Carbon, (AVT_2011$NDVI2011)/10000)


# 2 Year AVT carbon and NDVI----

plot(Data_AVT$AVT_Carbon,Data_AVT$NDVI2012)
plot(Data_AVT$AVT_Carbon,Data_AVT$NDVI2011)

Data_AVT$NDVI_cyr<-NA
Data_AVT$NDVI_cyr[Data_AVT$Year==2011]<-Data_AVT$NDVI2011[Data_AVT$Year==2011]
Data_AVT$NDVI_cyr[Data_AVT$Year==2012]<-Data_AVT$NDVI2012[Data_AVT$Year==2012]

Data_AVT %>%
  ggplot(aes(x = log(AVT_Carbon), y = NDVI_cyr, color = Year)) +
  geom_point() +
  geom_smooth(method = 'lm')

lm1 <- lm(log(AVT_Carbon)~NDVI_cyr,data = Data_AVT)
summary(lm1)



# summary observations..
unique(Final_Dataframe$District)
table(Final_Dataframe$District)
table(Final_Dataframe$Year)


PlotNumber_by_District <- Final_Dataframe %>%
  group_by(District) %>%
  summarise(count = n())

# Load up the values extracted from the AVitabile AGB Map----
  
Data_AVT <- read.csv("Data/AGB_AVT_plots.csv")
Data_AVT <- Data_AVT %>% mutate(AVT_Carbon= 0.47*(Data_AVT$mean))

# calculating the Average ABG value for each district for Aviatabile and NFI
Data_AVT_Dist <- Data_AVT %>% group_by(District) %>% summarise(mean_C_AVT = mean(AVT_Carbon, na.rm = T, n= n()))

mean_DistC_Total <-  Data_AVT %>% group_by(District) %>% summarise(mean_C_Total = mean(Total_C_ha, na.rm = T, n= n()))

Data_AVT_Dist$Mean_C_Field <- mean_DistC_Total$mean_C_Total

# Writing the csv file of Avitabile avearage carbon for each district

write.csv(Data_AVT_Dist, file = "Data_Output/CarbonDistrictL.csv", row.names = FALSE)

CarbonDistrictL <- read.csv("Data_Output/CarbonDistrictL.csv")

plot(CarbonDistrictL$mean_C_AVT, CarbonDistrictL$Mean_C_Field,col= "red", main= "Correlation between Avitabile Map Data and NFI Data") %>% abline()

CarbonDistrictL %>% ggplot(aes(x = District , y = mean_C_AVT, fill = District)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(
    x = "District",
    y = "Average Carbon Storage (Mg)",
    title = paste(
      "Average Carbon Storage by Avitabile Map"
    )
  )

CarbonDistrictL %>% ggplot(aes(x = District , y = Mean_C_Field, fill = District)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(
    x = "District",
    y = "Average Carbon Storage (Mg)",
    title = paste(
      "Average Carbon Storage by NFI"
    )
  )


ggplot(data = CarbonDistrictL, aes(x=mean_C_AVT, y=Mean_C_Field))+ geom_point(aes(colour = District))+ labs(xlab="Avitabile(ton/ha)", ylab = "NFI_Carbon(ton/ha)")

# Testing based on the training lessons
Test_Data <- read.csv("data/distance_to_cities.csv")
EX2Plot <- Test_Data %>%  filter(year==2011) %>% ggplot(mapping = aes(x=Total_C_ha, y=mean))+ geom_point(color="red")+ geom_smooth(color= "green")+theme_bw()

Survey_2012 <- Test_Data %>% select(Year, Total_C_ha, mean, District) %>% 
  filter(Year == 2012)

ggplot(Survey_2012, aes(x= Total_C_ha, y = mean))+ geom_point()+theme_bw()


ggplot(data = Test_Data, aes(x=Total_C_ha, y=mean))+ geom_point(aes(colour = District))+ geom_density_2d(data = Survey_2012, colour= "navyblue")
