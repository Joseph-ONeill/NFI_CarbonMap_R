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
Trees_25M <- Trees_data %>% filter(DBH_mm <= 199)

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

# upper 1% of the data were omitted due to uncertainities associated-----

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

# Producing the clean dataframe----
Cleaned_Dataframe <- Cut_Out %>% dplyr::select(Plot_names, Year.x, District.x, Total_C_ha, EAST.x, NORTH.x, Zone.x)


Cleaned_Dataframe <- Cleaned_Dataframe %>% rename(Year = Year.x) %>% rename(District = District.x) %>% 
  rename(EAST = EAST.x) %>% rename(NORTH = NORTH.x) %>% 
  rename(Zone = Zone.x)

which(Cleaned_Dataframe$EAST==512000)
Wrong_Plot <- Cleaned_Dataframe %>% filter(Plot_names=="N512000E2077000")

Cleaned_Dataframe[7289, 5] = 215000

write.csv(Cleaned_Dataframe,file = "Data_Output/Cleaned_Data.csv",row.names = FALSE)

Final_Dataframe <- read.csv("Data_Output/Cleaned_Data.csv")


hist(Final_Dataframe$Total_C_ha, breaks = 100)


unique(Final_Dataframe$District)


PlotNumber_by_District <- Final_Dataframe %>%
  group_by(District) %>%
  summarise(count = n())


