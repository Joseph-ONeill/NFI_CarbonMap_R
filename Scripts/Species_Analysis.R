rm(list = ls())

library(vegan)
library(tidyverse)
library(dplyr)

# All_Forests <- read.csv("Data/Forests_All_info.csv")
All_Forests <- read.csv("Data/Final_Dataframe_trees.csv")

All_Forests
sum(is.na(All_Forests$Species_names))
Abundance <- All_Forests %>%
  filter(!is.na(Species_names))%>%
  group_by(Plot_id, Species_names, Forest_type) %>%
  dplyr::summarise(Tree = n()) %>%
  spread(key=Species_names, value = Tree, fill=0)

Site_species <- Abundance[,c(3:nrow(Abundance))]
H<- diversity(Site_species, index = "shannon")
H <- data.frame(H)
SI <- diversity(Site_species, index = "simpson")
SI <- data.frame(SI)
J <- H/log(specnumber(Site_species))
J <- data.frame(J)
beta <- vegdist(Site_species, binary = TRUE)
beta <- data.frame(beta)

Abundance$Shannon <- H$H
Abundance$Simpson <- SI$SI
Abundance$Pieolou_evenness <- J$J

Species_Index <- Abundance[,c(1,2,178,179,180)]

All_Forest_data <- read.csv("Data/Final_Dataframe.csv")

All_Forest_data$Pieolou_evenness <- Species_Index$Pieolou_evenness

write.table(All_Forest_data, file = "Data/Final_Dataframe.csv", append = FALSE, quote = FALSE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "") 
# Dipterocarpus_Forests---------------------------------------------------------------

Dipterocarpus_Forest <- All_Forests %>%
  filter(Forest_type == "Dipterocarpus_Forest")
mean(Dipterocarpus_Forest$H_m)
mean(Dipterocarpus_Forest$Dbh_cm)

AbundanceDIP <- Dipterocarpus_Forest %>%
  filter(!is.na(Species_names))%>%
  group_by(Plot_id, Species_names) %>%
  dplyr::summarise(Tree = n()) %>%
  spread(key=Species_names, value = Tree, fill=0)

Site_speciesDIP <- AbundanceDIP[,c(2:nrow(AbundanceDIP))]

# All plots without the GPS locations are neglected in the Beta-diversity analysis and GDM analysis

# Shannon index
H_DIP <- diversity(Site_speciesDIP, index = "shannon")

mean(H_DIP)
# simpson index
SI_DIP <- diversity(Site_speciesDIP, index = "simpson")
mean(SI_DIP)
# Pieolou's evenness 
J_DIP <- H_DIP/log(specnumber(Site_speciesDIP))
mean(J_DIP)
#  The Sorensen index of dismillarity is used for the data

beta_DIP <- vegdist(Site_speciesDIP, binary = TRUE)

mean(beta_DIP)


# Dry_Forests---------------------------------------------------------------

Dry_Forest <- All_Forests %>%
  filter(Forest_type == "Dry_Forest")
mean(Dry_Forest$H_m)
mean(Dry_Forest$Dbh_cm)

AbundanceDF <- Dry_Forest %>%
  filter(!is.na(Species_names))%>%
  group_by(Plot_id, Species_names) %>%
  dplyr::summarise(Tree = n()) %>%
  spread(key=Species_names, value = Tree, fill=0)

Site_speciesDF <- AbundanceDF[,c(2:nrow(AbundanceDF))]

# All plots without the GPS locations are neglected in the Beta-diversity analysis and GDM analysis

# Shannon index
H_DF <- diversity(Site_speciesDF, index = "shannon")
mean(H_DF)

# simpson index
SI_DF <- diversity(Site_speciesDF, index = "simpson")
mean(SI_DF)

# Pieolou's evenness 
J_DF <- H_DF/log(specnumber(Site_speciesDF))
mean(J_DF, na.rm = T)
#  The Sorensen index of dismillarity is used for the data.
beta_DF <- vegdist(Site_speciesDF, binary = TRUE)
mean(beta_DF, na.rm = T)

anova(beta_DF, beta_DIP)
# Dry_Hill_Forests---------------------------------------------------------------

Dry_Hill_Forest <- All_Forests %>%
  filter(Forest_type == "Dry_Hill_Forest")
mean(Dry_Hill_Forest$H_m)
mean(Dry_Hill_Forest$Dbh_cm)


AbundanceDHF <- Dry_Hill_Forest %>%
  filter(!is.na(Species_names))%>%
  group_by(Plot_id, Species_names) %>%
  dplyr::summarise(Tree = n()) %>%
  spread(key=Species_names, value = Tree, fill=0)

Site_speciesDHF <- AbundanceDHF[,c(2:nrow(AbundanceDHF))]

# All plots without the GPS locations are neglected in the Beta-diversity analysis and GDM analysis

# Shannon index
H_DHF <- diversity(Site_speciesDHF, index = "shannon")
mean(H_DHF)

# simpson index
SI_DHF <- diversity(Site_speciesDHF, index = "simpson")
mean(SI_DHF)
# Pieolou's evenness 
J_DHF <- H_DHF/log(specnumber(Site_speciesDHF))
mean(J_DHF, na.rm = T)

#  The Sorensen index of dismillarity is used for the data.
beta_DHF <- vegdist(Site_speciesDHF, binary = TRUE)
mean(beta_DHF)

# Dry_Mixed_Deciduous_Forests---------------------------------------------------------------

Dry_Mixed_Deciduous_Forest <- All_Forests %>%
  filter(Forest_type == "Dry_Mixed_Deciduous_Forest")

mean(Dry_Mixed_Deciduous_Forest$H_m)
mean(Dry_Mixed_Deciduous_Forest$Dbh_cm)

AbundanceDMDF <- Dry_Mixed_Deciduous_Forest %>%
  filter(!is.na(Species_names))%>%
  group_by(Plot_id, Species_names) %>%
  dplyr::summarise(Tree = n()) %>%
  spread(key=Species_names, value = Tree, fill=0)

Site_speciesDMDF <- AbundanceDMDF[,c(2:nrow(AbundanceDMDF))]

# All plots without the GPS locations are neglected in the Beta-diversity analysis and GDM analysis

# Shannon index
H_DMDF <- diversity(Site_speciesDMDF, index = "shannon")
mean(H_DMDF)

# simpson index
SI_DMDF <- diversity(Site_speciesDMDF, index = "simpson")
mean(SI_DMDF)
# Pieolou's evenness 
J_DMDF <- H_DMDF/log(specnumber(Site_speciesDMDF))
mean(J_DMDF)

#  The Sorensen index of dismillarity is used for the data.
beta_DMDF <- vegdist(Site_speciesDMDF, binary = TRUE)
mean(beta_DMDF)

# Moist_Mixed_Deciduous_Forest---------------------------------------------------------------

Moist_Mixed_Deciduous_Forest <- All_Forests %>%
  filter(Forest_type == "Moist_Mixed_Deciduous_Forest")

mean(Moist_Mixed_Deciduous_Forest$H_m, na.rm = T)

mean(Moist_Mixed_Deciduous_Forest$Dbh_cm, na.rm = T)



AbundanceMMDF <- Moist_Mixed_Deciduous_Forest %>%
  filter(!is.na(Species_names))%>%
  group_by(Plot_id, Species_names) %>%
  dplyr::summarise(Tree = n()) %>%
  spread(key=Species_names, value = Tree, fill=0)

Site_speciesMMDF <- AbundanceMMDF[,c(2:nrow(AbundanceMMDF))]

# All plots without the GPS locations are neglected in the Beta-diversity analysis and GDM analysis

# Shannon index
H_MMDF <- diversity(Site_speciesMMDF, index = "shannon")
MMDF_data <- Moist_Mixed_Deciduous_Forest %>% group_by(Plot_id) %>% mutate(Shannon = H_MMDF)
mean(H_MMDF)
# simpson index
SI_MMDF <- diversity(Site_speciesMMDF, index = "simpson")
mean(SI_MMDF)
# Pieolou's evenness 
J_MMDF <- H_MMDF/log(specnumber(Site_speciesMMDF))
mean(J_MMDF, na.rm = T)
#  The Sorensen index of dismillarity is used for the data.
beta_MMDF <- vegdist(Site_speciesMMDF, binary = TRUE)
mean(beta_MMDF)


# Statistical analysis---------------------------

hist(H_DF)
hist(H_DHF)
hist(H_DIP)
hist(H_DMDF)
hist(H_MMDF)


anova(H_DF,H_DHF)
