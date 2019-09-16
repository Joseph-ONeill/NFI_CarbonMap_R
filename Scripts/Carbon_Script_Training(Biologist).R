Test_Data <- read.csv("data/distance_to_cities.csv")
EX2Plot <- Test_Data %>%  filter(year==2011) %>% ggplot(mapping = aes(x=Total_C_ha, y=mean))+ geom_point(color="red")+ geom_smooth(color= "green")+theme_bw()

Survey_2012 <- Test_Data %>% select(Year, Total_C_ha, mean, District) %>% 
  filter(Year == 2012)
  
ggplot(Survey_2012, aes(x= Total_C_ha, y = mean))+ geom_point()+theme_bw()


ggplot(data = Test_Data, aes(x=Total_C_ha, y=mean))+ geom_point(aes(colour = District))+ geom_density_2d(data = Survey_2012, colour= "navyblue")


Test_Data <- Test_Data %>% mutate(Dist_Cities=Test_Data$mean) %>% select(-mean)
Test_Data$Dist_Cities <- Test_Data$Dist_Cities/60
Test_Data



EX2Plot <- Test_Data %>%  filter(year==2011) %>% ggplot(mapping = aes(x=Total_C_ha, y=Dist_Cities))+ geom_point(color="red")+ geom_smooth(color= "green")+theme_bw()

Survey_2012 <- Test_Data %>% select(Year, Total_C_ha, Dist_Cities, District) %>% 
  filter(Year == 2012)

ggplot(Survey_2012, aes(x= Total_C_ha, y = Dist_Cities))+ geom_point()+theme_bw()


ggplot(data = Test_Data, aes(x=Total_C_ha, y=Dist_Cities))+ geom_point(aes(colour = District))+ geom_density_2d(data = Survey_2012, colour= "navyblue")


Test_Data %>% mutate(Year= as.factor(Year)) %>% ggplot(mapping = aes(x=Year, y= Total_C_ha))+
  geom_boxplot()


A <- Test_Data %>% arrange(Total_C_ha, desc(Dist_Cities))

#  Creating a histrogram of the Total Carbon per ha

A %>% count(Total_C_ha, District) %>% ggplot(aes(Total_C_ha))+geom_histogram()

#  grouping data
A %>% group_by(District, Year) %>% 
  summarise(Mean_C = mean(Total_C_ha, Max_C= max(Total_C_ha))) %>% mutate(Year= as.factor(Year)) %>% 
  ggplot(aes(x=Year, y=Mean_C))+geom_boxplot()

Max_Carbon_each_Year <- A %>%  group_by(Year) %>% filter(Total_C_ha==max(Total_C_ha)) %>% 
  select(Plot_names, Dist_Cities, District, Total_C_ha)

