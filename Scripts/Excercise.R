download.file(url="https://ndownloader.figshare.com/files/2292169",
              destfile = "data/portal_data_joined.csv")
surveys <- read_csv("data/portal_data_joined.csv")
surveys %>% count(sex)
surveys %>% group_by(sex) %>% summarise(count= n())
surveys %>% count(sex, sort = TRUE)
surveys %>% count(sex, species)
surveys %>% filter(!is.na(sex)) %>%
  count(sex, species) %>% arrange(species, desc(n))

surveys %>% count(plot_type)

surveys %>% filter(!is.na(hindfoot_length)) %>% group_by(species_id) %>% 
  summarise(mean_hindfoot_length = mean(hindfoot_length), min_hindfoot_length = min(hindfoot_length)
            , max_hindfoot_length = max(hindfoot_length), n = n()) 


surveys %>% filter(!is.na(weight)) %>% 
  group_by(year) %>% 
  filter(weight==max(weight)) %>% 
  select(year,genus,species, weight) %>%
  arrange(year)

surveys_gw <- surveys %>% 
  filter(!is.na(weight)) %>% 
  group_by(genus, plot_id) %>% 
  summarise(mean_weight=mean(weight))

str(surveys_gw)

surveys_spread <- surveys_gw %>% 
  spread(key = genus, value = mean_weight)

str(surveys_spread)

surveys_gw %>% spread(genus, mean_weight, fill = 0) %>% 
  head()

surveys_gather <- surveys_spread %>% 
  gather(key=genus, value = mean_weight, -plot_id)

surveys_spread %>% gather(key = genus, value = mean_weight, Baiomys:Spermophilus) %>% 
  head()

rich_time <- surveys %>% group_by(plot_id, year) %>% summarise(n_genera = n_distinct(genus)) %>% 
  spread(year,n_genera)

rich_time %>% gather(year, n_genera, -plot_id)

surveys_long <- surveys %>% 
  gather(measurement, value,hindfoot_length, weight)

surveys_long %>% group_by(year,measurement,plot_type) %>% summarise(mean_value = mean(value, na.rm = T)) %>% 
  spread(measurement,mean_value)
