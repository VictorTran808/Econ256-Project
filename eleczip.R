





library(sf) #simple feature

#NEW CODE
electricity <- read_sf(dsn = "tidy_electric", layer = "tidy_electric") #average electricity usage per zipcode
pvdata <- read_sf(dsn = "solarzip", layer = "solarzip") #zipcodes of ALL solar projects
ggplot()+
  geom_sf(data = electricity, aes(fill = electricity$kWh.mo))+
  scale_fill_gradient(low = "yellow", high = "orangered2", na.value = NA)+
  theme_void()+
  labs(title = "Average Electricity Used per Month According to Zip Code", caption = "Data Source: HECO & City and County of Honolulu", fill = "kWh per Month")+
  geom_sf(data = pvdata, color = "dodgerblue4") #ALL pv points





