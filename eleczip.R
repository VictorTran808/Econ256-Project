

library(sf)
library(ggplot2)
library(tidyverse)

#SORTING/CLEANING
eleczip <- read_csv("eleczip.csv")
eleczip$ZIP <- as.character(eleczip$ZIP)
zips <- read_sf(dsn = "zips", layer = "zips")
tidy_elec <- left_join(zips, eleczip, by = "ZIP")

#st_write(tidy_elec, dsn = "tidy_elec", layer = "tidy_elec", driver = "ESRI Shapefile")
#check <- read_sf(dsn = "tidy_elec", layer = "tidy_elec")

#PLOTTING
electricity <- read_sf(dsn = "tidy_elec", layer = "tidy_elec") #average electricity usage per zipcode
pvdata <- read_sf(dsn = "solarzip", layer = "solarzip") #zipcodes of ALL solar projects
ggplot()+
  geom_sf(data = electricity, aes(fill = electricity$kWh.mo))+
  scale_fill_gradient(low = "yellow", high = "orangered2", na.value = NA)+
  theme_void()+
  labs(title = "Average Electricity Used per Month According to Zip Code", caption = "Data Source: HECO & City and County of Honolulu", fill = "kWh per Month")+
  geom_sf(data = pvdata, color = "dodgerblue4") #ALL pv points



