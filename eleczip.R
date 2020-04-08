setwd("/Users/elie/Desktop/UH thangs/classes/econ256/econ proj")
library(sf) #simple feature
dataset <- read_sf(dsn = "masterdata", layer = "masterdata")
zips <- read_sf(dsn = "Zip_Codes", layer = "Zip_Codes")
eleczip <- read_csv("eleczip.csv")

#merge
eleczip$ZIP <- as.character(eleczip$ZIP)
electricity <- left_join(zips, eleczip, by = "ZIP")
dataset <- rename(dataset, ZIP = zipcode)

#plot
ggplot()+
  geom_sf(data = electricity, aes(fill = electricity$`kWh/mo`))+
  scale_fill_gradient(low = "yellow", high = "orangered2", na.value = NA)+
  theme_void()+
  labs(title = "Average Electricity Used per Month According to Zip Code", caption = "Data Source: HECO", fill = "kWh per Month")+
  geom_sf(data = dataset, color = "dark blue")

#export eleczip
write.csv(eleczip, "tidy_electric.CSV", row.names = FALSE)
write_sf(electricity, "tidy_electric.shp")

#NEW CODE
electricity <- read_sf(dsn = "tidy_electric", layer = "tidy_electric")
pvdata <- read_sf(dsn = "masterdata", layer = "masterdata")
ggplot()+
  geom_sf(data = electricity, aes(fill = electricity$kWh.mo))+
  scale_fill_gradient(low = "yellow", high = "orangered2", na.value = NA)+
  theme_void()+
  labs(title = "Average Electricity Used per Month According to Zip Code", caption = "Data Source: HECO", fill = "kWh per Month")+
  geom_sf(data = pvdata, color = "dodgerblue4")