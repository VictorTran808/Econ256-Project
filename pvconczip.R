

library(sf)
library(tidyverse)

data <- read_sf(dsn = "masterdataV7", layer = "masterdataV7") %>% 
  st_drop_geometry()

tidy_pvconc <- data %>%
  dplyr::group_by(zipcode)%>%
  dplyr::summarise(pvconc = mean(ratio))
tidy_pvconc <- rename(tidy_pvconc, "ZIP" = "zipcode")
tidy_pvconc$pvconc <- tidy_pvconc$pvconc*100
tidy_pvconc$pvconc <- round(tidy_pvconc$pvconc, 2)
zips <- read_sf(dsn = "zips", layer = "zips")
tidy_pvconc <- left_join(zips, tidy_pvconc, by = "ZIP")

st_write(tidy_pvconc, dsn = "tidy_pvconc", layer = "tidy_pvconc", driver = "ESRI Shapefile")
check <- read_sf(dsn = "tidy_pvconc", layer = "tidy_pvconc")


#PLOTTING
ggplot()+
  geom_sf(data = tidy_pvconc, aes(fill = tidy_pvconc$pvconc), color = "black")+
  scale_fill_gradient(low = "thistle", high = "purple4", na.value = NA)+
  theme_void()+
  labs(title = "Concentration of PV per Zipcode", caption = "Data Source: Point2Homes & City and County of Honolulu", fill = "Estimated Percentage 
of Households 
with PV installed")




