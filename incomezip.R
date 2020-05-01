
library(sf)
library(ggplot2)

income <- read_csv("nhgis0008_ds239_20185_2018_zcta.csv") #2014-2018 estimates of median household income according to zipcode from IPUMS
income$ZIP <- as.character(income$ZCTA5A)
income$medinc <- income$AJZAE001
income <- select(income, "ZIP", "medinc")

zips <- read_sf(dsn = "zips", layer = "zips")
incomezip <- left_join(zips, incomezip, by = "ZIP")
incomezip <- select(incomezip, "ZIP", "medinc")


#tidy_income <- incomezip
#st_write(tidy_income, dsn = "tidy_income", layer = "tidy_income", driver = "ESRI Shapefile")
#check <- read_sf(dsn = "tidy_income", layer = "tidy_income")

pvdata <- read_sf(dsn = "solarzip", layer = "solarzip")

ggplot()+
  geom_sf(data = incomezip, aes(fill = incomezip$medinc))+
  scale_fill_gradient(low = "#ffecde", high = "#5c2600", na.value = NA)+
  theme_void()+
  labs(title = "Median Household Income According to Zipcode", caption = "Data Source: IPUMS ACS & City and County of Honolulu", fill = "Median Household Income")+
  geom_sf(data = pvdata, color = "black") #ALL pv points


