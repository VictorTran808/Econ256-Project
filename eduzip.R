
library(sf)
library(ggplot2)

####NEW
eduzip <- read.csv("nhgis0007_ds239_20185_2018_zcta.csv") #ACS 2014-2018 Estimates 25 yrs old +
eduzip$ZIP <- as.character(eduzip$ZCTA5A)
eduzip <- select(eduzip, "GISJOIN", "ZIP", "AJYPE001",
                 "AJYPE002", "AJYPE003", "AJYPE004", "AJYPE005", "AJYPE006", "AJYPE007", "AJYPE008", "AJYPE009", "AJYPE010", "AJYPE011", "AJYPE012", "AJYPE013", "AJYPE014", "AJYPE015", "AJYPE016",
                 "AJYPE017", "AJYPE018", "AJYPE019", "AJYPE020", "AJYPE021",
                 "AJYPE022", "AJYPE023", "AJYPE024", "AJYPE025")
zips <- read_sf(dsn = "zips", layer = "zips")
eduzip <- left_join(zips, eduzip, by = "ZIP")
eduzip$college <- eduzip$AJYPE022 + eduzip$AJYPE023 + eduzip$AJYPE024 + eduzip$AJYPE025
eduzip$percollege <- eduzip$college/eduzip$AJYPE001
eduzip <- select(eduzip,"ZIP", "college", "percollege")
pvdata <- read_sf(dsn = "solarzip", layer = "solarzip") #zipcodes of ALL solar projects
  
ggplot()+
  geom_sf(data = eduzip, aes(fill = eduzip$percollege))+
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = NA)+
  theme_void()+
  labs(title = "Estimated Percent of Population with 
at least a Bachelor's Degree According to Zip Code", caption = "Data Source: IPUMS ACS & City and County of Honolulu", fill = "Estimated Percentage")+
  geom_sf(data = pvdata, color = "black") #ALL pv points


tidy_edu <- eduzip
st_write(tidy_edu, dsn = "tidy_edu", layer = "tidy_edu", driver = "ESRI Shapefile")
check <- read_sf(dsn = "tidy_edu", layer = "tidy_edu")







