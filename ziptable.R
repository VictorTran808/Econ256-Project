
library(sf)
library(plyr)
library(tidyverse)

data <- read_sf(dsn = "masterdataV7", layer = "masterdataV7")%>% 
  st_drop_geometry()
tidy_elec <- read_sf(dsn = "tidy_elec", layer = "tidy_elec")%>% 
  st_drop_geometry()
tidy_edu <- read_sf(dsn = "tidy_edu", layer = "tidy_edu")%>% 
  st_drop_geometry()
tidy_income <- read_sf(dsn = "tidy_income", layer = "tidy_income")%>% 
  st_drop_geometry()

detach("package:plyr", unload = TRUE)
tidy_homevalue <- data %>%
  dplyr::group_by(zipcode)%>%
  dplyr::summarise(avghomevalue = mean(Hm_Indx))
tidy_homevalue <- rename(tidy_homevalue, "ZIP" = "zipcode")
tidy_homevalue$avghomevalue <- round(tidy_homevalue$avghomevalue)

tidy_solarrad <- data %>% 
  dplyr::group_by(zipcode) %>% 
  dplyr::summarise(avgsolarrad = mean(Sn_rdtn))
tidy_solarrad <- rename(tidy_solarrad, "ZIP" = "zipcode")
tidy_solarrad$avgsolarrad <- round(tidy_solarrad$avgsolarrad, 2)

tidy_pvconc <- data %>%
  dplyr::group_by(zipcode)%>%
  dplyr::summarise(pvconc = mean(ratio))
tidy_pvconc <- rename(tidy_pvconc, "ZIP" = "zipcode")
tidy_pvconc$pvconc <- tidy_pvconc$pvconc*100
tidy_pvconc$pvconc <- round(tidy_pvconc$pvconc, 2)

tidy_edu$percent <- round(tidy_edu$percent, 2)

library(plyr)
library(tidyverse)
ziptable <- join_all(list(tidy_income, tidy_homevalue, tidy_elec, tidy_edu, tidy_solarrad, tidy_pvconc), by='ZIP', type='left')
detach("package:plyr", unload = TRUE)

ziptable <- select(ziptable, "ZIP", "medinc", "avghomevalue", "kWh.mo", "percent", "avgsolarrad", "pvconc")
ziptable <- rename(ziptable, 
                   "Median Income" = "medinc",
                   "Zillow Home Value" = "avghomevalue",
                   "Electricity Usage (kWh per month)" = "kWh.mo",
                   "Estimated Percent of Population with College Degree" = "percent",
                   "Solar Radiation" = "avgsolarrad",
                   "Solar PV Instalation Concentration Percent" = "pvconc",
                   "ZipCode" = "ZIP")

summary_ziptable <- do.call(cbind, lapply(ziptable, summary))
summary_ziptable <- as.data.frame(summary_ziptable)
summary_ziptable <- select(summary_ziptable, -"ZipCode", -"Solar Radiation", -"Solar PV Instalation Concentration Percent")

data$Hm_Indx <- round(data$Hm_Indx)
data$Sn_rdtn <- round(data$Sn_rdtn, 2)
data$ratio <- data$ratio*100
data$ratio <- round(data$ratio, 2)
data$ratcllg <- data$ratcllg*100
data$ratcllg <- round(data$ratcllg, 2)

summary_data <- do.call(cbind, lapply(data, summary))
summary_data <- as.data.frame(summary_data)

summary_data <- rename(summary_data,
                       "Median Income" = "Income",
                       "Zillow Home Value" = "Hm_Indx",
                       "Electricity Usage (kWh per month)" = "kWh_mo",
                       "Estimated Percent of Population with College Degree" = "ratcllg",
                       "Solar Radiation" = "Sn_rdtn",
                       "Solar PV Instalation Concentration Percent" = "ratio")
summary_data <- select(summary_data, "Median Income", "Zillow Home Value", "Electricity Usage (kWh per month)", "Estimated Percent of Population with College Degree", "Solar Radiation", "Solar PV Instalation Concentration Percent")

#Note: All values (other than Median Income) are averages across the entire zipcode. 
#summary_ziptable is table of summary accross all zipcodes
#summary_data is table of summary across all PV installations



