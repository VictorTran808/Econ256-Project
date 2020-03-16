library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(sf)
Income <- read_csv("Raw-Dataset/Income.csv")
solar_permits <- read_csv("Raw-Dataset/solar_permits.csv")
tractsf <- read_sf(dsn = "Raw-Dataset", layer = "tl_2019_15_tract")


Oahutract <- filter(tractsf, COUNTYFP == "003")

Rename_Income <- Income %>% rename(GEOID = "ID Geography")
Rename_Income <- mutate(Rename_Income, GEOID = str_remove(Rename_Income$GEOID, "14000US"))
censusInc <-  left_join(Oahutract, Rename_Income, by='GEOID')
censusInc <- filter(censusInc, NAME != '9812')
censusInc <- filter(censusInc, Year == 2017 | is.na(Year) == TRUE)
censusInc <- filter(censusInc, NAME != '9900.01')


Household_Income <- censusInc$`Household Income by Race`
censusInc <- censusInc %>% rename(Household_Inc = "Household Income by Race")
#Import Icon
PV_Icon <- makeIcon(
    iconUrl = "Images/pv_icon.png",
    iconWidth = 30, iconHeight = 30
    
)
# PV_lon <- solar_permits$lng
# PV_lat <- solar_permits$lat

# Now redefining pal variable again, but as a colorNumeric function. 
pal <- colorNumeric(palette = "viridis", 
                    domain = Household_Income)



#=============================================================================
#_________________________USER INTERFACE______________________________________
ui <- dashboardPage(title = "Dashboard-test", skin = "green",
                    
                    dashboardHeader(title = "ECON 256 Project", 
                                    
                                    
                                    dropdownMenu(type = "tasks",
                                                 taskItem(
                                                     value = 60,
                                                     color = "green",
                                                     "PV"),
                                                 taskItem(
                                                     value = 0,
                                                     color = "green",
                                                     "EV"
                                                 ),
                                                 taskItem(
                                                     value = 50,
                                                     color = "green",
                                                     "Income"
                                                 ),
                                                 taskItem(
                                                     value = 0,
                                                     color = "green",
                                                     "Housing Price"
                                                 ),
                                                 taskItem(
                                                     value = 0,
                                                     color = "green",
                                                     "Electric Usage"
                                                 ),
                                                 taskItem(
                                                     value = 0,
                                                     color = "green",
                                                     "Solar Intensity"
                                                 )
                                                 
                                    )                
                                    
                                    
                    ),
                    
                    dashboardSidebar(
                        
                        sidebarMenu(
                            #menuItem("Dashboard Menu", tabName = "Dashboard_Menu"),
                            menuSubItem("PV", tabName = "PV"),
                            menuSubItem("EV", tabName = "EV"),
                            menuSubItem("Income", tabName = "Income"), 
                            menuSubItem("House Price", tabName = "House_Price"), 
                            menuSubItem("Electric Usage", tabName = "Electric_Usage"),
                            menuSubItem("Solar Intensity", tabName = "Solar_Intensity")
                            
                        )
                        
                        
                        
                    ),
                    
                    dashboardBody(
                        tabItems(
                                tabItem(tabName = "PV",h1("PV"), fluidRow(box(title = "PV location on Oahu",
                                                                              solidHeader = TRUE,
                                                                              background = "olive",
                                                                              leafletOutput("map",width = "100%", height=400)))),
                               
                                tabItem(tabName = "EV",h1("EV")),

                                tabItem(tabName = "Income",h1("Income"),fluidRow(box(title = "PV location on Oahu",
                                                                                     solidHeader = TRUE,
                                                                                     background = "olive",
                                                                                     leafletOutput("map_inc",height=500)))),

                                tabItem(tabName = "House_Price",h1("House Price")),

                                tabItem(tabName = "Electric_Usage",h1("Electric Usage")),

                                tabItem(tabName = "Solar_Intensity",h1("Solar Intensity"))
                            
                        )
                          
                        
                        
                    )
)
#_____________________________________________________________________________




#=============================================================================
#__________________________SERVER FUNCTION_____________________________________
server <- function(input, output) {
    
    output$map<-renderLeaflet({
   
        
        leaflet()%>% 
            addTiles()%>%
            addMarkers(data = solar_permits[1:500,], lng = ~lng, lat = ~lat, icon = PV_Icon, clusterOptions = markerClusterOptions())
        
        
        
              
    })
    output$map_inc <- renderLeaflet({
       
        #NEw remapping looks goods. Small issues such as the legend value are reverse
        #Lowest income at the top of the bar and highest at the bottom of the bar. 
        censusInc %>%
            st_transform(crs = "+init=epsg:4326") %>%
            leaflet(width = "100%") %>%
            #addProviderTiles(provider = "CartoDB.Positron") %>%
            addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                        stroke = FALSE,
                        smoothFactor = 0,
                        fillOpacity = 0.7,
                        color = ~ pal(Household_Income)) %>%
            addLegend("topright", 
                      pal = pal, 
                      values = ~ Household_Income,
                      title = "Household income",
                      labFormat = labelFormat(prefix = "$"),
                      opacity = 1) %>%
            addTiles()
    })
    # output$histogram <- renderPlot({
    #     hist(faithful$eruptions,breaks = input$bins)
    # })
    
}
#_____________________________________________________________________________




#=============================================================================
#__________________________RUN THE APPLICATION________________________________
shinyApp(ui = ui, server = server)