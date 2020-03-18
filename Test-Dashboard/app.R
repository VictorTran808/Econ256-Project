#=============================================================================
#_________________________Import Packages_____________________________________
library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(sf)
#=============================================================================
#_________________________Import Dataset______________________________________
Income <- read_csv("Dataset/Income.csv")
solar_permits <- read_csv("Dataset/solar_permits.csv")
tractsf <- read_sf(dsn = "Dataset", layer = "tl_2019_15_tract")


#=============================================================================
#_________________________Cleaning Data_______________________________________
#Filter Honolulu County area only
Oahutract <- filter(tractsf, COUNTYFP == "003")
#Rename ID Geography into GEOID so the data can join the shape file data of Oahu
Rename_Income <- Income %>% rename(GEOID = "ID Geography")
Rename_Income <- mutate(Rename_Income, GEOID = str_remove(Rename_Income$GEOID, "14000US"))
#Combine the two data together
censusInc <-  left_join(Oahutract, Rename_Income, by='GEOID')
#Remove the Northwestern Hawaii Island from the Honolulu County
censusInc <- filter(censusInc, NAME != '9812')
#Filter only data from year 2017 & NA
censusInc <- filter(censusInc, Year == 2017 | is.na(Year) == TRUE)
#Remove this census which is the surround body of water around Oahu
censusInc <- filter(censusInc, NAME != '9900.01')
#Rename "Household Income by Race" to household_Inc
censusInc <- censusInc %>% rename(Household_Inc = "Household Income by Race")
#Lets try to remove unnescessary column
censusInc <- select(censusInc, -c("Household Income by Race Moe",
                                       "ID Race",
                                       "ID Year",
                                       "Race",
                                       "FUNCSTAT",
                                       "MTFCC",
                                       "Geography",
                                       "ALAND",
                                       "AWATER"))
#Renaming the variables for longitude & latitude
censusInc <- rename(censusInc, lng = "INTPTLON")
censusInc <- rename(censusInc, lat = "INTPTLAT")
#Turn all na values in Household_Inc to zero
censusInc$Household_Inc[is.na(censusInc$Household_Inc)] = 0

#================================================================================
#__________________________Import Graphic & Icons Here_____________________________
#Import Icon
PV_Icon <- makeIcon(
    iconUrl = "Images/pv_icon.png",
    iconWidth = 30, iconHeight = 30
    
)


#================================================================================
#_________________________Color & Aesthetic Variables Here_______________________
# Now redefining pal variable again, but as a colorNumeric function. 
Household_Income <- censusInc$Household_Inc
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
                    #The dashboardBody is where we call function we make in the server code black to be shown on the shiny app
                    #For example: In tabItem, where tabName = PV, I call the function leafletoutput("map"), where "map" is a variable I made in 
                    #the server code block called output$map
                    #Do the same for all the tabItem
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
    #This is a function called 'map', which display the interactive leaflet map for PV location
    output$map<-renderLeaflet({
   
        
        leaflet()%>% 
            addTiles()%>%
            addMarkers(data = solar_permits, lng = ~lng, lat = ~lat, icon = PV_Icon, clusterOptions = markerClusterOptions())
        
        
        
              
    })
    #This is the function called 'map_inc' which display the interactive choropleth map for Income
    output$map_inc <- renderLeaflet({
       
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
    
    #__________________________________________________________________________________________
    #Lenny & Elenor, pls write your function below when you are ready with your data
    #Use the form:
    #
    # output$name_of_your_variable <- type_of_render_you_are_using ({
    #
    #
    #   Copy & paste the code that does your plotting inside here
    #   
    #
    # })
    #________________________________________________________________________________________
    
    #EV code HERE
    
    
    
    
    
    #House Price HERE
    
    
    
    
    #Electric Usage HERE
    
    
    
    
    
    #Solar Intensity HERE
    
    
    
    
    
    
    #_______________________________________________________________________________________
    # Now the function 'name_of_your_variable' can be call into the dashboardbody code block
    # An example would be to call the function to where the tabItem of your selected data is 
    # Ex:
    # tabItem(tabName = "House_Price",h1("House Price"), name_of_your_variable)

    
    
}
#_____________________________________________________________________________




#=============================================================================
#__________________________RUN THE APPLICATION________________________________
shinyApp(ui = ui, server = server)