# loading the libraries

# For creating the interactive app
library(shiny)
library(shinydashboard)

# For the world maps
library(maps) 
library(leaflet)


# For filtering data
library(dplyr)


# Reading in data file
TRI2022 <- read.csv("2022_us.csv")

# Filtering down to 25 variables instead of 119
TRI2022 <- TRI2022 %>%
  select(YEAR, FACILITY.NAME, CITY, ST, ZIP, LATITUDE, LONGITUDE, PARENT.CO.NAME, INDUSTRY.SECTOR, CHEMICAL, CLEAN.AIR.ACT.CHEMICAL,
         CLASSIFICATION, METAL, CARCINOGEN, PBT, PFAS, UNIT.OF.MEASURE, FUGITIVE.AIR, STACK.AIR, WATER, UNDERGROUND, LANDFILLS,
         ON.SITE.RELEASE.TOTAL, OFF.SITE.RELEASE.TOTAL, PRODUCTION.RATIO)

ui <- fluidPage(
  
  titlePanel("TRI Zip input test"),
  
  sidebarLayout(  
    sidebarPanel(
      textInput("zip_input", "Enter Zip Code:", ""),
      actionButton("submit_btn", "Submit") # So far this is useless, the map changes as soon as a zip is entered
    ),
    mainPanel(
      leafletOutput("mymap", height = "80vh")  
    )
  )
)

server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    zip_code <- as.numeric(input$zip_input) # make sure widget input is numeric instead of character string
    
    # filter data so only points from selected zip are used
    data_filtered <- TRI2022 %>% 
      filter(ZIP == zip_code)
    
    # if there are no data points for an entered zip code, show an error
    if (nrow(data_filtered) == 0) {
      leaflet() %>%
        addTiles() %>%
        addMarkers(lat = 0, lng = 0) %>%
        addPopups(lat = 0, lng = 0, content = "There are no release sites in this zip code. Please try another.")
    } 
    
    # when a valid zip is entered
    else {
      leaflet(data_filtered) %>%
        addTiles() %>%
        addMarkers(lat = ~LATITUDE, 
                   lng = ~LONGITUDE,
                   # when a user clicks on a maker info about the facility, industry, and chemical is listed
                   popup = ~paste("Facility:", FACILITY.NAME, "<br>",
                                  "Industry:", INDUSTRY.SECTOR, "<br>",
                                  "Chemical:", CHEMICAL))
      
    }
  })
}

# call the shinyApp function with the ui and server as arguments
shinyApp(ui, server)




