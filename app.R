# loading the libraries

# For creating the interactive app
library(shiny)

# For the world maps
library(maps) 
library(leaflet)


# For filtering data
library(dplyr)



TRI2022 <- read.csv("2022_us.csv")
TRI2022 <- TRI2022 %>%
  select(YEAR, FACILITY.NAME, CITY, ST, ZIP, LATITUDE, LONGITUDE, PARENT.CO.NAME, INDUSTRY.SECTOR, CHEMICAL, CLEAN.AIR.ACT.CHEMICAL,
         CLASSIFICATION, METAL, CARCINOGEN, PBT, PFAS, UNIT.OF.MEASURE, FUGITIVE.AIR, STACK.AIR, WATER, UNDERGROUND, LANDFILLS,
         ON.SITE.RELEASE.TOTAL, OFF.SITE.RELEASE.TOTAL, PRODUCTION.RATIO)

ui <- fluidPage(
  
  titlePanel("TRI Zip input test"),
  
  sidebarLayout(  # Corrected the typo here, it should be sidebarLayout, not sidebarlayout
    sidebarPanel(
      textInput("zip_input", "Enter Zip Code:", ""),
      actionButton("submit_btn", "Submit")
    ),
    mainPanel(
      tags$style("#mymap {height: 600px;}"),
      leafletOutput("mymap")  # Corrected the output ID to "mymap"
    )
  )
)

server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    zip_code <- as.numeric(input$zip_input)
    
    data_filtered <- TRI2022 %>%
      filter(ZIP == zip_code)
    
    if (nrow(data_filtered) == 0) {
      leaflet() %>%
        addTiles() %>%
        addMarkers(lat = 0, lng = 0) %>%
        addPopups(lat = 0, lng = 0, content = "There are no release sites in this zip code. Please try another.")
    } 
    else {
      leaflet(data_filtered) %>%
        addTiles() %>%
        addMarkers(lat = ~LATITUDE, 
                   lng = ~LONGITUDE,
                   popup = ~paste("Facility:", FACILITY.NAME, "<br>",
                                  "Industry:", INDUSTRY.SECTOR, "<br>",
                                  "Chemical:", CHEMICAL))
      
    }
  })
}

# call the shinyApp function with the ui and server as arguments
shinyApp(ui, server)




