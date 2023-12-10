# For creating the interactive app
library(shiny)
library(shinydashboard)
library(shinyjs)

# For the world maps
library(maps) 
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)

# For filtering data
library(dplyr)

# Reading in data file
TRI2022 <- read.csv("2022_us.csv")

# Filtering down to 25 variables instead of 119
TRI2022 <- TRI2022 %>%
  select(YEAR, FACILITY.NAME, CITY, ST, ZIP, LATITUDE, LONGITUDE, PARENT.CO.NAME, 
         INDUSTRY.SECTOR, CHEMICAL, CLEAN.AIR.ACT.CHEMICAL,
         CLASSIFICATION, METAL, CARCINOGEN, PBT, PFAS, UNIT.OF.MEASURE, 
         FUGITIVE.AIR, STACK.AIR, WATER, UNDERGROUND, LANDFILLS,
         ON.SITE.RELEASE.TOTAL, OFF.SITE.RELEASE.TOTAL, PRODUCTION.RATIO)

# conversion function 
grams2pounds <- function(grams) {
  pounds <- grams / 453.592  # 1 pound = 453.592 grams
  return(pounds)
}

# selecting all rows where the unit is grams
TRI2022$UNIT.OF.MEASURE <- as.character(TRI2022$UNIT.OF.MEASURE)
grams_rows <- TRI2022$UNIT.OF.MEASURE == "Grams"

# using grams2pounds functions to convert selected rows
TRI2022$FUGITIVE.AIR[grams_rows] <- grams2pounds(TRI2022$FUGITIVE.AIR[grams_rows])
TRI2022$STACK.AIR[grams_rows] <- grams2pounds(TRI2022$STACK.AIR[grams_rows])
TRI2022$WATER[grams_rows] <- grams2pounds(TRI2022$WATER[grams_rows])
TRI2022$LANDFILLS[grams_rows] <- grams2pounds(TRI2022$LANDFILLS[grams_rows])
TRI2022$UNDERGROUND[grams_rows] <- grams2pounds(TRI2022$UNDERGROUND[grams_rows])
TRI2022$ON.SITE.RELEASE.TOTAL[grams_rows] <- grams2pounds(TRI2022$ON.SITE.RELEASE.TOTAL[grams_rows])
TRI2022$OFF.SITE.RELEASE.TOTAL[grams_rows] <- grams2pounds(TRI2022$OFF.SITE.RELEASE.TOTAL[grams_rows])

# replacing 0 with NA

ColumnsReplace <- c("FUGITIVE.AIR", "STACK.AIR", "WATER", 
                    "LANDFILLS", "UNDERGROUND", "ON.SITE.RELEASE.TOTAL", 
                    "OFF.SITE.RELEASE.TOTAL")

TRI2022 <- TRI2022 %>%
  mutate(across(all_of(ColumnsReplace), ~ifelse(. == 0, NA, .)))


# State heatmap disaster
by_state22 <- read.csv("by_state22.csv")

map_data <- map_data("state")
merged_data <- merge(map_data, by_state22, by.x = "region", by.y = "State", all.x = TRUE)
merged_data <- merged_data[order(merged_data$order), ]

ui <- dashboardPage(
  
  dashboardHeader(title = "TRI"),
  
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Background and Disclaimer", tabName = "page1"),
      menuItem("Releases Near Me", tabName = "page2"),
      menuItem("Chemical Information", tabName = "page3"),
      menuItem("National Trends", tabName = "page4"),
      menuItem("Trends by State", tabName = "page5"),
      menuItem("Historical Trends", tabName = "page6")
    )
  ),
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName = "page1",
              h2("Background and Disclaimer"),
              p("The data in used in this app is sourced from the EPA's Toxic Release Inventory, which was created as a 
                result of the 1986 Emergency Planning and Community Right to Know Act"),
              p("With the excpetion of the Historical Trends tab, all other information is based on the 2022 report."),
              p("Due to the nature of this app, some data was omitted. This app should not be used to aid in any 
                serious personal and professional decision, and is simply a tool to create a more informed public."),
              p("If you'd like to know more about the Toxic Release Inventory and it's implications please click on the EPA logo."),
              tags$a(href = "https://www.epa.gov/toxics-release-inventory-tri-program/what-toxics-release-inventory",
                     tags$img(src = "EPA.png", height = 100, width = 200)
                     ),
              p("To see the code to create the dashboard, click the link below to github:")
              
              ),
      
      tabItem(tabName = "page2",
              h2("Releases Near Me"),
              sidebarLayout(  
                sidebarPanel(
                  textInput("zip_input", "Enter Zip Code:", ""),
                  actionButton("submit_btn", "Submit") # So far this is useless, the map changes as soon as a zip is entered
                ),
                mainPanel(
                  leafletOutput("mymap", height = "80vh")  
                )
              )),
      
      tabItem(tabName = "page3",
              h2("Wikipedia Page"),
              sidebarLayout(
                sidebarPanel(
                  selectInput("search2_term", 
                              "Select Chemical:",
                              choices = unique(TRI2022$CHEMICAL),
                              selected = "Lead")
                ),
                mainPanel(
                  uiOutput("page")
                )
              )),
      
      tabItem(tabName = "page4",
              h2("National Trends"),
              sidebarLayout(
                sidebarPanel(
                  radioButtons("variable4", "Select Release Method", 
                               choices = c("Fugitive Air",
                                           "Stack Air",
                                           "Water",
                                           "Landfills",
                                           "Underground",
                                           "On Site Total",
                                           "Off Site Total"),
                               selected = "Fugitive Air")
                ),
                mainPanel(
                  leafletOutput("map2", height = "80vh")
                )
              )),
      
      tabItem(tabName = "page5",
              h2("Trends by State"),
              sidebarLayout(
                sidebarPanel(
                  selectInput("search5_term", 
                              "Select State:",
                              choices = unique(TRI2022$ST),
                              selected = "SD")
                ),
                mainPanel(
                  
                )
              )),
      
      tabItem(tabName = "page6",
              h2("Historical Trends"),
              sidebarLayout(
                sidebarPanel(
                  radioButtons("dataset", "Select Range:",
                               choices = c("2 years",
                                           "5 years",
                                           "10 years",
                                           "15 years"),
                               selected = "2 years")
                ),
                mainPanel(
                  
                )
              ))
    )
  ),
  
  tags$head(
    tags$style(HTML("
      /* Custom CSS for neutral greens theme */
      body, .main-header, .main-sidebar, .left-side, .sidebar-menu {
        background-color: #d2e5d2 !important;
      }
      .main-sidebar {
        border-right: 1px solid #5e7f5e !important;
      }
      .main-header .navbar {
        background-color: #5e7f5e !important;
      }
      .main-header .logo {
        background-color: #5e7f5e !important;
        border-bottom: 1px solid #4b664b !important;
      }
      .main-header .navbar .sidebar-toggle:hover {
        background-color: #4b664b !important;
      }
      .sidebar-menu a:hover {
        background-color: #4b664b !important;
      }
      .content-header {
        background-color: #5e7f5e !important;
      }
      .content-wrapper {
        background-color: #f0f5f0 !important;
      }
      .box {
        border: 1px solid #5e7f5e !important;
      }
    "))
  )
)

server <- function(input, output) {
  
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
  
  output$page <- renderUI({
    page <- paste0("https://en.wikipedia.org/wiki/", gsub(" ", "_", input$search2_term))
    tags$iframe(src = page, height = 800, width = 900)
  })

  selected_variable <- reactive({
    switch(input$variable4,
           "Fugitive Air" = merged_data$FugitiveAir,
           "Stack Air" = merged_data$StackAir,
           "Water" = merged_data$Water,
           "Landfills" = merged_data$Landfills,
           "Underground" = merged_data$Underground,
           "On Site Total" = merged_data$OnSite,
           "Off Site Total" = merged_data$OffSite
           )
  })
  
  
  output$map2 <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -95.7129, lat = 37.0902, zoom = 4)  # Centered on the United States
  })
  
  # Add heatmap layer to the map
  observe({
    leafletProxy("map2") %>%
      clearHeatmap() %>%
      addHeatmap(
        data = merged_data,
        lng = ~long,
        lat = ~lat,
        intensity = ~selected_variable(),
        blur = 30,
        max = max(selected_variable())
      )
  })
  
  
  
  
}

shinyApp(ui, server)

