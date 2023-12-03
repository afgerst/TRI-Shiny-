# For creating the interactive app
library(shiny)
library(shinydashboard)
library(shinyjs)

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



ui <- dashboardPage(
  dashboardHeader(title = "TRI"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Releases Near Me", tabName = "page1"),
      menuItem("Chemical Information", tabName = "page2"),
      menuItem("Wikipedia Link Generator test", tabName = "page3")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "page1",
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
      tabItem(tabName = "page2",
              h2("Chemical Information"),
              sidebarLayout(
                sidebarPanel(
                  selectInput("search_term", 
                              "Select Chemical:",
                              choices = unique(TRI2022$CHEMICAL),
                              selected = "Lead"),
                  actionButton("search_button", "Generate Link")
                ),
                mainPanel(
                  textOutput("wiki_link")
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
  observeEvent(input$search_button, {
    selected_term <- gsub(" ", "_", input$search_term)  # Replace spaces with underscores
    wiki_link <- paste0("https://en.wikipedia.org/wiki/", selected_term)
    output$wiki_link <- renderText({
      paste("Wikipedia Link:", tags$a(href = wiki_link, target = "_blank", wiki_link))
    })
  })
  output$page <- renderUI({
    page <- paste0("https://en.wikipedia.org/wiki/", gsub(" ", "_", input$search2_term))
    tags$iframe(src = page, height = 800, width = 900)
  })
}

shinyApp(ui, server)

