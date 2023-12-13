# For creating the interactive app
library(shiny)
library(shinydashboard)
library(shinyjs)
library(png)

# For the world maps
library(maps) 
library(ggplot2)
library(leaflet)
library(leaflet.extras)

# Plots
library(plotly)

# For filtering data
library(dplyr)

# Reading in data file - Raw data from EPA website
TRI2022 <- read.csv("2022_us.csv")

# Data from 2007, 2012, 2017, 2020, 2022 cleaned in Excel to make life easier
# Only contains the year, release method, mean, and standard error data
TRIHist <- read.csv("historical.csv")

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
by_state22 <- read.csv("by_state22.csv") # Some data cleaning in excel

# lat long data from maps library
map_data <- map_data("state")

# merge and re-organize the two datasets
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
              p("If you'd like to know more about the Toxic Release Inventory and it's implications please click on the EPA logo below."),
              tags$a(
                href = "https://www.epa.gov/toxics-release-inventory-tri-program/what-toxics-release-inventory",
                tags$img(src = "https://seeklogo.com/images/E/Environmental_Protection_Agency-logo-6E0F9CEE62-seeklogo.com.png")
              ),
              p("To see the code to create the dashboard, click the link below to github:"),
              tags$a(href = "https://github.com/afgerst/TRI-Shiny-",
                     tags$img(src = "https://1000logos.net/wp-content/uploads/2021/05/GitHub-logo-500x281.png")
              )
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
              h2("Choose a chemical to learn more about"),
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
                                           "Off Site Total"))
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
                  plotlyOutput("pieChart"),
                  plotlyOutput("IndustryBar"),
                  plotOutput("TotalReleases")
                )
              )),
      
      tabItem(tabName = "page6",
              h2("Historical Trends"),
              sidebarLayout(
                sidebarPanel(
                  selectInput("year_input",
                              "Select Year(s):",
                              choices = unique(TRIHist$year),
                              multiple = TRUE,
                              selected = "2022")
                ),
                mainPanel(
                  plotOutput("SEM")
                )
              ))
    )
  ),
  
  tags$head(
    # CSS code for theme from Chat gpt 3.5
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
                                  "Chemical:", CHEMICAL, "<br>",
                                  "Carcinogen:", CARCINOGEN))
      
    }
  })
  
  # Creates a URL out of the selected chemcial, and opens the wiki page within the app
  output$page <- renderUI({
    page <- paste0("https://en.wikipedia.org/wiki/", gsub(" ", "_", input$search2_term))
    tags$iframe(src = page, height = 800, width = 900)
  })
  
  # Default view of the United States when nothing is selected
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
        # Since the data set is switching between columns and not within a column, 
        # need to define what the input is referring to.
        intensity = switch(input$variable4,
                           "Fugitive Air" = merged_data$FugitiveAir,
                           "Stack Air" = merged_data$StackAir,
                           "Water" = merged_data$Water,
                           "Landfills" = merged_data$Landfills,
                           "Underground" = merged_data$Underground,
                           "On Site Total" = merged_data$OnSite,
                           "Off Site Total" = merged_data$OffSite
        ),
        blur = 30
      )
  })
  
  
  # create a filtered data set depending on the state selected
  filteredData <- reactive({
    TRI2022[TRI2022$ST == input$search5_term, ]
  })
  
  # pie chart of carcinogens
  output$pieChart <- renderPlotly({
    filteredData <- filteredData()
    counts <- count(filteredData, CARCINOGEN)
    pie_chart <- plot_ly(counts, labels = ~CARCINOGEN, values = ~n, type = 'pie')
    pie_chart <- pie_chart %>% layout(title = paste("Carcinogen Distribution in", input$search5_term))
    pie_chart
  })
  
  # bar chart of industries
  output$IndustryBar <- renderPlotly({
    filteredData <- filteredData()
    industry_counts <- table(filteredData$INDUSTRY.SECTOR)
    bar_chart <- plot_ly(x = names(industry_counts), y = industry_counts, type = "bar",
                         marker = list(color = "darkgreen")) %>%
                            layout(title = "Occurences of Industries",
                            xaxis = list(title = "Industries"),
                             yaxis = list(title = "Frequency"))
    bar_chart
  })
  
 output$TotalReleases <- renderPlot({
   bar_data <- filteredData() %>%
     select("FUGITIVE.AIR", "STACK.AIR", "WATER", "LANDFILLS", "UNDERGROUND", "ON.SITE.RELEASE.TOTAL", "OFF.SITE.RELEASE.TOTAL") %>%
     summarise_all(~ sum(ifelse(is.na(.), 0, .)))
   
   bar_names <- colnames(bar_data)
   bar_values <- as.numeric(bar_data)
   
   barplot(bar_values, names.arg = bar_names, col = "skyblue",
           main = paste("Total Releases for", input$search5_term),
           xlab = "Release Method", ylab = "Total (lbs)", las = 2)
 })
  
  
  # Mean SEM chart of release types over time
  output$SEM <- renderPlot({
    g <- TRIHist %>% 
      filter(year %in% input$year_input)
    pd <- position_dodge(0.2)
    g1 <- ggplot(g, aes(x = year, y = mean, group = release, color = release)) + 
           geom_point(position = pd, size = 3) + 
           geom_line(position = pd, linewidth = 1) +
           geom_errorbar(aes(ymin = mean - standarderror,
                              ymax = mean + standarderror),
                          width = 0.1,
                          position = pd) +
          labs(y = "Average Release (lbs)", x = "Year")
    g1
  }) 
  
}

shinyApp(ui, server)

