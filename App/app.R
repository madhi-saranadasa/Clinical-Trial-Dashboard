library(shiny)
library(leaflet)
library(tidyverse)

projectfolder <- dirname(getwd())

datapath <- file.path(projectfolder, "Data", "CTD_DetailsClean.csv")
CTD_DetailsClean <- read_csv(datapath)

datapath <- file.path(projectfolder, "Data", "CTD_LocationAdded.csv")
CTD_LocationAdded <- read_csv(datapath)

ui <- fluidPage(

  theme = bslib::bs_theme(bootswatch = "slate"),
  
  navbarPage("NCFB Clinical Trials Dashboard",
             tabPanel(icon("home")),
             tabPanel("Endpoint Explorer"),
             tabPanel("About this dashboard"),
  ),
    
  splitLayout(
    
    mainPanel(
      sliderInput(inputId = "year", label = "Year:",
                  min = 2000, max = 2022, step = 1, value = 2000, width = "100vh", sep = ""),
      
      leafletOutput(outputId = "mymap",
                    height="70vh", width = "90vh")
    ),
    
    mainPanel(
      tableOutput(outputId = "stats")
    )
  
  )

)
  



server <- function(input, output) {
  
  output$mymap <- renderLeaflet({
    
    study_list <- CTD_DetailsClean %>%
      filter(year(start_date) == input$year) %>%
      distinct(id, enrollment) 
    
    locations <- CTD_LocationAdded %>%
      inner_join(study_list, by = "id")
    
    basemap <- leaflet(data = locations) %>%
      addTiles() %>%
      setView(-96, 37.8, 4) %>%
      addCircles(lng = ~lon,
                 lat = ~lat,
                 weight = 1,
                 radius = ~enrollment * 100)
    
    basemap
    
  })
  
  output$stats <- renderTable({
    
    study_list <- CTD_DetailsClean %>%
      filter(year(start_date) == input$year) %>%
      distinct(id) %>%
      pull(id)
    
    locations <- CTD_LocationAdded %>%
      filter(id %in% study_list)
  
  })
  
}


shinyApp(ui = ui, server = server)
