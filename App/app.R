library(shiny)
library(leaflet)
library(tidyverse)
library(lubridate)
library(htmltools)
library(RColorBrewer)

markercolors <- c('red', 'darkred', 'orange', 'green', 'darkgreen', 'blue', 'purple', 'darkpurple', 'cadetblue')

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
    
    wellPanel(
      
      fluidRow(
        p("Scrub through years and select trial of interest:"
        )
      ),
      
      fluidRow(
        column(12, 
               sliderInput(inputId = "year", label = "Year:",
                           min = 2000, max = 2022, step = 1, value = 2000, width = "100vh", sep = "", animate = TRUE)
               )
      ),
      fluidRow(
        column(12,
               leafletOutput(outputId = "mymap", height="70vh", width = "90vh")
               )
      )
    ),
    
    mainPanel(
      fluidRow(p(textOutput(outputId = "rightside")))
    )
  )

)
  

server <- function(input, output) {
  
  # ----- observe for map updating -----
  observe({
    
    study_list <- CTD_DetailsClean %>%
      filter(year(start_date) == input$year) %>%
      distinct(id)
    
    if(nrow(study_list) == 0) {
      
      locations <- CTD_LocationAdded %>%
        inner_join(study_list, by = "id") %>%
        mutate(label = paste0(id))
    } else {
      
      idmapping <- tibble(id = study_list$id,
                          color = rep_len(markercolors, length(study_list$id)))
      
      locations <- CTD_LocationAdded %>%
        inner_join(study_list, by = "id") %>%
        inner_join(idmapping, by = "id") %>%
        mutate(label = paste0(id))
      
    }
    
    icons2 <- awesomeIcons(
      icon = 'home',
      iconColor = 'white',
      library = 'glyphicon',
      markerColor = locations$color
    )
    
    leafletProxy("mymap", data = locations) %>%
      clearShapes() %>%
      clearMarkers() %>%
      addAwesomeMarkers(lng = ~lon,
                        lat = ~lat,
                        icon = icons2,
                        popup = ~htmlEscape(label),
                        layerId = ~markerID,
                        labelOptions = labelOptions(noHide = F, direction = 'auto'),
                        options = markerOptions(riseOnHover = TRUE))
  })
  
  
  # ----- reactive to fetch study details on click -----
  onClickGetStudy <- reactive({
    click<-input$mymap_marker_click
    
    if(is.null(click))
      return()
    
    study <- CTD_LocationAdded %>%
      filter(markerID == click$id) %>%
      distinct(id) %>%
      inner_join(CTD_DetailsClean, by = "id")
  })
  
  
  # ----- render right side -----
  output$rightside <- renderText({
    
    study <- onClickGetStudy()
    text<-study$title
    
  })
  
  
  # ----- render the base map -----
  output$mymap <- renderLeaflet({
    
    basemap <- leaflet() %>%
      addTiles() %>%
      setView(-96, 37.8, 4) 
    
    basemap
    
  })
  
}


shinyApp(ui = ui, server = server)
