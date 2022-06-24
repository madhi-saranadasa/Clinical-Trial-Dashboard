library(shiny)
library(leaflet)
library(tidyverse)
library(lubridate)
library(htmltools)
library(RColorBrewer)
library(thematic)

markercolors <- c('red', 'darkred', 'orange', 'green', 'darkgreen', 'blue', 'purple', 'darkpurple', 'cadetblue')

projectfolder <- dirname(getwd())

datapath <- file.path(projectfolder, "Data", "CTD_DetailsClean.csv")
CTD_DetailsClean <- read_csv(datapath) %>%
  filter()

datapath <- file.path(projectfolder, "Data", "CTD_LocationAdded.csv")
CTD_LocationAdded <- read_csv(datapath) 

thematic_shiny(font = "auto")

ui <- fluidPage(

  theme = bslib::bs_theme(bootswatch = "slate"),
  
  
  navbarPage("NCFB Clinical Trials Dashboard",
             tabPanel("Explore by geography"),
             tabPanel("Explore by endpoint"),
             tabPanel("About this dashboard"),
  ),
    
  fluidRow(
    
    column(width = 6,
           wellPanel(
             
             tags$h4("Number of studies started in each year", align = "center"),
             
             plotOutput(outputId = "yearPlot", 
                        click = "yearPlot_click",
                        height="10vh"),
             
             br(),
             br(),
             
             
             
             fluidRow(column(width = 10, offset = 1, sliderInput(inputId = "year", 
                                                     label = "",
                                                     min = 2004, 
                                                     max = 2021, 
                                                     step = 1, 
                                                     value = 2004,
                                                     sep = "", 
                                                     animate = TRUE,
                                                     width = "80vh")), align = "center"),
             
             leafletOutput(outputId = "mymap")
             
             )
           ),
    
    column(width = 6, 
           
           tags$h4("Details for the selected study", align = "center"),
           htmlOutput(outputId = "titleblock")
           )
    
  )
)
  

server <- function(input, output, session) {
  
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
  
  
  # ----- reactive to subset study year on click -----
  onYearPlotClick <- observe({
    test = input$yearPlot_click$x
    
    if(is.null(test))
      return()
    
    updateSliderInput(session, "year", value = round(test))
    
    round(test)
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
  

  # ----- render yearly plot -----
  output$yearPlot <- renderPlot({
    
    test <- CTD_DetailsClean %>%
      mutate(year = year(start_date)) %>%
      group_by(year) %>%
      summarise(n = n())
    
    ggplot() +
      geom_line(data = test,
                aes(x = year,
                    y = n),
                size = 0.75) +
      scale_y_continuous(limits = c(0, NA),
                         breaks = c(0, 10)) +
      scale_x_continuous(limits = c(2004, 2021),
                         breaks = seq(2004, 2021, 1)) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text = element_text(size = 12),
            axis.ticks = element_blank())
  })
  
  
  # ----- render title -----
  output$title <- renderText({
    
    study <- onClickGetStudy()
    text<-study$title
    
  })
  
  
  # ----- render title -----
  output$titleblock <- renderUI({
    study <- onClickGetStudy()
    
    if(is.null(study))
      return()
    
    tags$body(
      tags$h3(study$title, align = "center"),
      tags$h4(study$phase, align = "center"),
      tags$h4("Sponsor:", study$agency, align = "center"),
      tags$p(study$brief_summary)
    )
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
