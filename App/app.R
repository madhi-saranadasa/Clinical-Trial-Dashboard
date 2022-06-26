source("setup.R")
source("ui_snippets.R")


# UI ----------------------------------------------------------------------

ui <- fluidPage(

  #theme = bslib::bs_theme(bootswatch = "slate"),
  
  
  navbarPage("NCFB Clinical Trials Dashboard",
             tabPanel("Trials"),
             tabPanel("Endpoints"),
             tabPanel("About this dashboard"),
  ),
    
  fluidRow(
    
    column(width = 5, 
           offset = 1,
           wellPanel(
             
             tags$h4("Number of studies started in each year", align = "center"),
             
             plotOutput(outputId = "yearPlot", click = "yearPlot_click", height="10vh"),
             
             br(),
             
             sliderInputOverYears,
             
             leafletOutput(outputId = "mymap"),
             
             br(),
             
             wellPanel(checkboxGroup, style = "background: #e2e2e2")
             
             )
           ),
    
    column(width = 5,
           
           tags$h4("Details for the selected study", align = "center"),
           
           htmlOutput(outputId = "titleblock"),
           
           verbatimTextOutput(outputId = "debug")
           
           )
    
  )
)
  

server <- function(input, output, session) {

  # Get Slider Studies ------------------------------------------------------
  GetSliderStudies <- reactive({
    study_list <- CTD_DetailsClean %>%
      filter(year(start_date) == input$YearSlider) %>% 
      distinct(id) %>%
      pull(id)
    
    study_list
  })
  
  
  # Get Checkbox studies ----------------------------------------------------
  GetCheckboxStudies <- reactive({
    
    allstudies <- CTD_DetailsClean %>% distinct(id) %>% pull(id)
    
    if(length(input$check_sponser) == 0) {
      list_sponser <- allstudies
    } else {
      list_sponser <- CTD_DetailsClean %>% 
        filter(agency %in% input$check_sponser) %>%
        distinct(id) %>%
        pull(id)
    }
    
    list_sponser
    
  })
  
  
  # Render Year plot --------------------------------------------------------
  output$yearPlot <- renderPlot({
    
    subset <- GetCheckboxStudies()
    
    test <- CTD_DetailsClean %>%
      filter(id %in% subset) %>%
      mutate(year = year(start_date)) %>%
      group_by(year) %>%
      summarise(n = n()) %>%
      filter(!is.na(year))
    
    ggplot() +
      geom_bar(data = test,
               aes(x = year,
                   y = n),
               stat = "identity") +
      scale_y_continuous(limits = c(0, NA),
                         breaks = c(0, 5, 10)) +
      scale_x_continuous(limits = c(2003.5, 2021.5),
                         breaks = seq(2004, 2021, 1)) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text = element_text(size = 12),
            axis.ticks = element_blank(),
            plot.background = element_rect(fill = "#f5f5f5", color = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
  })
  

  # Update year slider ------------------------------------------------------
  UpdateSlider <- observe({
    click_location = input$yearPlot_click$x
    
    if(is.null(click_location))
      return()
    
    updateSliderInput(session, "YearSlider", value = round(click_location))
  })

  
  # Update map markers ------------------------------------------------------
  observe({

    finalStudies <- intersect(GetSliderStudies(), GetCheckboxStudies())
    
    colormap <- tibble(id = finalStudies,
                       color = rep_len(markercolors, length(finalStudies)))
    
    locations <- CTD_LocationAdded %>%
      filter(id %in% finalStudies) %>%
      inner_join(colormap, by = "id") %>%
      mutate(label = paste0(id))
    
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
  

  # Get selected studies ----------------------------------------------------
  GetSelectedStudies <- reactive({
    click<-input$mymap_marker_click
    
    if(is.null(click))
      return()
    
    study <- CTD_LocationAdded %>%
      filter(markerID == click$id) %>%
      distinct(id) %>%
      inner_join(CTD_DetailsClean, by = "id")
  })
  
  
  # Render title block ------------------------------------------------------
  output$titleblock <- renderUI({
    study <- GetSelectedStudies()
    
    if(is.null(study))
      return()
    
    tags$body(
      tags$h3(study$title, align = "center"),
      tags$h4(study$phase, align = "center"),
      tags$h4("Sponsor:", study$agency, align = "center"),
      tags$p(study$brief_summary)
    )
  })
  
  
  # Render base map ---------------------------------------------------------
  output$mymap <- renderLeaflet({
    
    basemap <- leaflet() %>%
      addTiles() %>%
      setView(-96, 37.8, 4) 
    
    basemap
    
  })

  
  # DEBUG -------------------------------------------------------------------
  output$debug <- renderPrint({
    
    length(GetCheckboxStudies())

  })
  
}


shinyApp(ui = ui, server = server)
