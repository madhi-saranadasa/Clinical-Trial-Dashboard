source("setup.R")
source("ui_snippets.R")

ui <- fluidPage(

  #theme = bslib::bs_theme(bootswatch = "slate"),
  
  
  navbarPage("NCFB Clinical Trials Explorer",
             
             tabPanel("Explore!",
                      fluidRow(
      
                        # Explore - Left Side -----------------------------------------------------
                        column(width = 5, 
                               offset = 1,
                               wellPanel(
                                 
                                 tags$h4(tags$b("Filter by study details:"), align = "center"),
                                 
                                 br(),
                                 
                                 tags$h5(tags$b("Select study start year:"), align = "left"),
                                 
                                 plotOutput(outputId = "yearPlot", click = "yearPlot_click", height="10vh"),
                                 
                                 #sliderInputOverYears,
                                 
                                 wellPanel(checkboxGroup, style = "background: #e2e2e2"),
                                 
                                 tags$h5(tags$b("Select qualifying studies by geography:"), align = "left"),
                                 
                                 leafletOutput(outputId = "mymap")
                                 
                               )
                        ),
                        
                        
                        
                        # Explore - Right Side ----------------------------------------------------
                        column(width = 5,
                               
                               tags$h4(tags$b("Details for the selected study"), align = "center"),
                               br(),
                               
                               htmlOutput(outputId = "titleblock"),
                               
                               tabsetPanel(
                                 
                                 tabPanel(title = "Summary",
                                          br(),
                                          leafletOutput(outputId = "selectmap"),
                                          br(),
                                          htmlOutput(outputId = "detailblock"),
                                          br(),
                                          tags$h4("Interventions:"),
                                          tableOutput(outputId = "interventions")),
                                 
                                 tabPanel(title = "Study Criteria",
                                          br(),
                                          verbatimTextOutput(outputId = "ie")),
                                 
                                 tabPanel(title = "Endpoints",
                                          br(),
                                          tags$h4(tags$b("Primary Endpoint:")),
                                          tableOutput(outputId = "outcomes1"),
                                          tags$h4(tags$b("Secondary Endpoints:")),
                                          tableOutput(outputId = "outcomes2"),)
                               )
                        )
                        
                      )),
             
             tabPanel("About this dashboard",
                      
                      # About UI --------------------------------------------------------------
                      fluidRow(column(width = 5, offset = 1, imageOutput("aboutImage")))
             )
  )
)
  

server <- function(input, output, session) {
  
  # Setup reactive values  --------------------------------------------------
  rv <- reactiveValues(selectedYear = 2004)
  
  # Get Slider Studies ------------------------------------------------------
  GetSliderStudies <- reactive({
    study_list <- CTD_DetailsClean %>%
      filter(year(start_date) == rv$selectedYear) %>% 
      distinct(id) %>%
      pull(id)
    
    study_list
  })
  
  
  # Get Checkbox studies ----------------------------------------------------
  GetCheckboxStudies <- reactive({
    
    allstudies <- CTD_DetailsClean %>% distinct(id) %>% pull(id)
    
    # sponsers
    if(length(input$check_sponser) == 0) {
      list_sponser <- allstudies
    } else {
      list_sponser <- CTD_FlagSponser %>% 
        filter(keyword %in% input$check_sponser) %>%
        distinct(id) %>%
        pull(id)
    }
    
    # criteria
    if(length(input$check_criteria) == 0) {
      list_criteria <- allstudies
    } else {

      list_criteria <- CTD_FlagInclusion %>%
        filter(keyword %in% input$check_criteria) %>%
        group_by(id) %>%
        summarise(n = n()) %>%
        filter(n == length(input$check_criteria)) %>%
        pull(id)
    }
    
    # outcomes
    if(length(input$check_outcomes) == 0) {
      list_outcomes <- allstudies
    } else {
      
      list_outcomes <- CTD_FlagOutcome %>%
        filter(keyword %in% input$check_outcomes) %>%
        group_by(id) %>%
        summarise(n = n()) %>%
        filter(n == length(input$check_outcomes)) %>%
        pull(id)
    }
    
    # medications
    if(length(input$check_meds) == 0) {
      list_meds <- allstudies
    } else {
      
      list_meds <- CTD_FlagMed %>%
        filter(keyword %in% input$check_meds) %>%
        group_by(id) %>%
        summarise(n = n()) %>%
        filter(n == length(input$check_meds)) %>%
        pull(id)
    }
    
    out <- intersect(list_sponser, list_criteria)
    out <- intersect(out, list_outcomes)
    out <- intersect(out, list_meds)
    
  })
  
  
  # Render Year plot --------------------------------------------------------
  output$yearPlot <- renderPlot({
    
    subset <- GetCheckboxStudies()
    
    background <- CTD_DetailsClean %>%
      mutate(year = year(start_date)) %>%
      group_by(year) %>%
      summarise(n = n()) %>%
      filter(!is.na(year))
    
    test <- CTD_DetailsClean %>%
      filter(id %in% subset) %>%
      mutate(year = year(start_date)) %>%
      group_by(year) %>%
      summarise(n = n()) %>%
      filter(!is.na(year))
    
    ggplot() +
      geom_bar(data = background,
               aes(x = year,
                   y = n),
               stat = "identity",
               width = 0.95,
               alpha = 0.15) +
      geom_bar(data = test,
               aes(x = year,
                   y = n),
               stat = "identity",
               width = 0.95) +
      geom_vline(aes(xintercept = rv$selectedYear),
                 size = 8,
                 color = "#ff7536",
                 alpha = 0.25) +
      scale_y_continuous(limits = c(0, 10),
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
  

  # Process plot click  ------------------------------------------------------
  UpdateSlider <- observe({
    click_location = input$yearPlot_click$x
    
    if(is.null(click_location))
      return()
    
    #updateSliderInput(session, "YearSlider", value = round(click_location))
    rv$selectedYear <- round(click_location)
    print(rv$selectedYear)
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
      pull(id)
  })
  
  
  # Render title block ------------------------------------------------------
  output$titleblock <- renderUI({
    study <- GetSelectedStudies()
    
    if(is.null(study))
      return()
    
    details <- CTD_DetailsClean %>% filter(id %in% study)
    
    tags$body(
      tags$h4(details$title, align = "center"),
      tags$h5(details$phase, align = "center"),
      tags$h5("Sponsor:", details$agency, align = "center")
    )
  })
  

  # Render interventions table ----------------------------------------------
  output$interventions <- renderTable({
    study <- GetSelectedStudies()
    
    if(is.null(study))
      return()
    
    interventions <- CTD_Intervention %>% 
      filter(id %in% study) %>%
      select(intervention_name, description)
    
    interventions
    
  }, colnames = FALSE)
  
  

  # Render outcomes tables ---------------------------------------------------
  output$outcomes1 <- renderTable({
    study <- GetSelectedStudies()
    
    if(is.null(study))
      return()
    
    outcomes1 <- CTD_Outcome1 %>%
      filter(id %in% study) %>%
      select(measure, time_frame)
    
    outcomes1
    
  }, colnames = FALSE, width = "100%")
  
  
  output$outcomes2 <- renderTable({
    study <- GetSelectedStudies()
    
    if(is.null(study))
      return()
    
    outcomes2 <- CTD_Outcome2 %>%
      filter(id %in% study) %>%
      select(measure, time_frame)
    
    outcomes2
    
  }, colnames = FALSE, width = "100%")
  
  
  # Render details block ------------------------------------------------------
  output$detailblock <- renderUI({
    study <- GetSelectedStudies()
    
    if(is.null(study))
      return()
    
    details <- CTD_DetailsClean %>% filter(id %in% study)

    tags$body(
      tags$p(details$brief_summary)
    )
  })
  
  
  # Render base map ---------------------------------------------------------
  output$mymap <- renderLeaflet({
    
    basemap <- leaflet() %>%
      addTiles() %>%
      setView(-96, 37.8, 4) 
    
    basemap
    
  })
  
  
  # Render select map -------------------------------------------------------
  output$selectmap <- renderLeaflet({
    study <- GetSelectedStudies()
      
    if(is.null(study))
      return()
    
    locations <- CTD_LocationAdded %>%
      filter(id %in% study) %>%
      mutate(label = paste0(id))
    
    bounds <- locations %>%
      summarise(n = n(),
                meanlat = mean(lat, na.rm = TRUE),
                meanlon = mean(lon, na.rm = TRUE)) 
    
    icons2 <- awesomeIcons(
      icon = 'home',
      iconColor = 'white',
      library = 'glyphicon',
      markerColor = "blue"
    )
    
    
    basemap <- leaflet(data = locations,
                       options = leafletOptions(maxZoom = 4)) %>%
      addTiles() %>%
      addAwesomeMarkers(lng = ~lon,
                        lat = ~lat,
                        icon = icons2,
                        popup = ~htmlEscape(label),
                        layerId = ~markerID,
                        labelOptions = labelOptions(noHide = F, direction = 'auto'),
                        options = markerOptions(riseOnHover = TRUE))
    
  })

  # Render about info -------------------------------------------------------
  
  output$aboutPlot <- renderPlot({
    
    ggplot() +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text = element_text(size = 12),
            axis.ticks = element_blank(),
            plot.background = element_rect(fill = "#f5f5f5", color = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
  })
  
  output$aboutImage <- renderImage({
    
    width  <- session$clientData$output_plot2_width
    height <- session$clientData$output_plot2_height
    
    
    list(src = "workflow.png",
         width = width,
         height = height)
  }, deleteFile = FALSE)
  
  # DEBUG -------------------------------------------------------------------
  output$ie <- renderText({
    
    study <- GetSelectedStudies()
  
    CTD_Eligibility_criteria %>% filter(nct_id == study) %>% pull(eligibility_criteria)
    
  })
  
}


shinyApp(ui = ui, server = server)
