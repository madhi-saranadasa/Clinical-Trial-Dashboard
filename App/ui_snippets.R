# ----------------------------------------------------------------------------
# Slider input for year selection
# ----------------------------------------------------------------------------
sliderInputOverYears <- 
  fluidRow(
    column(width = 10, 
           offset = 1,
           align = "center",
           sliderInput(inputId = "YearSlider", 
                       label = "",
                       min = 2004, 
                       max = 2021, 
                       step = 1, 
                       value = 2004,
                       sep = "", 
                       animate = TRUE,
                       width = "80vh")
           )
    )


# ----------------------------------------------------------------------------
# Check box input for topic selection
# ----------------------------------------------------------------------------
topicList <- list("Exacerbation", "Sputum", "PROs", "Spirometry endpoint", "Bacterial endpoint")
medList <- list("Ciprofloxacin", "Tobramycin", "Azithromycin", "Amikacin", "Colistimethate")
companyList <- list("Bayer", "Insmed", "AstraZeneca", "Boehringer Ingelheim", "Novartis", "Gilead")
  
checkboxGroup <-
  fluidRow(
    column(width = 3, checkboxGroupInput(inputId = "check_topics", "Filter criteria:",
                                         choiceNames = topicList,
                                         choiceValues = topicList)
           ),
    
    column(width = 3, checkboxGroupInput(inputId = "check_topics", "Filter outcomes:",
                                         choiceNames = topicList,
                                         choiceValues = topicList)
    ),
    
    column(width = 3, checkboxGroupInput(inputId = "check_meds", "Filter meds:",
                                         choiceNames = medList,
                                         choiceValues = medList)
           ),
    
    column(width = 3, checkboxGroupInput(inputId = "check_sponser", "Filter sponsers:",
                                         choiceNames = companyList,
                                         choiceValues = companyList)
           ),
  )