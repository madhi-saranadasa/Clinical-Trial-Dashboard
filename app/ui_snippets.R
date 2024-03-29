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
topicList <- list("Antibiotics", "CT", "Exacerbation", "PA", "Sputum", "Spirometry")
outcomeList <- list("Exacerbation", "QOL", "Spirometry")
medList <- list("Azithromycin", "Ciprofloxacin")
companyList <- list("AstraZeneca", "Aradigm", "Bayer", "Gilead", "Insmed", "Novartis")
  
checkboxGroup <-
  fluidRow(
    column(width = 3, checkboxGroupInput(inputId = "check_criteria", "Select I/E criteria:",
                                         choiceNames = topicList,
                                         choiceValues = topicList)
           ),
    
    column(width = 3, checkboxGroupInput(inputId = "check_outcomes", "Select endpoints:",
                                         choiceNames = outcomeList,
                                         choiceValues = outcomeList)
    ),
    
    column(width = 3, checkboxGroupInput(inputId = "check_meds", "Select meds:",
                                         choiceNames = medList,
                                         choiceValues = medList)
           ),
    
    column(width = 3, checkboxGroupInput(inputId = "check_sponser", "Select sponsers:",
                                         choiceNames = companyList,
                                         choiceValues = companyList)
           ),
  )