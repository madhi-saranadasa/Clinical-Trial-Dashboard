# ----------------------------------------------------------------------------
# Libraries
# ----------------------------------------------------------------------------
library(shiny)
library(leaflet)
library(tidyverse)
library(lubridate)
library(htmltools)
library(RColorBrewer) 
library(thematic)
library(shinythemes)

# ----------------------------------------------------------------------------
# Parameters
# ----------------------------------------------------------------------------
markercolors <- c('red', 'darkred', 'orange', 'green', 'darkgreen', 'blue', 'purple', 'darkpurple', 'cadetblue')
#thematic_shiny(font = "auto")


# ----------------------------------------------------------------------------
# Load Data
# ----------------------------------------------------------------------------
tables <- c("CTD_DetailsClean",
            "CTD_FlagSponser",
            "CTD_FlagInclusion",
            "CTD_FlagOutcome",
            "CTD_FlagMed",
            "CTD_LocationAdded",
            "CTD_Eligibility_criteria",
            "CTD_Outcome1",
            "CTD_Outcome2",
            "CTD_Intervention")

files <- paste0(tables, ".csv")

for (i in 1:length(files)) {
  temp <- read_csv(files[i])
  assign(tables[i], temp)
}
