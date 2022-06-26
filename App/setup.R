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
projectfolder <- dirname(getwd())

datapath <- file.path(projectfolder, "Data", "CTD_DetailsClean.csv")
CTD_DetailsClean <- read_csv(datapath)

datapath <- file.path(projectfolder, "Data", "CTD_LocationAdded.csv")
CTD_LocationAdded <- read_csv(datapath) 


