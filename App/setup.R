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
datafolder <- file.path(projectfolder, "Data")

filenames <- list.files(datafolder)
filenames <- filenames[str_detect(filenames, "CTD")]
filenames <- str_remove(filenames, ".csv")

filepaths <- list.files(datafolder, full.names = TRUE)
filepaths <- filepaths[str_detect(filepaths, "CTD")]

for (i in 1:length(filepaths)) {
  temp <- read_csv(filepaths[i])
  assign(filenames[i], temp)
}