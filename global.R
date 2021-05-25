library(readr)
library(shiny)
library(tidyverse)
library(plotly)
library(leaflet)
library(shinythemes)
library(dygraphs)
library(xts)
library(shinyWidgets)
library(lubridate)
library(formattable)

library(adobeCreekData)

source("modules/fishkill.R")
source("modules/clear_lake.R")
source("modules/water_quality.R")

bvr_analytes <- distinct(bvr_water_quality, analyte) %>% pull()

analyte_descriptions <- read_tsv("data/analyte-descriptions.csv")
fish_kill_data <- read_rds("data/fish_kill_data.rds")
summary_table <-read_rds("data/summary_table.rds")
