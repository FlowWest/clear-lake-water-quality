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

library(adobeCreekData)

source("modules/home.R")
source("modules/clear_lake.R")

bvr_analytes <- distinct(bvr_wq, characteristic_name) %>% pull()

