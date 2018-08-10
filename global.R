library(shiny)
library(tidyverse)
library(plotly)
library(leaflet)
library(shinythemes)

source("modules/home.R")

wq_data <- read_rds('data/test-data.rds')
glimpse(wq_data)
wq_data$analyte_name %>% unique
wq_data$analyte_property %>% unique
