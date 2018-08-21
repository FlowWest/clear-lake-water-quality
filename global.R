library(shiny)
library(tidyverse)
library(plotly)
library(leaflet)
library(shinythemes)
library(dygraphs)
library(xts)

source("modules/home.R")

ceden_wq <- read_rds('data/ceden_analyte_data.rds')
ceden_stations <- read_rds('data/ceden_stations.rds')
clear_lake_wse <- read_rds("data/clear-lake-wse.rds")

wq_data <- bind_rows(
  ceden_wq
)

wq_stations <- bind_rows(
  ceden_stations
)

