library(shiny)
library(tidyverse)
library(plotly)
library(leaflet)
library(shinythemes)

source("modules/home.R")

ceden_wq <- read_rds('data/ceden_data.rds')
ceden_stations <- read_rds('data/ceden_stations.rds')

analyte_choices <- ceden_wq %>% distinct(analyte_name) %>% pull()
