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

source("modules/fishkill.R")
source("modules/clear_lake.R")
source("modules/water_quality.R")

# Water Quality -----------------------------------------------------------------

# look for the data we wrote from the other repo

# read the stations datasets 
clear_lake_stations <- read_rds("data/clear-lake-stations.rds")

# read the observations datasets
clear_lake_wq <- read_rds("data/clear-lake-water-quality.rds")
clear_lake_wq_choices <- sort(pull(distinct(clear_lake_wq, analyte_name)))
analyte_descriptions <- read_tsv("data/analyte-descriptions.csv")

# Fish Kill ---------------------------------------------------------------------
# add tag to show if recent observation
fish_kill_data <- read_rds("data/fish_kill_data.rds") %>% 
  mutate(is_recent = ifelse(date_observed > today() - 15, 
                            "recent observation (15 days)", 
                            "archived observation (16+ days)"))

summary_table <- read_rds("data/summary_table.rds")
rumsey_flows <- read_rds("data/rumsey_flows.rds")

clear_lake_elevation <- read_rds("data/clear-lake-elevation.rds")