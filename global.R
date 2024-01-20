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
library(httr)
library(jsonlite)
library(dataRetrieval)
library(shinydashboard)
library(rinat)
library(DT)

source("modules/fishkill.R")
source("modules/wq_api.R")
source("modules/contact.R")
# source("modules/water_quality.R")


# Water Quality -----------------------------------------------------------------

# look for the data we wrote from the other repo

# read the stations datasets 
clear_lake_stations <- read_rds("data/clear-lake-stations.rds")

# read the observations datasets
#TODO: have not consolidated usgs and dwr data into clear lake data
clear_lake_wq <- read_rds("data/clear-lake-analyte-data.rds") %>% 
  filter(analyte_name %in% c("Dissolved Oxygen", "pH", "Temperature", "Turbidity", "Fluridone", 
                             "Specific Conductance", "Specific Conductivity"))
clear_lake_wq_choices <- sort(pull(distinct(clear_lake_wq, analyte_name)))
analyte_descriptions <- read_csv("data/analyte-descriptions.csv")
siteNumber <- "11450000"
parametercd <- "00065"


# Fish Kill ---------------------------------------------------------------------
# add tag to show if recent observation
get_latest_inat <- function() {
  fish_kill_observations <- get_inat_obs_project("clear-lake-fish-kill-monitoring-project", type = c("observations", "info"), raw = TRUE)
  fish_kill_data <- tibble(taxon_name = fish_kill_observations$taxon.name,
                           taxon_common_name = fish_kill_observations$taxon.common_name.name, 
                           user = fish_kill_observations$user_login,
                           date_observed = fish_kill_observations$observed_on,
                           record_id = fish_kill_observations$id,
                           description = fish_kill_observations$description,
                           latitude = fish_kill_observations$latitude,
                           longitude = fish_kill_observations$longitude,
                           photo = fish_kill_observations$photos, 
                           link = fish_kill_observations$uri) %>%
    mutate(latitude = as.numeric(latitude),
           longitude = as.numeric(longitude))
}

# add tag to show if recent observation
fish_kill_data <- tryCatch(get_latest_inat(), 
                           error = function(e) {
                             read_rds("data/fish_kill_data.rds")
                           })
fish_kill_data <- fish_kill_data %>% 
  mutate(is_recent = ifelse(date_observed > today() - 15, 
                            "recent observation (15 days)", 
                            "archived observation (16+ days)"))

summary_table <- fish_kill_data %>% 
  select(-photo) %>%
  mutate(description = ifelse(is.na(description), "No Description", description),
         Date = as.Date(date_observed)) %>% 
  select(taxon_common_name, taxon_name, date_observed, description, latitude, longitude)

# summary_table <- read_rds("data/summary_table.rds")
rumsey_flows <- read_rds("data/rumsey_flows.rds")

clear_lake_elevation <- read_rds("data/clear-lake-elevation.rds")

#WQ API ------------------------------------------------------------------------

apikey <- Sys.getenv("WQ_API_KEY")
base_url <- "https://www.wqdatalive.com/api/v1/devices"
edited_oaks_wq <- read_rds("data/oaks_water_quality.rds")
edited_riviera_wq <- read_rds("data/riviera_water_quality.rds")
riviera_id <- 2656
oaks_id <- 3733
wq_parameters <- read.csv("data/water_quality_parameters.csv")
monitoring_stations <- read_rds("data/monitoring_stations_coordinates.rds")

