## Retrieves rumsey gage data from USGS

library(dataRetrieval)
library(tidyverse)

station_id <- 11451800 # id for station cache creek a rumsey ca
date <- "2010-01-01" # pull data for 2 weeks prior
parameterCd <- "00060"

rumsey_flows_prior_2_weeks <- dataRetrieval::readNWISdv(station_id, parameterCd, date)
rumsey_flows <- rumsey_flows_prior_2_weeks %>%
  select(Date, "Daily Flow Rumsey" =  X_00060_00003) %>%
  as_tibble()

write_rds(rumsey_flows, "data/rumsey_flows.rds")


# clear lake elevation
clear_lake_id <- 11450000
clear_lake_param_cd <- "00065"

clear_lake_elevation <- readNWISdv(siteNumbers = clear_lake_id, 
                                   parameterCd = clear_lake_param_cd, 
                                   startDate = date, 
                                   endDate = Sys.Date()) %>% 
  select(Date, `Elevation (ft)` = X_00065_00003)

write_rds(clear_lake_elevation, "data/clear-lake-elevation.rds")

