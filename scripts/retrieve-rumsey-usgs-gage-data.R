## Retrieves rumsey gage data from USGS

library(dataRetrieval)
library(tidyverse)

station_id <- 11451800 # id for station cache creek a rumsey ca
date <- Sys.Date() - 30 # pull data for 2 weeks prior
parameterCd <- "00060"

rumsey_flows_prior_2_weeks <- dataRetrieval::readNWISdv(station_id, parameterCd, date)
rumsey_flows <- rumsey_flows_prior_2_weeks %>%
  select(Date, "Daily Flow Rumsey" =  X_00060_00003) %>%
  as_tibble()

write_rds(rumsey_flows, "data/rumsey_flows.rds")
