load(file = "data-raw/bvr_water_quality.rda")
bvr_analytes <- distinct(bvr_water_quality, analyte) %>% pull()

write_rds(bvr_analytes, "data/bvr-analytes.rds")

#Water Quality API -------------------------------------------------------------
get_station_parameters <-
  function(station_id) {
  device_url = paste0(base_url, "/", station_id, "/", "parameters?", apikey)
  device = GET(url = device_url)
  device_parameters <- fromJSON(rawToChar(device$content))
  parameters_df <- do.call(rbind.data.frame, device_parameters)
  return(parameters_df)
  }

riviera_params <- get_station_parameters(riviera_id)[13:20,]
oaks_params <- get_station_parameters(oaks_id)[13:24,]
wq_parameters <- bind_rows(riviera_params %>% 
                             mutate(sensor_location = 'Riviera'), 
                           oaks_params %>% 
                             mutate(sensor_location = 'Oaks'))
write.csv(wq_parameters, "data/water_quality_parameters.csv")
