load(file = "data-raw/bvr_water_quality.rda")
bvr_analytes <- distinct(bvr_water_quality, analyte) %>% pull()

write_rds(bvr_analytes, "data/bvr-analytes.rds")

#Water Quality API -------------------------------------------------------------
#create selection that returns monitoring device id
get_station_parameters <-
  function(station_id) {
    device_url = paste0(base_url, "/", station_id, "/", "parameters?apiKey=", apikey)
    device = GET(url = device_url)
    device_parameters <- fromJSON(rawToChar(device$content))
    parameters_df <- do.call(rbind.data.frame, device_parameters)
  }
riviera_id <- 2656
oaks_id <- 3733

riviera_params <- get_station_parameters(riviera_id)[13:20,]
oaks_params <- get_station_parameters(oaks_id)[13:24,]
wq_parameters <- bind_rows(riviera_params %>% 
                             mutate(sensor_location = 'Riviera'), 
                           oaks_params %>% 
                             mutate(sensor_location = 'Oaks'))
write.csv(wq_parameters, "data/water_quality_parameters.csv")

edited_riviera_wq <- c("Surface Temperature (F)",
                       "Surface Specific Conductivity (uS/cm)",
                       "Surface Dissolved Oxygen Saturation (%)",
                       "Surface Dissolved Oxygen (mg/L)",
                       "Surface Chlorophyll (ug/L)",
                       "Surface Chlorophyll (RFU)",
                       "Surface Phycocyanin (ug/L)",
                       "Surface Phycocyanin (RFU)") 
write_rds(edited_riviera_wq, "data/riviera_water_quality.rds")

edited_oaks_wq <- c( "Lake Bed Temperature (F)",                
                     "Lake Bed Specific Conductivity (uS/cm)",           
                     "Lake Bed Dissolved Oxygen Saturation (%)",
                     "Lake Bed Dissolved Oxygen (mg/L)",        
                     "Lake Bed pH mV",                 
                     "Lake Bed pH",            
                     "Surface Temperature (F)", 
                     "Surface Conductivity (uS/cm)",         
                     "Surface Specific Conductivity (uS/cm)",              
                     "Surface Salinity (PPT)",               
                     "Surface pH mV",              
                     "Surface pH")

write_rds(edited_oaks_wq, "data/oaks_water_quality.rds")
