library(httr)
library(jsonlite)
library(tidyverse)
library(RCurl)
library(lubridate)
library(plotly)

apikey <- "apiKey=0c1f808db10a43899b8eb3b3dc128b0b"

base_url <- "https://www.wqdatalive.com/api/v1/devices"

devices_full_url  = paste0(base_url, "?",apikey)

#Access API
api_call = GET(url = devices_full_url)
#Could check status_code or content
#Content is in unicode - need to convert it to text and parse the JSON
devices <- fromJSON(rawToChar(api_call$content))

devices_df <- do.call(what = "rbind",
                      args = lapply(devices, as.data.frame))
riviera_id <- devices_df['id'][1,]
clearlake_oaks_id <- devices_df['id'][2,]

#Access device data parameters
riviera_device_url = paste0(base_url, "/", riviera_id,"/","parameters?", apikey)
riviera_device = GET(url = riviera_device_url)
riviera_parameters <- fromJSON(rawToChar(riviera_device$content))

#Riviera West Data (all surface)
#Temperature, Sp Cond, ODOSat, ODO, Chlorophyll, Chlorophyll RFU, BGA-PC, BGA-RHYcocyanin RFU

#Sp Cond: conductivity (uS/cm)
#ODOSat: Optical Dissovled Oxygen (percent air saturation)
#ODO: optical dissolved oxygen (mg/L)
#Phlorophyll: (ug/L)
#Phlorophyll RFU: RFU (relative fluorescence units)
#BGA-PC : Chlorophyll a, Phycocyanin (ug/L)
#BGA-phycocyanin RFU:  RFU (relative fluorescence units)

#Clearlake Oaks Data
#Temperature (surface, lake bed), Sp Cond (surface, lake bed), ODOSat(surface, lake bed), ODO(surface, lakebed), 
# Surface: Chlorophyll, Chlorophyll RFU, BGA-PC, BGA-RHYcocyanin RFU

#Access surface temperature data (parameter id = 53286) from 2021/12/1 10:00 to 2021/12/20/10:00
riviera_surface_temp_url = paste0(base_url, "/", riviera_id,"/","parameters/53286/data?", apikey, "&from=2021-12-01%2010:00:00&to=2021-12-20%2010:00:00")
riviera_surface_temp = GET(url = riviera_surface_temp_url)
riviera_surface <- fromJSON(rawToChar(riviera_surface_temp$content))
raw_riviera_temp_data <- riviera_surface$data   



#Data Transformation
clean_riviera_temp_data <- raw_riviera_temp_data %>%  
  rename("water_temperature_c" = value) %>%
  mutate_at("water_temperature_c", as.numeric) %>%  
  mutate("timestamp" = ymd_hms(raw_riviera_temp_data$timestamp,tz=Sys.timezone())) %>%
  glimpse()

#Visualization
water_temp_fig <- plot_ly(clean_riviera_temp_data, 
               x = ~timestamp, 
               y = ~water_temperature_c, 
               type = 'scatter', 
               mode = 'lines',
               hoverinfo = 'text',
               text = ~paste('<br> Time Stamp: ', timestamp,
                             '<br> Temperature: ', water_temperature_c)) %>% 
  layout(title = "Water Surface Temperature from Riviera West Sensor at Clearlake",
         xaxis = list(title = 'Date'),
         yaxis = list(title = 'Water Temperature (Celsius)')
         )
water_temp_fig
