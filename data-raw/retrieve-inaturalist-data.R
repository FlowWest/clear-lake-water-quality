## Retrieves inaturalist data on fishkill project in clearlake
# install.packages("rinat")
library(rinat)
fish_kill_observations <- get_inat_obs_project("clear-lake-fish-kill-monitoring-project", type = c("observations", "info"), raw = TRUE)
fish_kill_data <- tibble(taxon_name = fish_kill_observations$taxon.name,
                         taxon_common_name = fish_kill_observations$taxon.common_name.name, 
                         user = fish_kill_observations$user_login,
                         date_observed = fish_kill_observations$observed_on,
                         record_id = fish_kill_observations$id,
                         description = fish_kill_observations$description,
                         latitude = fish_kill_observations$latitude,
                         longitude = fish_kill_observations$longitude,
                         photo = fish_kill_observations$photos) %>%
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude))
write_rds(fish_kill_data, "data/fish_kill_data.rds")

labels <- sprintf("<strong>Taxon:</strong> <strong>%s</strong> (%s) <br/> <strong>User:</strong> %s <br/> <strong>Date:</strong> %s <br/> <strong>Description:</strong> %s",
                  fish_kill_data$taxon_name, 
                  fish_kill_data$taxon_common_name,
                  fish_kill_data$user,
                  fish_kill_data$date_observed,
                  fish_kill_data$description
                  ) %>% lapply(htmltools::HTML)
leaflet() %>%
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Map") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  addCircleMarkers(data = fish_kill_data,  label = labels,
                   color = "#972D15", 
                   weight = 1.5,
                   opacity =  1, fillOpacity = 1, 
                   labelOptions = labelOptions(style = list("font-size" = "14px"))
  )
