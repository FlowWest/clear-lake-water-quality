## Retrieves inaturalist data on fishkill project in clearlake
# install.packages("rinat")
library(rinat)
library(tidyverse)

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
write_rds(fish_kill_data, "data/fish_kill_data.rds")

labels <- sprintf("<strong>Taxon:</strong> <strong>%s</strong> (%s) <br/> <strong>User:</strong> %s <br/> <strong>Date:</strong> %s <br/> <strong>Description:</strong> %s",
                  fish_kill_data$taxon_name, 
                  fish_kill_data$taxon_common_name,
                  fish_kill_data$user,
                  fish_kill_data$date_observed,
                  fish_kill_data$description
                  ) %>% lapply(htmltools::HTML)

# Add useful summary stats to table once we decide what we want (for now just current data)
summary_table <- fish_kill_data %>% 
  select(-photo) %>%
  mutate(description = ifelse(is.na(description), "No Description", description),
         Date = as.Date(date_observed)) %>%
  select(taxon_common_name, taxon_name, date_observed, description, latitude, longitude)
 

write_rds(summary_table, "data/summary_table.rds")
