library(adobeCreekData)

bvr_analytes <- distinct(bvr_water_quality, analyte) %>% pull()

write_rds(bvr_analytes, "data/bvr-analytes.rds")
