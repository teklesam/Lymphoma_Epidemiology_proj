#  NHL Blood AMR Analysis--------------------------------------------------------

## Title:-----------------------------------------------------------------------

# Analysis of epidemiology of Lymphoma in NHL, Asmara, Eritrea 


# Written by : Samuel Tekle,MD--------------------------------------------------
# Copyright (c) - 2024 Samuel Tekle

# Version control:--------------------------------------------------------------
# Started on : 27.10.2024

#-------------------------------------------------------------------------------

install.packages(c("tidygeocoder", "dplyr", "leaflet"))

library(pacman)

pacman::p_load(
  tidygeocoder,
  leaflet,
  gtsummary,            # Tabulation of data in publication-friendly format
  tidyverse,            # data manipulation and visualization
  labelled,             # Labelling the variables or column names
  usethis,        
  here,             
  rio,              
  skimr,
  stringr,
  forcats,
  janitor,
  ggplot2,
  dplyr,
  lubridate,
  scales,
  incidence
)

## 0.2 Import data--------------------------------------------------------------

lymphoma_geo_data <-  rio::import(here("1. Data",
                                       "geo_coded_lymphoma_data.xlsx"))

# Filter unique addresses to minimize API calls
unique_addresses <- lymphoma_geo_data %>%
  filter(!is.na(address)) %>%
  distinct(address)

# Geocode the addresses
geocoded_addresses <- unique_addresses %>%
  geocode(address = address, method = "osm", full_results = TRUE)

# Merge geocoded data back with the original dataset
lymphoma_geo_data <- lymphoma_geo_data %>%
  left_join(geocoded_addresses, by = "address")




# Exporting geocoded data


rio::export(lymphoma_geo_data,here("3. Output",
                                   "lymphoma_geo_data.rds"))
rio::export(lymphoma_geo_data,here("3. Output",
                                   "lymphoma_geo_data.xlsx"))

# Importing

rio::import(lymphoma_geo_data,here("3. Output",
                                   "lymphoma_geo_data.xlsx"))

# Filter out rows where lat/long are NA (geocoding failures)
map_data <- lymphoma_geo_data %>% filter(!is.na(lat) & !is.na(long))

# Create an interactive map
leaflet(map_data) %>%
  addTiles() %>%
  addCircleMarkers(
    ~long, ~lat,
    color = ~ifelse(diagnosis_binned == "Hodgkins Lymphoma", "blue", 
                    ifelse(diagnosis_binned == "non-Hodgkins Lymphoma", "red", 
                           "green")),
    popup = ~paste("Address:", address, "<br>",
                   "Diagnosis:", diagnosis_binned, "<br>",
                   "Age:", age),
    radius = 5
  )

summary(map_data)

leaflet(map_data) %>%
  addTiles() %>%
  addCircleMarkers(
    ~long, ~lat,
    color = ~ifelse(diagnosis_binned == "Hodgkins Lymphoma", "blue", 
                    ifelse(diagnosis_binned == "non-Hodgkins Lymphoma", "red", 
                           "green")),
    popup = ~paste("Address:", address, "<br>",
                   "Diagnosis:", diagnosis_binned, "<br>",
                   "Age:", age),
    radius = 5
  ) %>%
  fitBounds(lng1 = min(map_data$long, na.rm = TRUE),
            lat1 = min(map_data$lat, na.rm = TRUE),
            lng2 = max(map_data$long, na.rm = TRUE),
            lat2 = max(map_data$lat, na.rm = TRUE))


# Load required libraries
library(dplyr)
library(leaflet)

# Aggregate data to get count of cases at each location
map_data_aggregated <- map_data %>%
  group_by(long, lat, address, diagnosis_binned) %>%
  summarise(count = n(), .groups = 'drop')

# Generate the map
leaflet(map_data_aggregated) %>%
  addTiles() %>%
  addCircleMarkers(
    ~long, ~lat,
    color = ~ifelse(diagnosis_binned == "Hodgkins Lymphoma", "blue", 
                    ifelse(diagnosis_binned == "non-Hodgkins Lymphoma", "red", 
                           "green")),
    radius = ~sqrt(count) * 2,  # Adjust size based on count, scaled with square root for readability
    popup = ~paste("Address:", address, "<br>",
                   "Diagnosis:", diagnosis_binned, "<br>",
                   "Cases:", count),
    fillOpacity = 0.6
  ) %>%
  fitBounds(lng1 = min(map_data_aggregated$long, na.rm = TRUE),
            lat1 = min(map_data_aggregated$lat, na.rm = TRUE),
            lng2 = max(map_data_aggregated$long, na.rm = TRUE),
            lat2 = max(map_data_aggregated$lat, na.rm = TRUE))
# Filter data for cases in Asmara
library(stringr)

# Filter data for cases in Asmara
asmara_data <- map_data %>% filter(str_detect(name, "Asmara"))

library(ggplot2)
library(dplyr)
library(sf)          # For handling spatial data

getwd()



# Merge the geographic data with `map_data` for plotting
map_data <- map_data %>% mutate(adress_by_zone = str_to_title(adress_by_zone))  # Clean up zone names if needed

# Countrywide Map with Regional Delineations and Labels
country_map <- ggplot() +
  geom_sf(data = eritrea_shape, fill = "gray95", color = "black", size = 0.3) +   # Regions as background
  geom_point(data = map_data, aes(x = long, y = lat, color = diagnosis_binned), size = 2) +
  scale_color_manual(values = c("Hodgkins Lymphoma" = "blue", 
                                "non-Hodgkins Lymphoma" = "red", 
                                "Unclassified Lymphoma" = "green")) +
  geom_sf_text(data = eritrea_shape, aes(label = NAME_1), size = 3, color = "black", fontface = "bold") +
  labs(title = "Lymphoma Cases Across Eritrea",
       subtitle = "Showing distribution by administrative regions",
       x = "Longitude", y = "Latitude",
       color = "Diagnosis Type") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom") +
  coord_sf(xlim = c(36, 43), ylim = c(12, 18))   # Set boundaries for Eritrea

# Zoomed Map for Asmara
asmara_map <- ggplot() +
  geom_sf(data = eritrea_shape, fill = "gray95", color = "black", size = 0.3) +
  geom_point(data = map_data %>% filter(str_detect(adress, "Asmara")),
             aes(x = long, y = lat, color = diagnosis_binned), size = 2) +
  scale_color_manual(values = c("Hodgkins Lymphoma" = "blue", 
                                "non-Hodgkins Lymphoma" = "red", 
                                "Unclassified Lymphoma" = "green")) +
  labs(title = "Lymphoma Cases in Asmara",
       subtitle = "Detailed view of case distribution within Asmara",
       x = "Longitude", y = "Latitude",
       color = "Diagnosis Type") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom") +
  coord_sf(xlim = c(38.85, 39.1), ylim = c(15.2, 15.4))   # Zoom to Asmara coordinates

# Save as high-resolution PNG for publication
ggsave("Countrywide_Lymphoma_Map_Detailed.png", plot = country_map, width = 10, height = 7, dpi = 300)
ggsave("Asmara_Lymphoma_Map_Detailed.png", plot = asmara_map, width = 10, height = 7, dpi = 300)
