library(readr)
library(dplyr)
library(purrr)
library(stringr)

# Read the CSV
birds_df <- read_csv("data/priority_birds_chatgpt_generated - Sheet1.csv")

# Split the 'range_center' field into lat and lng
birds_df <- birds_df %>%
  mutate(
    lat = as.numeric(str_split_fixed(range_center, ",", 2)[,1]),
    lng = as.numeric(str_split_fixed(range_center, ",", 2)[,2])
  )

# Now build the list of lists
birds <- birds_df %>%
  mutate(
    lat = as.numeric(str_split_fixed(range_center, ",", 2)[,1]),
    lng = as.numeric(str_split_fixed(range_center, ",", 2)[,2])
  ) %>% 
  mutate(
    range_center = map2(lat, lng, ~list(lat = .x, lng = .y)), 
    range_radius = range_radius_m
  ) %>%
  select(name, scientific_name, status, habitat, population, image, range_center, range_radius, description) %>%
  pmap(function(name, scientific_name, status, habitat, population, image, range_center, range_radius, description) {
    list(
      name = name,
      scientific_name = scientific_name,
      status = status,
      habitat = habitat,
      population = population,
      image = image,
      range_center = range_center,
      range_radius = range_radius,
      description = description
    )
  })
