#
#
#
#
#
#
#
#
#
#| message: false
#| cache: true
library(tidyverse)
library(purrr)
library(leaflet)
library(rvest)
library(httr2)
library(jsonlite)

raw <- fromJSON("data/wildfires.geojson")
print(length(raw$features))

first_fire <- raw$features[[1]]
print(names(first_fire))

first_coordinate <- raw$features$geometry$coordinates[[1]][1, 1, ]
print(first_coordinate)

fire_tb <- tibble(features = raw$features)
print(fire_tb)

fires <- fire_tb |>
  unnest_wider(features) |>
  unnest_wider(properties) |>
  unnest_wider(geometry, names_sep = "_") |>
  transmute(
    incident = incident,
    gis_acres = as.numeric(gis_acres),
    fire_year = as.integer(fire_year),
    agency = agency,
    state = state,
    geometry_coordinates = geometry_coordinates
  )

```{r}
#| cache: true
fires |>
  group_by(state) |>
  summarize(total_acres = sum(gis_acres, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(total_acres)) |>
  slice_head(n = 10) |>
  ggplot(aes(x = reorder(state, total_acres), y = total_acres)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(
    title = "Top 10 states by total acres burned",
    x = "State",
    y = "Total acres burned",
    caption = "Source: wildfire GeoJSON data."
  ) +
  theme_minimal()
#
#
#
#| cache: true
big_fires <- fires |>
  filter(gis_acres >= 100000) |>
  mutate(
    lon = map_dbl(geometry_coordinates, ~ mean(.x[[1]][1, , 1])),
    lat = map_dbl(geometry_coordinates, ~ mean(.x[[1]][1, , 2]))
  )
#
#
#
#| cache: true
big_fires |>
  count(agency, sort = TRUE) |>
  ggplot(aes(x = reorder(agency, n), y = n)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(
    title = "Number of big fires by managing agency",
    x = "Managing agency",
    y = "Number of fires"
  ) +
  theme_minimal()
#
#
#
#| cache: true
leaflet(big_fires) |>
  addProviderTiles("CartoDB.Positron") |>
  addCircleMarkers(
    ~lon,
    ~lat,
    radius = ~sqrt(gis_acres) / 20,
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~paste0(incident, "<br>", format(gis_acres, big.mark = ","), " acres")
  ) |>
  fitBounds(lng1 = -125, lat1 = 31, lng2 = -102, lat2 = 49)
#
#
#
#
#
