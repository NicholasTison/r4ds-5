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
top_fires <- big_fires |>
  arrange(desc(gis_acres)) |>
  slice_head(n = 10)

fire_map <- leaflet() |>
  addProviderTiles("CartoDB.Positron")

for (i in seq_len(nrow(top_fires))) {
  coordinates <- top_fires$geometry_coordinates[[i]][[1]][1, , ]
  popup <- paste0(
    "<strong>", top_fires$incident[i], "</strong><br>",
    "Year: ", top_fires$fire_year[i], "<br>",
    "Acres: ", format(top_fires$gis_acres[i], big.mark = ",")
  )

  fire_map <- fire_map |>
    addPolygons(
      lng = coordinates[, 1],
      lat = coordinates[, 2],
      popup = popup,
      fillColor = "firebrick",
      fillOpacity = 0.45,
      color = "firebrick"
    )
}

all_coordinates <- map(top_fires$geometry_coordinates, ~ .x[[1]][1, , ])
longitude_range <- range(unlist(map(all_coordinates, ~ .x[, 1])))
latitude_range <- range(unlist(map(all_coordinates, ~ .x[, 2])))

fire_map |>
  fitBounds(
    lng1 = longitude_range[1],
    lat1 = latitude_range[1],
    lng2 = longitude_range[2],
    lat2 = latitude_range[2]
  )
#
#
#
#| cache: true
imdb_snapshots <- readRDS("data/imdb_snapshots.rds")

print(imdb_snapshots)

imdb_snapshots |>
  count(snap_year)
#
#
#
#| cache: true
rank_changes <- imdb_snapshots |>
  filter(snap_year %in% c(2015, 2022)) |>
  select(title, year, snap_year, rank) |>
  pivot_wider(
    names_from = snap_year,
    values_from = rank,
    names_prefix = "rank_"
  ) |>
  drop_na(rank_2015, rank_2022) |>
  mutate(
    release_decade = floor(year / 10) * 10,
    rank_change = abs(rank_2015 - rank_2022)
  )

rank_changes |>
  group_by(release_decade) |>
  summarize(average_rank_change = mean(rank_change), .groups = "drop") |>
  arrange(release_decade)
#
#
#
#| cache: true
nolan_films <- c(
  "The Dark Knight",
  "Inception",
  "Interstellar",
  "The Dark Knight Rises",
  "Memento",
  "The Prestige",
  "Batman Begins"
)

imdb_snapshots |>
  filter(title %in% nolan_films) |>
  select(title, snap_year, rank) |>
  pivot_wider(names_from = snap_year, values_from = rank, names_prefix = "rank_")
#
#
#
#
#
