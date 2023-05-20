library(sf)
library(dplyr)
library(neighbourhoodstudy)
library(ggplot2)

ons_gen3_shp <- neighbourhoodstudy::ons_gen3_shp |>
  dplyr::filter(ONS_Region == "OTTAWA")

food <- sf::read_sf("data/OSM - Ottawa food API pull (2023-05-19).geojson")

food_ottawa <- food |>
  dplyr::mutate(type = dplyr::if_else(is.na(amenity), shop, amenity)) |>
  dplyr::select(type, name, shop, amenity, brand, dplyr::starts_with("addr")) |>
  sf::st_centroid(food) |>
  sf::st_filter(ons_gen3_shp)



food_ottawa |>
  ggplot() + geom_sf(data=ons_gen3_shp)+ geom_sf(aes(colour = type))


food_ottawa |>
  dplyr::group_by(type) |>
  dplyr::count() |>
  ggplot() + geom_col(aes(x=reorder(type, n), y=n), fill="#123456") + coord_flip() +
  labs(title = "OSM Data: Food Location Counts in Ottawa, ON",
       y = "# Locations",
       x = "Location Type") +
  theme_minimal()
