library(targets)
library(sf)
library(dplyr)
library(readr)
library(neighbourhoodstudy)
library(osmdata)

source("R/functions.R")

list(

  # ONS Gen3 shape and buffer, load from neighbourhoodstudy package
  targets::tar_target(ons_gen3_shp, dplyr::filter(neighbourhoodstudy::ons_gen3_shp, ONS_Region == "OTTAWA")),
  targets::tar_target(ons_gen3_buffer,
                      ons_gen3_shp |>
                        sf::st_transform(ons_gen3_shp, crs=32189) |>
                        sf::st_union() |>
                        sf::st_buffer(10000) |>
                        sf::st_transform(crs="WGS84")),

  # OSM Overpass query parameters
  targets::tar_target(amenities,
                      c("restaurant",
                        "pub",
                        "bar",
                        "cafe",
                        "fast_food",
                        "food_court" )),

  targets::tar_target(shops,
                      c("grocery",
                        "convenience",
                        "deli",
                        "greengrocer",
                        "food",
                        "general",
                        "supermarket")),

  # Query Overpass API
  targets::tar_target(overpass_data,
                      query_osm_api_food(ons_gen3_buffer, amenities, shops)),

  targets::tar_target(ottawa_food,
                    process_overpass_data(overpass_data)),

  # Save foodspace results to disk
  targets::tar_target(save_ottawa_food,
                      save_food_data(ottawa_food)),


  NULL
)
