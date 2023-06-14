library(targets)
library(sf)
library(dplyr)
library(readr)
library(neighbourhoodstudy)
library(osmdata)
library(future)

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

  targets::tar_target(ottawa_roads_shp, pseudohouseholds::ottawa_roads_shp),
  targets::tar_target(ons_db_sli, neighbourhoodstudy::sli_dbs_gen3_maxoverlap),

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
                    process_overpass_data(overpass_data, shops, amenities)),

  # Save foodspace results to disk
  targets::tar_target(save_ottawa_food,
                      save_food_data(ottawa_food)),


  # Run distance analysis
  targets::tar_target(ons_phhs,
                      create_ons_phhs (ons_gen3_shp, ons_gen3_pop2021, ottawa_roads_shp)),

  targets::tar_target(result_within_15min_walk,
                      phh_walkdistance (ottawa_phhs, ons_gen3_shp, ottawa_food, ons_db_sli)),
  # targets::tar_target(od_table,
  #                     phh_distance_analysis (ottawa_phhs = ottawa_phhs, ons_gen3_shp = ons_gen3_shp, destinations = ottawa_food, destination_id_col = "osm_id")),

  NULL
)
