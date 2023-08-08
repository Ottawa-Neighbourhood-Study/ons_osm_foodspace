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
  targets::tar_target(dbpops, neighbourhoodstudy::ottawa_dbs_pop2021),
  targets::tar_target(da_ons_sli, neighbourhoodstudy::sli_das_gen3_mape),
  # targets::tar_targeT(ottawa_phhs, )

  # OSM Overpass query parameters
  targets::tar_target(specialty_shops,
                      c("bakery", "beverages", "butcher", "cheese", "chocolate", "coffee", "confectionary", "deli", "dairy", "health_food", "ice_cream", "pastry", "seafood", "spices", "tea")),
  targets::tar_target(specialty_amenities,
                      c("ice_cream")),

  targets::tar_target(fast_food_amenities,
                      c("fast_food", "food_court", "cafe")),

  targets::tar_target(convenience_shops,
                      c("convenience")),

  targets::tar_target(grocery_shops,
                      c("greengrocer", "supermarket")),

  targets::tar_target(restaurant_amenities,
                      c("pub", "restaurant")),



  # Query Overpass API
  targets::tar_target(foodspace_specialty,
                      neighbourhoodstudy::query_osm_api(bounding_box_shp = ons_gen3_buffer, amenities = specialty_amenities, shops = specialty_shops)),

  targets::tar_target(foodspace_fast_food,
                      neighbourhoodstudy::query_osm_api(bounding_box_shp = ons_gen3_buffer, amenities = fast_food_amenities, shops = "")),

  targets::tar_target(foodspace_convenience,
                      neighbourhoodstudy::query_osm_api(bounding_box_shp = ons_gen3_buffer, amenities = "", shops = convenience_shops)),

  targets::tar_target(foodspace_grocery,
                      neighbourhoodstudy::query_osm_api(bounding_box_shp = ons_gen3_buffer, amenities = "", shops = grocery_shops)),

  targets::tar_target(foodspace_restaurant,
                      neighbourhoodstudy::query_osm_api(bounding_box_shp = ons_gen3_buffer, amenities = restaurant_amenities, shops = "")),

  ######## VALHALLA DISTANCE ANALYSIS

  targets::tar_target(foodspace_specialty_distances,
                      get_db_centroid_walkdistance (neighbourhoodstudy::ottawa_dbs_shp2021, foodspace_specialty, foodspace_id_col = "osm_id")),

  targets::tar_target(foodspace_grocery_distances,
                      get_db_centroid_walkdistance (neighbourhoodstudy::ottawa_dbs_shp2021, foodspace_grocery, foodspace_id_col = "osm_id")),

  ######## SPECIALTY
  targets::tar_target(foodspace_stats_specialty, {

    compute_and_save_stats (prefix = "food_specialty", foodspace_data = foodspace_specialty,
                            foodspace_distances = foodspace_specialty_distances,
                            ons_gen3_shp = ons_gen3_shp, pop_data = dbpops, single_link_indicator = da_ons_sli )


    }),

  targets::tar_target(foodspace_stats_avgdistclosest3,

                      FALSE
  ),

  targets::tar_target(foodspace_stats_pctwithin15minswalk,
                      FALSE
  ),

  ######## FAST FOOD

  ######## CONVENIENCE

  ######## GROVERY

  ######## RESTAURANT
  #
  #   targets::tar_target(overpass_data,
  #                       query_osm_api_food(ons_gen3_buffer, amenities, shops)),
  #
  #   targets::tar_target(ottawa_food,
  #                     process_overpass_data(overpass_data, shops, amenities)),
  #
  #   # Save foodspace results to disk
  #   targets::tar_target(save_ottawa_food,
  #                       save_food_data(ottawa_food)),


  # # Run distance analysis
  # targets::tar_target(ons_phhs,
  #                     create_ons_phhs (ons_gen3_shp, ons_gen3_pop2021, ottawa_roads_shp)),
  #
  # targets::tar_target(result_within_15min_walk,
  #                     phh_walkdistance (ottawa_phhs, ons_gen3_shp, ottawa_food, ons_db_sli)),
  # targets::tar_target(phh_destination_candidates,
  #                     get_destination_candidates(ottawa_phhs = ottawa_phhs, ons_gen3_shp = ons_gen3_shp, destinations = ottawa_food, destination_id_col = "osm_id")),
  # targets::tar_target(candidate_distances,
  #                    get_candidate_distances (phh_destination_candidates, ottawa_food)),
  # targets::tar_target(od_table,
  #                     phh_distance_analysis (ottawa_phhs = ottawa_phhs, ons_gen3_shp = ons_gen3_shp, destinations = ottawa_food, destination_id_col = "osm_id")),

  NULL
)


# targets::tar_target(amenities,
#                     c("restaurant",
#                       "pub",
#                       "bar",
#                       "cafe",
#                       "fast_food",
#                       "food_court" )),
#
# targets::tar_target(shops,
#                     c("grocery",
#                       "convenience",
#                       "deli",
#                       "greengrocer",
#                       "food",
#                       "general",
#                       "supermarket")),
