library(targets)
suppressPackageStartupMessages(library(sf))
library(dplyr, warn.conflicts = FALSE)
library(readr)
suppressPackageStartupMessages(library(neighbourhoodstudy))
suppressPackageStartupMessages(library(osmdata))
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

  targets::tar_target(foodspace_fastfood_distances,
                      get_db_centroid_walkdistance (neighbourhoodstudy::ottawa_dbs_shp2021, foodspace_fast_food, foodspace_id_col = "osm_id")),

  targets::tar_target(foodspace_convenience_distances,
                      get_db_centroid_walkdistance (neighbourhoodstudy::ottawa_dbs_shp2021, foodspace_convenience, foodspace_id_col = "osm_id")),

  targets::tar_target(foodspace_grocery_distances,
                      get_db_centroid_walkdistance (neighbourhoodstudy::ottawa_dbs_shp2021, foodspace_grocery, foodspace_id_col = "osm_id")),

  targets::tar_target(foodspace_restaurant_distances,
                      get_db_centroid_walkdistance (neighbourhoodstudy::ottawa_dbs_shp2021, foodspace_restaurant, foodspace_id_col = "osm_id")),


   ######## COMPUTE STATS

  # Specialty
  targets::tar_target(foodspace_stats_specialty, {

    compute_and_save_stats (prefix = "food_specialty_", foodspace_data = foodspace_specialty,
                            foodspace_distances = foodspace_specialty_distances,
                            ons_gen3_shp = ons_gen3_shp, pop_data = dbpops, single_link_indicator = da_ons_sli )
  }),

  # Fast food
  targets::tar_target(foodspace_stats_fastfood, {

    compute_and_save_stats (prefix = "food_fastfood_", foodspace_data = foodspace_fast_food,
                            foodspace_distances = foodspace_fastfood_distances,
                            ons_gen3_shp = ons_gen3_shp, pop_data = dbpops, single_link_indicator = da_ons_sli )
  }),

  # Convenience
  targets::tar_target(foodspace_stats_convenience, {

    compute_and_save_stats (prefix = "food_convenience_", foodspace_data = foodspace_convenience,
                            foodspace_distances = foodspace_convenience_distances,
                            ons_gen3_shp = ons_gen3_shp, pop_data = dbpops, single_link_indicator = da_ons_sli )
  }),

  # Grocery
  targets::tar_target(foodspace_stats_grocery, {

    compute_and_save_stats (prefix = "food_grocery_", foodspace_data = foodspace_grocery,
                            foodspace_distances = foodspace_grocery_distances,
                            ons_gen3_shp = ons_gen3_shp, pop_data = dbpops, single_link_indicator = da_ons_sli )
  }),

  # Restaurant
  targets::tar_target(foodspace_stats_restaurant, {

    compute_and_save_stats (prefix = "food_resto_", foodspace_data = foodspace_restaurant,
                            foodspace_distances = foodspace_restaurant_distances,
                            ons_gen3_shp = ons_gen3_shp, pop_data = dbpops, single_link_indicator = da_ons_sli )
  }),

  # Consolidated
  targets::tar_target(foodspace_stats_consolidated, {
    save_consolidated_food_stats(foodspace_stats_convenience,
                                 foodspace_stats_fastfood,
                                 foodspace_stats_grocery,
                                 foodspace_stats_restaurant,
                                 foodspace_stats_specialty)
  }),

  # SAVE GEOMETRY

  targets::tar_target(save_foodspace, {
    save_food_files(foodspace_convenience,
                        foodspace_fast_food,
                        foodspace_grocery,
                        foodspace_restaurant,
                        foodspace_specialty)
  }),



  NULL
)
