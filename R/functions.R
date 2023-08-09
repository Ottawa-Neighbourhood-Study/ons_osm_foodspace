
## FUNCTION COULD BE SUITABLE FOR PACKAGING

#' Count points in regions plus a buffer
#'
#' @param region_shp Sf object with polygonal shapes, one row per region, with at
#' least one column providing unique region identifiers.
#' @param point_data Sf object with point data to aggregate to regions.
#' @param region_id_col Character, column name in `region_shp` containing unique
#' region identifiers.
#' @param crs The CRS to convert regions and points to. Defaults to 32189
#' (NAD83 / MTM zone 9) for Ottawa, Ontario.
#' @param buffer_m Buffer in meters to apply to regions.
#'
#' @return A tibble with one row per region and the number of points within each.
#' @export
get_number_per_region_plus_buffer <- function(region_shp, point_data, region_id_col = "ONS_ID", crs = 32189, buffer_m = 50) {

  region_shp <- sf::st_transform(region_shp, crs=32189) |>
    sf::st_buffer(buffer_m) |>
    dplyr::select(-dplyr::any_of(c("join_field", "SHAPE_Leng", "SHAPE_Area", "ONS_Name", "ONS_Region")))

  point_data <- sf::st_transform(point_data, crs=32189)

  raw_counts <- sf::st_join(point_data, region_shp) |>
    sf::st_drop_geometry() |>
    dplyr::group_by(!!rlang::sym(region_id_col)) |>
    dplyr::summarise(num_region_plus_buffer = dplyr::n(), .groups = "drop")

  result <- dplyr::full_join(sf::st_drop_geometry(region_shp), raw_counts, by = region_id_col ) |>
    dplyr::mutate(num_region_plus_buffer = dplyr::if_else(is.na(num_region_plus_buffer), 0, num_region_plus_buffer)) |>
    tidyr::drop_na(!!rlang::sym(region_id_col))

  return(result)
}

#' Count points per 1000 residents in regions plus a buffer
#'
#' @param region_shp Sf object with polygonal shapes, one row per region, with at
#' least one column providing unique region identifiers.
#' @param point_data Sf object with point data to aggregate to regions.
#' @param pop_data Data frame giving regional population data, including at least
#' one column with unique region identifiers and one column with population data.
#' @param region_id_col Character, column name in `region_shp` containing unique
#' region identifiers.
#' @param pop_col character, column name in `pop_data` containing populations.
#' @param crs The CRS to convert regions and points to. Defaults to 32189
#' (NAD83 / MTM zone 9) for Ottawa, Ontario.
#' @param buffer_m Buffer in meters to apply to regions.
#'
#' @return A tibble with one row per region and the number of points within each.
#' @export
get_number_per_region_per_1000_residents_plus_buffer <- function(region_shp, point_data, pop_data, region_id_col = "ONS_ID", pop_col = "SF_TotalPop", crs = 32189, buffer_m = 50) {

  num_per_region <- get_number_per_region_plus_buffer(region_shp, point_data, region_id_col, crs, buffer_m)

  result <- dplyr::left_join(num_per_region, pop_data, by = region_id_col) |>
    dplyr::mutate(num_per_1000_res_plus_buffer = num_region_plus_buffer / SF_TotalPop * 1000) |>
    dplyr::select(dplyr::all_of(region_id_col), num_per_1000_res_plus_buffer)

  result <- result |>
    #dplyr::arrange(dplyr::desc(num_per_1000)) |> View()
    tidyr::drop_na(!!rlang::sym(region_id_col)) |>
    dplyr::mutate(num_per_1000_res_plus_buffer = dplyr::if_else(is.na(num_per_1000_res_plus_buffer) | is.infinite(num_per_1000_res_plus_buffer) | is.nan(num_per_1000_res_plus_buffer), 0, num_per_1000_res_plus_buffer))

  return(result)
}


get_db_centroid_walkdistance <- function(ottawa_dbs_shp, foodspace, foodspace_id_col = "osm_id") {


  testsize <- 500000

  foodspace_latlon <- dplyr::bind_cols(
    foodspace, sf::st_coordinates(foodspace)
  ) |>
    dplyr::rename(lat = Y, lon = X) |>
    sf::st_drop_geometry() |>
    dplyr::select(dplyr::all_of(c(foodspace_id_col, "lat", "lon"))) |>  head(n = testsize)


  db_centroids <- sf::st_centroid(sf::st_make_valid(ottawa_dbs_shp)) |> suppressWarnings()

  db_centroids_latlon <- dplyr::bind_cols(
    db_centroids, sf::st_coordinates(db_centroids)
  ) |>
    dplyr::rename(lat = Y, lon = X) |>
    sf::st_drop_geometry() |>
    dplyr::select(DBUID, lat, lon) |> head(n = testsize)

  future::plan(future::multisession, workers = 10)

  tranchesize <- 5

  tranches <-
    db_centroids_latlon |>
    tibble::rowid_to_column() |>
    dplyr::group_by(floor(rowid/tranchesize)) |>
    dplyr::group_split() #|>
  #head(5)

  warning("testing with only some data! use with caution!!!!")

  results <- furrr::future_map(tranches, function(db_centroids_latlon) {
    valhallr::od_table(froms = db_centroids_latlon, from_id_col = "DBUID",
                       costing="pedestrian",
                       batch_size=tranchesize,
                       tos = foodspace_latlon, to_id_col = foodspace_id_col, verbose = TRUE , hostname = "192.168.0.150")
  }, .progress=TRUE)

  future::plan(future::sequential)

  results <- dplyr::bind_rows(results)
  results
}



#### GET DISTANCE TO CLOSEST 3 FEATURES FROM DB CENTROIDS AFTER DOING DISTANCE CALCS
# od_table: tibble output from valhallr::od_table() with columns `distance` and `time`
# not quite general function, includes hard-coded DB-to-DA column mutations
get_avg_dist_to_closest_three <- function(od_table, from_id_col = "DBUID", to_id_col, froms_to_ons_sli, dbpops) {

  # take the od_table, get top 3 closest by distance for each origin,
  # average them, get populations for DBUIDs, convert DBUIDs to DAUIDs, use
  # single-link indicator to map DAs to ONS hoods, pop-weight avg distance from
  # DAs up to gen3 hoods
  result <-  tidyr::drop_na(od_table) |>
    dplyr::group_by(!!rlang::sym(from_id_col)) |>
    dplyr::arrange(distance) |>
    dplyr::slice_head(n=3) |>
    dplyr::summarise(dist_closest_3 = mean(distance, na.rm = TRUE)) |>
    dplyr::left_join(dbpops, by = "DBUID") |>
    dplyr::mutate(DAUID = substr(DBUID, 1, 8)) |>
    dplyr::left_join(froms_to_ons_sli, by = "DAUID") |>
    dplyr::select(DAUID, dist_closest_3, dbpop2021, ONS_ID) |>
    dplyr::group_by(ONS_ID) |>
    dplyr::summarise(dist_closest_3_popwt = sum((dist_closest_3 * dbpop2021) / sum(dbpop2021) ))

  return(result)
}


# take the od_table, find out if DBs have ANY trips under 15 minutes,
# add DB pops, link DBs to DAs, link DAs to hoods using sli, use db pops and
# whether they're covered or not to estimate % of hood pop within 15 minutes
get_pct_within_15_mins <- function(od_table, from_id_col = "DBUID", to_id_col, froms_to_ons_sli, dbpops) {

  # take the od_table, find out if DBs have ANY trips under 15 minutes,
  # add DB pops, link DBs to DAs, link DAs to hoods using sli, use db pops and
  # whether they're covered or not to estimate % of hood pop within 15 minutes
  result <-  tidyr::drop_na(od_table) |>
    dplyr::group_by(!!rlang::sym(from_id_col)) |>
    dplyr::summarise(covered = any (time < 15 * 60)) |>
    dplyr::left_join(dbpops, by = "DBUID") |>
    dplyr::mutate(DAUID = substr(DBUID, 1, 8)) |>
    dplyr::left_join(froms_to_ons_sli, by = "DAUID") |>
    dplyr::select(DAUID, covered, dbpop2021, ONS_ID) |>
    dplyr::group_by(ONS_ID) |>
    dplyr::summarise(pct_within_15_mins = sum((covered * dbpop2021) / sum(dbpop2021) ))

  return(result)
}




compute_and_save_stats <- function(prefix, foodspace_data, foodspace_distances, ons_gen3_shp, pop_data, single_link_indicator, dbpops ) {

  # to make it recompute
  nudge <- 1

  # remove errant items
  foodspace_distances <- dplyr::filter(foodspace_distances, !is.na(DBUID))

  # get the four values of interest
  num_per_region_plus_buffer <- get_number_per_region_plus_buffer(ons_gen3_shp, foodspace_data)
  num_per_1000_res_plus_buffer <- get_number_per_region_per_1000_residents_plus_buffer(region_shp = ons_gen3_shp, point_data = foodspace_data, pop_data = neighbourhoodstudy::ons_gen3_pop2021, region_id_col = "ONS_ID", pop_col = "SF_TotalPop")
  avg_dist_closest_3 <- get_avg_dist_to_closest_three(foodspace_distances, from_id_col = "DBUID", to_id_col = "osm_id", froms_to_ons_sli = neighbourhoodstudy::sli_das_gen3_mape, dbpops = neighbourhoodstudy::ottawa_dbs_pop2021)
  pct_within_15_minutes <- get_pct_within_15_mins(od_table = foodspace_distances, from_id_col = "DBUID", to_id_col = "osm_id", froms_to_ons_sli = neighbourhoodstudy::sli_das_gen3_mape, dbpops = neighbourhoodstudy::ottawa_dbs_pop2021)

  # combine and tidy
  results_tidy <- dplyr::left_join(num_per_region_plus_buffer, num_per_1000_res_plus_buffer, by = "ONS_ID") |>
    dplyr::left_join(avg_dist_closest_3, by = "ONS_ID") |>
    dplyr::left_join(pct_within_15_minutes, by = "ONS_ID") |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, digits = 3)  )) |>
    dplyr::rename_with(.cols = - ONS_ID, .fn = \(x) paste0(prefix, x))

  # create a wide version too
  results_wide <- results_tidy |>
    dplyr::arrange(ONS_ID) |>
    tidyr::pivot_longer(cols = -ONS_ID, names_to = "polygon_attribute", values_to = "value") |>
    tidyr::pivot_wider(id_cols = polygon_attribute, names_from = "ONS_ID", values_from = "value")

  # save to file
  datestamp <- Sys.Date()
  if (!dir.exists(paste0("output/", datestamp))) dir.create(paste0("output/", datestamp))

  output_dir <- paste0("output/", datestamp,"/hood_stats")
  if (!dir.exists(output_dir)) dir.create(output_dir)

  readr::write_csv(results_tidy, paste0(output_dir,"/", prefix, "_tidy_", Sys.Date(), ".csv"))

  readr::write_csv(results_wide, paste0(output_dir,"/", prefix, "_wide_", Sys.Date(), ".csv"))

  return(results_tidy)
}



mapcheck <- function(foodspace_data, foodspace_stats, ons_gen3_shp) {

  foodspace_data <- foodspace_grocery
  foodspace_stats <- foodspace_stats_grocery




}

# SAVE FOODSPACE FILES AS CSV AND SHP
save_food_files <- function(foodspace_convenience, foodspace_fast_food, foodspace_grocery, foodspace_restaurant, foodspace_specialty) {

  # save to file as csv
  # create date folder if needed
  datestamp <- Sys.Date()
  if (!dir.exists(paste0("output/", datestamp))) dir.create(paste0("output/", datestamp))

  output_dir <- paste0("output/", datestamp,"/foospace-csv")
  if (!dir.exists(output_dir)) dir.create(output_dir)

  readr::write_csv(sf_to_latlon(foodspace_convenience),
                   paste0(output_dir, "/food_convenience_", Sys.Date(), ".csv"))

  readr::write_csv(sf_to_latlon(foodspace_fast_food),
                   paste0(output_dir, "/food_fastfood_", Sys.Date(), "csv"))

  readr::write_csv(sf_to_latlon(foodspace_grocery),
                   paste0(output_dir, "/food_grocery_", Sys.Date(), ".csv"))

  readr::write_csv(sf_to_latlon(foodspace_restaurant),
                   paste0(output_dir, "/food_restaurant_", Sys.Date(), ".csv"))

  readr::write_csv(sf_to_latlon(foodspace_specialty),
                   paste0(output_dir, "/food_specialty_", Sys.Date(), ".csv"))




  # save to file as shapefiles
  datestamp <- Sys.Date()
  output_dir <- paste0("output/", datestamp,"/foospace-shp")
  if (!dir.exists(output_dir)) dir.create(output_dir)

  sf::write_sf(foodspace_convenience,
               paste0(output_dir, "/food_convenience_", Sys.Date(), ".shp"))

  sf::write_sf(foodspace_fast_food,
               paste0(output_dir, "/food_fastfood_", Sys.Date(), ".shp"))

  sf::write_sf(foodspace_grocery,
               paste0(output_dir, "/food_grocery_", Sys.Date(), ".shp"))

  sf::write_sf(foodspace_restaurant,
               paste0(output_dir, "/food_restaurant_", Sys.Date(), ".shp"))

  sf::write_sf(foodspace_specialty,
               paste0(output_dir, "/food_specialty_", Sys.Date(), ".shp"))


  return(TRUE)

}


sf_to_latlon <- function(shp) {
  # make sure in lat/lon wgs84
  shp <- sf::st_transform(shp, "WGS84")

  # get latlon in matrix
  latlon <- sf::st_coordinates(shp)

  # bind latlon to geometryless input, rename latlon columns
  dplyr::bind_cols(sf::st_drop_geometry(shp), latlon) |>
    dplyr::rename(lat = Y, lon = X)
}
