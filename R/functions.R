#'
#' ## FUNCTION COULD BE SUITABLE FOR PACKAGING
#'
#' #' Count points in regions plus a buffer
#' #'
#' #' @param region_shp Sf object with polygonal shapes, one row per region, with at
#' #' least one column providing unique region identifiers.
#' #' @param point_data Sf object with point data to aggregate to regions.
#' #' @param region_id_col Character, column name in `region_shp` containing unique
#' #' region identifiers.
#' #' @param crs The CRS to convert regions and points to. Defaults to 32189
#' #' (NAD83 / MTM zone 9) for Ottawa, Ontario.
#' #' @param buffer_m Buffer in meters to apply to regions.
#' #'
#' #' @return A tibble with one row per region and the number of points within each.
#' #' @export
#' get_number_per_region_plus_buffer <- function(region_shp, point_data, region_id_col = "ONS_ID", crs = 32189, buffer_m = 50) {
#'
#'   region_shp <- sf::st_transform(region_shp, crs=32189) |>
#'     sf::st_buffer(buffer_m) |>
#'     dplyr::select(-dplyr::any_of(c("join_field", "SHAPE_Leng", "SHAPE_Area", "ONS_Name", "ONS_Region")))
#'
#'   point_data <- sf::st_transform(point_data, crs=32189)
#'
#'   raw_counts <- sf::st_join(point_data, region_shp) |>
#'     sf::st_drop_geometry() |>
#'     dplyr::group_by(!!rlang::sym(region_id_col)) |>
#'     dplyr::summarise(num_region_plus_buffer = dplyr::n(), .groups = "drop")
#'
#'   # create overall measure too for id_col values = 0
#'   # find points within the union of the buffered areas, count rows
#'   raw_count_overall <- sf::st_filter(point_data, sf::st_as_sf(sf::st_union(region_shp))) |>
#'     sf::st_drop_geometry() |>
#'     dplyr::summarise(num_region_plus_buffer = dplyr::n()) |>
#'     dplyr::mutate({{region_id_col}} := "0", .before = 1 )
#'
#'   # get hoodwise counts
#'   result <- dplyr::full_join(sf::st_drop_geometry(region_shp), raw_counts, by = region_id_col ) |>
#'     dplyr::mutate(num_region_plus_buffer = dplyr::if_else(is.na(num_region_plus_buffer), 0, num_region_plus_buffer)) |>
#'     tidyr::drop_na(!!rlang::sym(region_id_col)) |>
#'     dplyr::arrange(!!rlang::sym(region_id_col))
#'
#'   # combine overall with hoodwise
#'   result <- dplyr::bind_rows(raw_count_overall, result)
#'
#'   # if no buffer, then do not name the column "plus_buffer"
#'   if (buffer_m == 0) {
#'     result <- dplyr::rename(result, num_region = num_region_plus_buffer)
#'   }
#'
#'   return(result)
#' }
#'
#' #' Count points per 1000 residents in regions plus a buffer
#' #'
#' #' @param region_shp Sf object with polygonal shapes, one row per region, with at
#' #' least one column providing unique region identifiers.
#' #' @param point_data Sf object with point data to aggregate to regions.
#' #' @param pop_data Data frame giving regional population data, including at least
#' #' one column with unique region identifiers and one column with population data.
#' #' @param region_id_col Character, column name in `region_shp` containing unique
#' #' region identifiers.
#' #' @param pop_col character, column name in `pop_data` containing populations.
#' #' @param crs The CRS to convert regions and points to. Defaults to 32189
#' #' (NAD83 / MTM zone 9) for Ottawa, Ontario.
#' #' @param buffer_m Buffer in meters to apply to regions.
#' #'
#' #' @return A tibble with one row per region and the number of points within each.
#' #' @export
#' get_number_per_region_per_1000_residents_plus_buffer <- function(region_shp, point_data, pop_data, region_id_col = "ONS_ID", pop_col = "SF_TotalPop", crs = 32189, buffer_m = 50) {
#'
#'   # add ottawa-wide population value to pop_data if we're taking
#'   # input from package `neighbourhoodstudy`
#'   #region_shp <- ons_gen3_shp
#'   #point_data <- foodspace_grocery
#'   #pop_data <- neighbourhoodstudy::ons_gen3_pop2021
#'
#'   if ( region_id_col == "ONS_ID" && ! "0" %in% pop_data$ONS_ID) {
#'     pop_ott <- sum(pop_data[,pop_col, drop = TRUE])
#'     pop_data <- dplyr::add_row(pop_data, ONS_ID = "0", ONS_Name = "OTTAWA",  {{pop_col}} := pop_ott, .before = 1)
#'   }
#'
#'
#'   # first we get the numbers per region, including ottawa-wide
#'   num_per_region <- get_number_per_region_plus_buffer(region_shp, point_data, region_id_col, crs, buffer_m)
#'
#'   # then divide by population
#'   result <- dplyr::left_join(num_per_region, pop_data, by = region_id_col) |>
#'     dplyr::mutate(num_per_1000_res_plus_buffer = num_region_plus_buffer / SF_TotalPop * 1000) |>
#'     dplyr::select(dplyr::all_of(region_id_col), num_per_1000_res_plus_buffer)
#'
#'   # methodology choice: give NA for any division by zero, could arguably give 0 instead
#'   result <- result |>
#'     #dplyr::arrange(dplyr::desc(num_per_1000)) |> View()
#'     tidyr::drop_na(!!rlang::sym(region_id_col)) |>
#'     dplyr::mutate(num_per_1000_res_plus_buffer = dplyr::if_else(is.na(num_per_1000_res_plus_buffer) | is.infinite(num_per_1000_res_plus_buffer) | is.nan(num_per_1000_res_plus_buffer), NA, num_per_1000_res_plus_buffer))
#'
#'   return(result)
#' }
#'
#'
#'
#'
#' #' Use a pre-computed origin-destination table to get ONS Neighbourhood-level average distances from DB centroids to closest 3 features
#' #'
#' #' @param od_table tibble output from `valhallr::od_table()` with columns `distance` and `time`
#' #' @param from_id_col Character, name of column with unique origin identifiers. Default "DBUID".
#' #' @param to_id_col Character, name of column with unique destination identifiers.
#' #' @param froms_to_ons_sli Single-link indicator (SLI) from origins to ONS neighbourhoods. Consider using `neighbourhoodstudy::sli_das_gen3_mape`.
#' #' @param dbpops Tibble, in the format of (or consider using) `neighbourhoodstudy::ottawa_dbs_pop2021`.
#' #'
#' #' @return Tibble with average distance to 3 closest features at ONS neighbourhood and Ottawa levels.
#' #' @export
#' get_avg_dist_to_closest_three <- function(od_table, from_id_col = "DBUID", to_id_col, froms_to_ons_sli, dbpops) {
#'
#'   # take the od_table, get top 3 closest by distance for each origin,
#'   # average them, get populations for DBUIDs, convert DBUIDs to DAUIDs, use
#'   # single-link indicator to map DAs to ONS hoods, pop-weight avg distance from
#'   # DAs up to gen3 hoods
#'   # first get db-level results, so we can do hoods and citywide
#'   result_dbs <-  tidyr::drop_na(od_table) |>
#'     dplyr::group_by(!!rlang::sym(from_id_col)) |>
#'     dplyr::arrange(distance) |>
#'     dplyr::slice_head(n=3) |>
#'     dplyr::summarise(dist_closest_3 = mean(distance, na.rm = TRUE)) |>
#'     dplyr::left_join(dbpops, by = "DBUID") |>
#'     dplyr::mutate(DAUID = substr(DBUID, 1, 8))
#'
#'   # hood results
#'   result_hoods <- result_dbs |>
#'     dplyr::left_join(froms_to_ons_sli, by = "DAUID") |>
#'     dplyr::select(DAUID, dist_closest_3, dbpop2021, ONS_ID) |>
#'     dplyr::group_by(ONS_ID) |>
#'     dplyr::summarise(dist_closest_3_popwt = sum((dist_closest_3 * dbpop2021) / sum(dbpop2021) ))
#'
#'   # citywide
#'   result_ott <- result_dbs |>
#'     dplyr::summarise(dist_closest_3_popwt = sum((dist_closest_3 * dbpop2021) / sum(dbpop2021) )) |>
#'     dplyr::mutate(ONS_ID = "0", .before = 1)
#'
#'   result <- dplyr::bind_rows(result_ott, result_hoods)
#'
#'   return(result)
#' }
#'
#'
#' # take the od_table, find out if DBs have ANY trips under 15 minutes,
#' # add DB pops, link DBs to DAs, link DAs to hoods using sli, use db pops and
#' # whether they're covered or not to estimate % of hood pop within 15 minutes
#'
#' #' Use precomputed distance tables to get the % of residents within 15-minute walk of features
#' #'
#' #' @param od_table tibble output from `valhallr::od_table()` with columns `distance` and `time`
#' #' @param from_id_col Character, name of column with unique origin identifiers. Default "DBUID".
#' #' @param to_id_col Character, name of column with unique destination identifiers.
#' #' @param froms_to_ons_sli Single-link indicator (SLI) from origins to ONS neighbourhoods. Consider using `neighbourhoodstudy::sli_das_gen3_mape`.
#' #' @param dbpops Tibble, in the format of (or consider using) `neighbourhoodstudy::ottawa_dbs_pop2021`.
#' #'
#' #' @return Tibble with % of residents within 15-minute walk at ONS neighbourhood and Ottawa levels.
#' #' @export
#' get_pct_within_15_mins <- function(od_table, from_id_col = "DBUID", to_id_col, froms_to_ons_sli, dbpops) {
#'
#'   # take the od_table, find out if DBs have ANY trips under 15 minutes,
#'   # add DB pops, link DBs to DAs, link DAs to hoods using sli, use db pops and
#'   # whether they're covered or not to estimate % of hood pop within 15 minutes
#'   result_db <-  tidyr::drop_na(od_table) |>
#'     dplyr::group_by(!!rlang::sym(from_id_col)) |>
#'     dplyr::summarise(covered = any (time < 15 * 60)) |>
#'     dplyr::left_join(dbpops, by = "DBUID") |>
#'     dplyr::mutate(DAUID = substr(DBUID, 1, 8))
#'
#'   result_hood <- result_db |>
#'     dplyr::left_join(froms_to_ons_sli, by = "DAUID") |>
#'     dplyr::select(DAUID, covered, dbpop2021, ONS_ID) |>
#'     dplyr::group_by(ONS_ID) |>
#'     dplyr::summarise(pct_within_15_mins = sum((covered * dbpop2021) / sum(dbpop2021) ))
#'
#'   result_ott <- result_db |>
#'     dplyr::summarise(pct_within_15_mins = sum((covered * dbpop2021) / sum(dbpop2021) ) )|>
#'     dplyr::mutate(ONS_ID = "0", .before = 1)
#'
#'   result <- dplyr::bind_rows(result_ott, result_hood)
#'
#'   return(result)
#' }
#'



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



compute_and_save_stats <- function(prefix, foodspace_data, foodspace_distances, ons_gen3_shp, pop_data, single_link_indicator, dbpops ) {

  # to make it recompute
  nudge <- 1

  # remove errant items
  foodspace_distances <- dplyr::filter(foodspace_distances, !is.na(DBUID))

  # get the four values of interest
  num_per_region <- neighbourhoodstudy::get_number_per_region_plus_buffer(ons_gen3_shp, foodspace_data, buffer_m=0)
  num_per_1000_res_plus_buffer <- neighbourhoodstudy::get_number_per_region_per_1000_residents_plus_buffer(region_shp = ons_gen3_shp, point_data = foodspace_data, pop_data = neighbourhoodstudy::ons_gen3_pop2021, region_id_col = "ONS_ID", pop_col = "SF_TotalPop")
  avg_dist_closest_3 <- neighbourhoodstudy::get_avg_dist_to_closest_three(foodspace_distances, from_id_col = "DBUID", to_id_col = "osm_id", froms_to_ons_sli = neighbourhoodstudy::sli_das_gen3_mape, dbpops = neighbourhoodstudy::ottawa_dbs_pop2021)
  pct_within_15_minutes <- neighbourhoodstudy::get_pct_within_15_mins(od_table = foodspace_distances, from_id_col = "DBUID", to_id_col = "osm_id", froms_to_ons_sli = neighbourhoodstudy::sli_das_gen3_mape, dbpops = neighbourhoodstudy::ottawa_dbs_pop2021)

  # combine and tidy
  results_tidy <- dplyr::left_join(num_per_region, num_per_1000_res_plus_buffer, by = "ONS_ID") |>
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

  readr::write_csv(results_tidy, paste0(output_dir,"/", prefix, "tidy_", Sys.Date(), ".csv"))

  readr::write_csv(results_wide, paste0(output_dir,"/", prefix, "wide_", Sys.Date(), ".csv"))

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


# finally save all the stats into one csv file for easier loading
# and to finish our targets workflow in a nice single node :)
save_consolidated_food_stats <- function(foodspace_stats_convenience,
                                         foodspace_stats_fastfood,
                                         foodspace_stats_grocery,
                                         foodspace_stats_restaurant,
                                         foodspace_stats_specialty){

  prefix <- "food_all_"

  results_tidy <- foodspace_stats_convenience |>
    dplyr::left_join(foodspace_stats_fastfood, by = "ONS_ID") |>
    dplyr::left_join(foodspace_stats_grocery, by = "ONS_ID") |>
    dplyr::left_join(foodspace_stats_restaurant, by = "ONS_ID") |>
    dplyr::left_join(foodspace_stats_specialty, by = "ONS_ID")

  results_wide <- results_tidy |>
    tidyr::pivot_longer(names_to = "variable", values_to = "value", cols = -ONS_ID) |>
    tidyr::pivot_wider(values_from = value, names_from = ONS_ID)

  # save to file
  datestamp <- Sys.Date()
  if (!dir.exists(paste0("output/", datestamp))) dir.create(paste0("output/", datestamp))

  output_dir <- paste0("output/", datestamp,"/hood_stats")
  if (!dir.exists(output_dir)) dir.create(output_dir)

  readr::write_csv(results_tidy, paste0(output_dir,"/", prefix, "tidy_", Sys.Date(), ".csv"))

  readr::write_csv(results_wide, paste0(output_dir,"/", prefix, "wide_", Sys.Date(), ".csv"))

  return (TRUE)
}
