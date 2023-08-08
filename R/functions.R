

# build and execute an Overpass API query using an input buffer and list of amenities and shops
query_osm_api <- function(bounding_box_shp, amenities= "", shops = "", process = TRUE, drop_unnamed_elements = TRUE) {

  if (shops == "" & amenities == "") stop ("Must provide character vector of shop or amenity key-values.")

  osm_query <- osmdata::opq(bbox = sf::st_bbox(bounding_box_shp), timeout = 10000)

  if (shops != ""){
    shops_query <- paste0("\"shop\"=\"", shops, "\"" )
  }  else {
    shops_query = NULL
  }

  if (amenities[[1]] != "") {
    amenities_query <- paste0("\"amenity\"=\"", amenities, "\"" )
  } else {
    amenities_query = NULL
  }

  osm_query <- osmdata::add_osm_features(osm_query, features = c(shops_query, amenities_query))

  overpass_data <- osmdata::osmdata_sf(osm_query, quiet = FALSE)


  # if we are not processing the data, return raw api response
  if (process == FALSE) {
    result <- overpass_data
  }

  # if we are processing data, do that here
  if (process == TRUE) {
    # extract the point and polygons. some stores shop up as only points, some
    # as only polygons, so we need both
    osm_pts <- overpass_data$osm_points
    osm_polys <- overpass_data$osm_polygons


    # define columns to select
    columns_to_select <- c("osm_id", "name", "shop", "amenity", "brand", "addr.street", "addr.housenumber", "addr.city", "addr.postcode")

    osm_pts <- osm_pts |>
      dplyr::as_tibble() |>
      sf::st_as_sf() |>
      dplyr::select(dplyr::any_of(columns_to_select))

    osm_poly_centroids <- osm_polys |>
      dplyr::as_tibble() |>
      sf::st_as_sf() |>
      dplyr::select(dplyr::any_of(columns_to_select)) |>
      sf::st_centroid()


    # combine points and polygons
    # create a new column called "type" that combines shop and amenity into one value
    result <- dplyr::bind_rows(osm_pts, osm_poly_centroids) |>
      #dplyr::mutate(type = dplyr::if_else((amenity %in% amenities),  amenity, shop), .before = 3) |>
      #dplyr::filter(!is.na(type)) |>
      sf::st_transform(crs = "WGS84")

    if (drop_unnamed_elements){
      result <- dplyr::filter(result, !is.na(name))
    }
  }

  return (result)
}


# build and execute an Overpass API query using an input buffer and list of amenities and shops
query_osm_api_food <- function(ons_gen3_buffer, amenities, shops) {

  osm_query <- osmdata::opq(bbox = sf::st_bbox(ons_gen3_buffer), timeout = 10000)

  shops_query <- paste0("\"shop\"=\"", shops, "\"" )

  amenities_query <- paste0("\"amenity\"=\"", amenities, "\"" )

  osm_query <- osmdata::add_osm_features(osm_query, features = c(shops_query, amenities_query))

  overpass_data <- osmdata::osmdata_sf(osm_query, quiet = FALSE)


  return (overpass_data)
}

# process the Overpass API results into a shapefile
# returns an sf tibble with one row per food source and geometric points
process_overpass_data <- function(overpass_data, shops, amenities){

  # extract the point and polygons. some stores shop up as only points, some
  # as only polygons, so we need both
  osm_pts <- overpass_data$osm_points
  osm_polys <- overpass_data$osm_polygons


  osm_pts <- osm_pts |>
    dplyr::as_tibble() |>
    sf::st_as_sf() |>
    dplyr::select(osm_id, name, shop, amenity, brand, addr.street, addr.housenumber, addr.city, addr.postcode)

  osm_poly_centroids <- osm_polys |>
    dplyr::as_tibble() |>
    sf::st_as_sf() |>
    dplyr::select(osm_id, name, shop, amenity, brand, addr.street, addr.housenumber, addr.city, addr.postcode) |>
    sf::st_centroid()


  # combine points and polygons
  # create a new column called "type" that combines shop and amenity into one value
  ottawa_food <- dplyr::bind_rows(osm_pts, osm_poly_centroids) |>
    dplyr::mutate(type = dplyr::if_else((amenity %in% amenities),  amenity, shop), .before = 3) |>
    dplyr::filter(!is.na(type)) |>
    sf::st_transform(crs = "WGS84")

  ottawa_food <- ottawa_food |>
    dplyr::mutate(ons_type = dplyr::case_when(
      type %in% "convenience" ~ "convenience",
      type %in% c("fast_food", "cafe", "food_court") ~ "fast_food",
      type %in% c("pub", "restaurant", "bar") ~ "restaurant",
      type %in% c("deli") ~ "specialty",
      type %in% c("supermarket", "grocery", "greengrocer", "general") ~ "grocery"
    ), .before = 3)

  return(ottawa_food)
}

# save foodspace data in shapefile and csv in datestamped folder
save_food_data <- function(ottawa_food){

  today <- Sys.Date()
  output_directory <- paste0("output/",today)

  if (!dir.exists("output")) dir.create("output")
  if (!dir.exists(output_directory)) dir.create(output_directory)

  sf::write_sf(ottawa_food, paste0(output_directory, "/ottawa-osm-food-", Sys.Date(), ".shp"))


  dplyr::bind_cols(ottawa_food, sf::st_coordinates(ottawa_food)) |>
    sf::st_drop_geometry() |>
    dplyr::rename(lat = Y, lon = X) |>
    readr::write_csv(paste0(output_directory, "/ottawa-osm-food-", Sys.Date(), ".csv"))

  return(TRUE)
}


# # given ons neighbourhoods and point data, return number per hood.
# get_number_per_hood <- function(ons_gen3_shp, point_data) {
#
#   point_data <- ottawa_food
#
#   sf::st_join(ons_gen3_shp, point_data) |>
#     sf::st_drop_geometry() |>
#     dplyr::group_by(ONS_ID, ONS_Name) |>
#     dplyr::count()

# }
#
# # given ons neighbourhoods and point data, return number per hood plus 50m buffer.
# get_number_per_hood <- function(ons_gen3_shp, point_data) {
#
#   ons_gen3_shp <- sf::st_transform(ons_gen3_shp, crs=32189) |>
#     sf::st_buffer(50)
#
#   point_data <- sf::st_transform(point_data, crs=32189)
#
#   sf::st_join(ons_gen3_shp, point_data) |>
#     sf::st_drop_geometry() |>
#     dplyr::group_by(ONS_ID, ONS_Name) |>
#     dplyr::count(name = "num")
#
# }


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



get_pct_residents_within_traveltime_valhalla_isochrones <- function(point_data, pop_point_data, db_pop_data, db_ons_sli, ons_pop_data, point_data_id_col, pop_col, travel_time_minutes = 15, travel_time_mode = "pedestrian", hostname = "localhost", port= 8002) {

  pop_point_data <- neighbourhoodstudy::ottawa_phhs
  point_data <- foodspace_grocery
  db_pop_data <- neighbourhoodstudy::ottawa_dbs_pop2021
  db_ons_sli <- neighbourhoodstudy::sli_dbs_gen3_maxoverlap
  ons_pop_data <- neighbourhoodstudy::ons_gen3_pop2021

  point_data_latlon <- sf::st_coordinates(point_data) |>
    dplyr::as_tibble() |>
    dplyr::rename(lat = Y, lon = X)

  point_data_split <- point_data |>
    sf::st_drop_geometry() |>
    dplyr::bind_cols(point_data_latlon) |>
    dplyr::select(dplyr::all_of(point_data_idcol), lat, lon) |>
    #head(5) |>
    dplyr::mutate(.rowid_for_split = 1:nrow(point_data)) |>
    split.data.frame(f =  ~.rowid_for_split) #|>    head(5)


  isochrones <- purrr::map_dfr(point_data_split, valhallr::isochrone, costing=travel_time_mode, contours=travel_time_minutes, hostname = hostname, port = port) |>
    sf::st_make_valid()

  isochrones_union <- sf::st_union(isochrones) |>
    sf::st_make_valid()

  phhs_covered <- sf::st_filter(pop_point_data, isochrones_union)


  ## WORKING ON THIS PART
  stop("fix this")
  residents_covered <- phhs_covered |>
    #sf::st_join(ons_gen3_shp) |>
    sf::st_drop_geometry() |>
    dplyr::group_by(DBUID) |>
    dplyr::summarise(residents_covered = sum(dbpop), .groups = "drop")

  stop ("nothing should map to ONS ID 3006, it has zero population!")
  pct_residents_covered <-
    dplyr::left_join(ons_db_sli, residents_covered, by = "DBUID") |>
    dplyr::group_by(ONS_ID) |>
    dplyr::summarise(residents_covered = sum(residents_covered, na.rm=TRUE)) |>
    dplyr::left_join(ons_pop_data, by = "ONS_ID") |>
    dplyr::mutate(pct_residents_covered = residents_covered / SF_TotalPop * 100) |>
    dplyr::mutate(pct_residents_covered = dplyr::if_else(is.infinite(pct_residents_covered) | is.nan(pct_residents_covered), 0, pct_residents_covered)) |>
    dplyr::mutate(pct_residents_covered = round(pct_residents_covered, digits = 2)) |>
    dplyr::mutate(pct_residents_covered = dplyr::if_else(pct_residents_covered > 100, 100, pct_residents_covered))

  result <- pct_residents_covered |>
    dplyr::mutate(ons_type = foodtype) |>
    dplyr::select(ONS_ID, ONS_Name, ons_type, pct_residents_covered )

  results <- dplyr::bind_rows(results, result)

}
#
# # given ons neighbourhoods and point data, return number per 1000 people plus 50m buffer.
# # population comes in tibble with column `SF_TotalPop`
# get_number_per_1000 <- function(ons_gen3_shp, ons_gen3_pop2021, point_data) {
#
#   ons_gen3_shp <- sf::st_transform(ons_gen3_shp, crs=32189) |>
#     sf::st_buffer(50)
#
#   point_data <- sf::st_transform(point_data, crs=32189)
#
#   sf::st_join(ons_gen3_shp, point_data) |>
#     sf::st_drop_geometry() |>
#     dplyr::group_by(ONS_ID, ONS_Name) |>
#     dplyr::count() |>
#     dplyr::left_join(ons_gen3_pop2021, by = c("ONS_ID", "ONS_Name")) |>
#     dplyr::mutate(num_per_1000 = n/SF_TotalPop * 1000 ) |>
#     dplyr::mutate(num_per_1000 = dplyr::if_else(is.infinite(num_per_1000), NA, num_per_1000)) |>
#     dplyr::select(ONS_ID, ONS_Name, num_per_1000)
#
# }

#get_number_per_hood(ons_gen3_shp, ottawa_food)
#get_number_per_1000(ons_gen3_shp, ons_gen3_pop2021, ottawa_food)


phh_walkdistance <- function(ottawa_phhs, ons_gen3_shp, ottawa_food, ons_db_sli){

  ons_db_sli <- neighbourhoodstudy::sli_dbs_gen3_maxoverlap
  ons_phhs <- sf::st_transform(ottawa_phhs, crs = "WGS84")
  ons_gen3_shp <- sf::st_transform(ons_gen3_shp, crs = "WGS84")
  ottawa_food <- sf::st_transform(ottawa_food, crs = "WGS84")

  latlon <- sf::st_coordinates(ottawa_food) |>
    dplyr::as_tibble() |>
    dplyr::rename(lat=Y,lon=X)

  ottawa_food <- ottawa_food |>
    sf::st_drop_geometry() |>
    dplyr::bind_cols(latlon)

  results <- dplyr::tibble()

  for (foodtype in unique(ottawa_food$ons_type)) {
    message(foodtype)

    foods <- dplyr::filter(ottawa_food, ons_type == foodtype)

    foods <- split(foods, foods$osm_id)

    isochrones <- purrr::map_dfr(foods, valhallr::isochrone, costing="pedestrian", contours=15) |>
      sf::st_make_valid()

    isochrones_union <- sf::st_union(isochrones) |>
      sf::st_make_valid()

    phhs_covered <- sf::st_filter(ons_phhs, isochrones_union)

    residents_covered <- phhs_covered |>
      #sf::st_join(ons_gen3_shp) |>
      sf::st_drop_geometry() |>
      dplyr::group_by(DBUID) |>
      dplyr::summarise(residents_covered = sum(dbpop), .groups = "drop")

    pct_residents_covered <-
      dplyr::left_join(ons_db_sli, residents_covered, by = "DBUID") |>
      dplyr::group_by(ONS_ID) |>
      dplyr::summarise(residents_covered = sum(residents_covered, na.rm=TRUE)) |>
      dplyr::left_join(ons_gen3_pop2021, by = "ONS_ID") |>
      dplyr::mutate(pct_residents_covered = residents_covered / SF_TotalPop * 100) |>
      dplyr::mutate(pct_residents_covered = dplyr::if_else(is.infinite(pct_residents_covered) | is.nan(pct_residents_covered), 0, pct_residents_covered)) |>
      dplyr::mutate(pct_residents_covered = round(pct_residents_covered, digits = 2)) |>
      dplyr::mutate(pct_residents_covered = dplyr::if_else(pct_residents_covered > 100, 100, pct_residents_covered))

    result <- pct_residents_covered |>
      dplyr::mutate(ons_type = foodtype) |>
      dplyr::select(ONS_ID, ONS_Name, ons_type, pct_residents_covered )

    results <- dplyr::bind_rows(results, result)
  }

  return (results)
}


# input cleaning and then run valhalla anlaysis
phh_distance_analysis <- function(ottawa_phhs, ons_gen3_shp, destinations, destination_id_col){

  stop("this doesn't work properly")
  ons_gen3_shp <- sf::st_transform(ons_gen3_shp, crs=32189) |>
    sf::st_buffer(50) |>
    sf::st_transform(crs="WGS84")

  ottawa_phhs <- neighbourhoodstudy::ottawa_phhs |>
    sf::st_filter(ons_gen3_shp)


  destinations_latlon <- dplyr::as_tibble(sf::st_coordinates(destinations)) |>
    dplyr::rename(lat=Y, lon=X)

  destinations <- destinations |>
    dplyr::bind_cols(destinations_latlon) |>
    sf::st_filter(ons_gen3_shp)

  #destination_id_col <- "osm_id"

  from_latlon <- dplyr::as_tibble(sf::st_coordinates(ottawa_phhs)) |>
    dplyr::rename(lat=Y, lon=X)

  froms <- dplyr::select(ottawa_phhs, phh_id) |>
    dplyr::bind_cols(from_latlon)

  results <- dplyr::tibble()

  for (i in 1:nrow(froms)){
    message(i,"/",nrow(froms))
    from <- froms[i,]
    tos <- dplyr::tibble()
    buffer_size <- 0

    # get some closeby places but not all of them
    while (nrow(tos) < 5) {
      #    message(buffer_size)
      buffer_size <- buffer_size + 500
      tos <- sf::st_filter(destinations, sf::st_transform(sf::st_buffer(sf::st_transform(from, crs=32189), buffer_size),crs="WGS84")) |>
        sf::st_drop_geometry() |>
        dplyr::select(dplyr::all_of(destination_id_col), lat, lon)
    }

    from$geometry <- NULL
    tos$geometry <- NULL

    result <- valhallr::od_table(froms = from, from_id_col = "phh_id", tos = tos, to_id_col = destination_id_col, verbose = TRUE, batch_size = 100)

    results <- dplyr::bind_rows(results, result)

  }


  to_latlon <- dplyr::as_tibble(sf::st_coordinates(destinations)) |>
    dplyr::rename(lat=Y, lon=X)

  tos <- destinations |>
    sf::st_drop_geometry() |>
    dplyr::select(dplyr::all_of(destination_id_col)) |>
    dplyr::bind_cols(to_latlon)

  froms1 <- froms #head(froms, n=100)
  tos1 <- head(tos, n = 200)

  valhallr::route(from = froms[1,], to = tos[1,])
  result <- valhallr::od_table(froms = froms1, from_id_col = "phh_id", tos = tos1, to_id_col = destination_id_col, verbose = TRUE, batch_size = 1)
  #result <- valhallr::od_table(froms = tos, from_id_col = destination_id_col, tos = froms, to_id_col = "phh_id", verbose = TRUE, batch_size = 5)
  return(result)
}




## CREATE PHHS FOR ONS NEIGHBOURHOODS USING STATSCAN NEIGHBOURHOOD POPULATIONS

create_ons_phhs <- function(ons_gen3_shp, ons_gen3_pop2021, ottawa_roads_shp) {

  future::plan(future::multisession)

  ottawa_roads_shp <- pseudohouseholds::ottawa_roads_shp

  ons_shp <- sf::st_transform(ons_gen3_shp, crs=32189) |>
    dplyr::left_join(ons_gen3_pop2021, by = c("ONS_ID", "ONS_Name")) |>
    dplyr::select(ONS_ID, ONS_Name, SF_TotalPop)

  result <- pseudohouseholds::get_phhs_parallel(regions = ons_shp,region_idcol = "ONS_ID", roads =  ottawa_roads_shp, roads_idcol = "NGD_UID" ,region_popcol = "SF_TotalPop")

  future::plan(future::sequential)
  #ggplot(result) + geom_sf(aes(colour=ONS_ID)) + theme(legend.position = "none")

  pseudohouseholds::validate_phhs(result, ons_shp, region_idcol = "ONS_ID", region_popcol = "SF_TotalPop")

  return(result)
}




# for each phh, return the 5 nearest by straight-line so that we can calculate
# road distance to them to find 3 closest
get_destination_candidates <- function(ottawa_phhs, ons_gen3_shp, destinations, destination_id_col){

  destinations <- sf::st_filter(destinations, ons_gen3_shp)
  destination_id_col <- "osm_id"


  ons_gen3_shp <- sf::st_transform(ons_gen3_shp, crs=32189) |>
    sf::st_buffer(50) |>
    sf::st_transform(crs="WGS84")

  ottawa_phhs <- neighbourhoodstudy::ottawa_phhs |>
    sf::st_filter(ons_gen3_shp)


  destinations_latlon <- dplyr::as_tibble(sf::st_coordinates(destinations)) |>
    dplyr::rename(lat=Y, lon=X)

  destinations <- destinations |>
    dplyr::bind_cols(destinations_latlon) |>
    sf::st_filter(ons_gen3_shp)

  #destination_id_col <- "osm_id"

  from_latlon <- dplyr::as_tibble(sf::st_coordinates(ottawa_phhs)) |>
    dplyr::rename(lat=Y, lon=X)

  froms <- dplyr::select(ottawa_phhs, phh_id) |>
    dplyr::bind_cols(from_latlon)


  # we will use a function to map through each food type

  destinations_list <- split(destinations, destinations$ons_type)

  #x <- destinations_list[5]

  # set up parallel processing
  future::plan(future::multisession, workers = 5)

  results <- furrr::future_map(destinations_list, function(x) {

    tictoc::tic()
    distances <- sf::st_distance(froms, x)
    tictoc::toc()

    dist_df <- dplyr::as_tibble(distances)

    colnames(dist_df) <- x$osm_id

    tictoc::tic()
    dist_df_long <- dist_df |>
      dplyr::mutate(phh_id = froms$phh_id, .before = 1) |>
      tidyr::pivot_longer(cols = -phh_id, names_to = "osm_id", values_to = "dist_m")
    tictoc::toc()

    tictoc::tic()
    dist_df_long <- dist_df_long |>
      dplyr::group_by(phh_id) |>
      dplyr::arrange(dist_m) |>
      dplyr::slice_head(n=5)
    tictoc::toc()

    return(dist_df_long)
  }, .options = furrr::furrr_options(seed = TRUE))

  future::plan(future::sequential)

  return (results)
}


get_candidate_distances <- function(phh_destination_candidates, ottawa_food){

  ottawa_phhs <- neighbourhoodstudy::ottawa_phhs

  ottawa_food_latlon <- ottawa_food |>
    dplyr::bind_cols(sf::st_coordinates(ottawa_food)) |>
    dplyr::select(osm_id, lat=Y, lon=X) |>
    sf::st_drop_geometry()


  ottawa_phhs_latlon <- ottawa_phhs |>
    dplyr::bind_cols(sf::st_coordinates(ottawa_phhs)) |>
    dplyr::select(phh_id, lat=Y, lon=X) |>
    sf::st_drop_geometry()

  purrr::map(phh_destination_candidates, function(x) {

    x |>
      dplyr::ungroup() |>
      #dplyr::slice_head(n=5000) |>

      tibble::rowid_to_column() |>
      dplyr::mutate(tranche_num = rowid %/% 1000) |>
      tidyr::nest(tranche = -tranche_num) |>
      dplyr::mutate(result = purrr::map(tranche, function(tranche) {

        # tranche <- phh_destination_candidates[[1]] |>
        #   dplyr::ungroup() |>
        #   dplyr::slice_head(n=1000)

        phhs <- dplyr::select(tranche, phh_id) |>
          dplyr::distinct() |>
          dplyr::left_join(ottawa_phhs_latlon, by = "phh_id")

        osms <- dplyr::select(tranche, osm_id) |>
          dplyr::distinct() |>
          dplyr::left_join(ottawa_food_latlon, by = "osm_id")

        tictoc::tic()
        result <- valhallr::od_table(froms = phhs, from_id_col = "phh_id", tos = osms, to_id_col = "osm_id")
        tictoc::toc()
        return(result)
      })) # end purrr::map(tranche)
  }) # end purrr::map(phh_destination_candidates)
} # end function get_candidate_distances

# THIS STUFF IS PART OF ROAD NETWORK ANALYSIS!!
#   for (i in 1:nrow(froms)){
#     message(i,"/",nrow(froms))
#     from <- froms[i,]
#     tos <- dplyr::tibble()
#     buffer_size <- 0
#
#     # get some closeby places but not all of them
#     while (nrow(tos) < 5) {
#       #    message(buffer_size)
#       buffer_size <- buffer_size + 500
#       tos <- sf::st_filter(destinations, sf::st_transform(sf::st_buffer(sf::st_transform(from, crs=32189), buffer_size),crs="WGS84")) |>
#         sf::st_drop_geometry() |>
#         dplyr::select(dplyr::all_of(destination_id_col), lat, lon)
#     }
#
#     from$geometry <- NULL
#     tos$geometry <- NULL
#
#     result <- valhallr::od_table(froms = from, from_id_col = "phh_id", tos = tos, to_id_col = destination_id_col, verbose = TRUE, batch_size = 100)
#
#     results <- dplyr::bind_rows(results, result)
#
#   }
#
#
#   to_latlon <- dplyr::as_tibble(sf::st_coordinates(destinations)) |>
#     dplyr::rename(lat=Y, lon=X)
#
#   tos <- destinations |>
#     sf::st_drop_geometry() |>
#     dplyr::select(dplyr::all_of(destination_id_col)) |>
#     dplyr::bind_cols(to_latlon)
#
#   froms1 <- froms #head(froms, n=100)
#   tos1 <- head(tos, n = 200)
#
#   valhallr::route(from = froms[1,], to = tos[1,])
#   result <- valhallr::od_table(froms = froms1, from_id_col = "phh_id", tos = tos1, to_id_col = destination_id_col, verbose = TRUE, batch_size = 1)
#   #result <- valhallr::od_table(froms = tos, from_id_col = destination_id_col, tos = froms, to_id_col = "phh_id", verbose = TRUE, batch_size = 5)
#   return(result)
# }




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

  # od_table <- foodspace_specialty_distances
  # tos <- foodspace_specialty
  # to_id_col <- "osm_id"
  # froms <- ottawa_dbs_shp
  # from_id_col <- "DBUID"
  # froms_to_ons_sli <- neighbourhoodstudy::sli_das_gen3_mape
  # dbpops <- neighbourhoodstudy::ottawa_dbs_pop2021
  # ons_shp <- neighbourhoodstudy::ons_gen3_shp


  # take the od_table, get top 3 closest by distance for each origin,
  # average them, get populations for DBUIDs, convert DBUIDs to DAUIDs, use
  # single-link indicator to map DAs to ONS hoods, pop-weight avg distance from
  # DAs up to gen3 hoods
  result <-  tidyr::drop_na(od_table) |>
    dplyr::group_by(!!rlang::sym(from_id_col)) |>
    dplyr::arrange("distance") |>
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

  # od_table <- foodspace_specialty_distances
  # to_id_col <- "osm_id"
  # from_id_col <- "DBUID"
  # froms_to_ons_sli <- neighbourhoodstudy::sli_das_gen3_mape
  # dbpops <- neighbourhoodstudy::ottawa_dbs_pop2021

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
 # prefix <- "food_specialty_"

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
  output_dir <- paste0("output/", datestamp)
  if (!dir.exists(output_dir)) dir.create(output_dir)

  readr::write_csv(results_tidy, paste0(output_dir,"/", prefix, "_tidy_", Sys.Date(), ".csv"))

  readr::write_csv(results_wide, paste0(output_dir,"/", prefix, "_wide_", Sys.Date(), ".csv"))

  return(results_tidy)
}
