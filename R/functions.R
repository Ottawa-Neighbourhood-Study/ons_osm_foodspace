
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


# given ons neighbourhoods and point data, return number per hood.
get_number_per_hood <- function(ons_gen3_shp, point_data) {

  point_data <- ottawa_food

  sf::st_join(ons_gen3_shp, point_data) |>
    sf::st_drop_geometry() |>
    dplyr::group_by(ONS_ID, ONS_Name) |>
    dplyr::count()

}

# given ons neighbourhoods and point data, return number per hood plus 50m buffer.
get_number_per_hood <- function(ons_gen3_shp, point_data) {

  ons_gen3_shp <- sf::st_transform(ons_gen3_shp, crs=32189) |>
    sf::st_buffer(50)

  point_data <- sf::st_transform(point_data, crs=32189)

  sf::st_join(ons_gen3_shp, point_data) |>
    sf::st_drop_geometry() |>
    dplyr::group_by(ONS_ID, ONS_Name) |>
    dplyr::count(name = "num")

}


# given ons neighbourhoods and point data, return number per 1000 people plus 50m buffer.
# population comes in tibble with column `SF_TotalPop`
get_number_per_1000 <- function(ons_gen3_shp, ons_gen3_pop2021, point_data) {

  ons_gen3_shp <- sf::st_transform(ons_gen3_shp, crs=32189) |>
    sf::st_buffer(50)

  point_data <- sf::st_transform(point_data, crs=32189)

  sf::st_join(ons_gen3_shp, point_data) |>
    sf::st_drop_geometry() |>
    dplyr::group_by(ONS_ID, ONS_Name) |>
    dplyr::count() |>
    dplyr::left_join(ons_gen3_pop2021, by = c("ONS_ID", "ONS_Name")) |>
    dplyr::mutate(num_per_1000 = n/SF_TotalPop * 1000 ) |>
    dplyr::mutate(num_per_1000 = dplyr::if_else(is.infinite(num_per_1000), NA, num_per_1000)) |>
    dplyr::select(ONS_ID, ONS_Name, num_per_1000)

}

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
