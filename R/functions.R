
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
process_overpass_data <- function(overpass_data){

  # extract the point and polygons. some stores shop up as only points, some
  # as only polygons, so we need both
  osm_pts <- overpass_data$osm_points
  osm_polys <- overpass_data$osm_polygons

  # we create a new column called "type" that combines shop and amenity into one value
  osm_pts <- osm_pts |>
    dplyr::as_tibble() |>
    sf::st_as_sf() |>
    dplyr::mutate(type = dplyr::if_else(is.na(amenity), shop, amenity)) |>
    dplyr::select(osm_id, type, name, shop, amenity, brand, addr.street, addr.housenumber, addr.city, addr.postcode) |>
    dplyr::filter(!is.na(type))

  osm_poly_centroids <- osm_polys |>
    dplyr::as_tibble() |>
    sf::st_as_sf() |>
    dplyr::mutate(type = dplyr::if_else(is.na(amenity), shop, amenity)) |>
    dplyr::select(osm_id, type, name, shop, amenity, brand, addr.street, addr.housenumber, addr.city, addr.postcode) |>
    dplyr::filter(!is.na(type)) |>
    sf::st_centroid()


  ottawa_food <- dplyr::bind_rows(osm_pts, osm_poly_centroids) |>
    sf::st_transform(crs = "WGS84")

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
