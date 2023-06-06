## testing overpass API call programmatically. it works using the web site

library(osmdata)
library(dplyr)
library(neighbourhoodstudy)
library(sf)

ons_gen3_shp <- dplyr::filter(neighbourhoodstudy::ons_gen3_shp, ONS_Region == "OTTAWA")
ons_gen3_buffer <- ons_gen3_shp |>
  sf::st_transform(ons_gen3_shp, crs=32189) |>
  sf::st_union() |>
  sf::st_buffer(10000) |>
  sf::st_transform(crs="WGS84")

osm_query <- osmdata::opq(bbox = sf::st_bbox(ons_gen3_buffer), timeout = 10000)

amenities <- c("restaurant",
               "pub",
               "bar",
               "cafe",
               "fast_food",
               "food_court" )

shops <- c("grocery",
           "convenience",
           "deli",
           "greengrocer",
           "food",
           "general",
           "supermarket")

shops_query <- paste0("\"shop\"=\"", shops, "\"" )

amenities_query <- paste0("\"amenity\"=\"", amenities, "\"" )


#bboxes <- make_buffer_grid(ons_gen3_buffer, side_length=2)

#osm_query <- osmdata::opq(bbox = bboxes[[1]], timeout = 10000)
osm_query <- osmdata::add_osm_features(osm_query, features = c(shops_query, amenities_query))

#osm_query <- osmdata::add_osm_features(osm_query, features = amenities_query)

osm_query
#for (amenity in amenities) osm_query <- osmdata::add_osm_feature(osm_query, key = "amenity", value = amenity)

#for (shop in shops) osm_query <- osmdata::add_osm_feature(osm_query, key = "shop", value = shop)

overpass_data <- osmdata::osmdata_sf(osm_query, quiet = FALSE)

save(overpass_data, file = paste0("data/overpass_response_raw-",Sys.Date(),".Rdata"))

osm_pts <- overpass_data$osm_points
osm_polys <- overpass_data$osm_polygons

osm_pts <- osm_pts |>
  dplyr::as_tibble() |>
  sf::st_as_sf() |>
  dplyr::mutate(type = dplyr::if_else(is.na(amenity), shop, amenity)) |>
  dplyr::select(osm_id, type, name, shop, amenity, brand, addr.street, addr.housenumber, addr.city, addr.postcode) |>
  dplyr::filter(!is.na(type))#|>
  #sf::st_transform(crs = "WGS84")

osm_poly_centroids <- osm_polys |>
  dplyr::as_tibble() |>
  sf::st_as_sf() |>
  dplyr::mutate(type = dplyr::if_else(is.na(amenity), shop, amenity)) |>
  dplyr::select(osm_id, type, name, shop, amenity, brand, addr.street, addr.housenumber, addr.city, addr.postcode) |>
  dplyr::filter(!is.na(type)) |>
  sf::st_centroid()#|>
  #sf::st_transform(crs = "WGS84")


ottawa_food <- dplyr::bind_rows(osm_pts, osm_poly_centroids) |>
  sf::st_transform(crs = "WGS84")

sf::write_sf(ottawa_food, paste0("data/ottawa-osm-food-", Sys.Date(), ".shp"))


ottawa_food <- sf::read_sf("data/ottawa-osm-food-2023-05-20.shp")

ottawa_food %>%
  dplyr::bind_cols(., sf::st_coordinates(.)) |>
  sf::st_drop_geometry() |>
  dplyr::rename(lat = Y, lon = X) |>
  readr::write_csv(paste0("data/ottawa-osm-food-", Sys.Date(), ".csv"))

ottawa_food |>
  ggplot() + geom_sf(data=ons_gen3_shp) + geom_sf(aes(colour=type))
# #sf::st_centroid(food) |>
# #sf::st_filter(ons_gen3_shp)
#
# colnames(osm_pts)
#
# test$overpass_call
#
# osm_query
#
# ##
# test <- osmdata::opq(bbox = sf::st_bbox(neighbourhoodstudy::ons_gen3_shp)) |>
#   osmdata::add_osm_feature(key = "shop", value = "supermarket")
#
# testdata <- test |>
#   osmdata::osmdata_sf(quiet=FALSE)
#
# testdata$osm_polygons
#
# test$osm_points
#
#
#
# ###### TESTING OVERPASS API
#
#
#
#
# sp::plot(cway_sev$osm_lines)


sf::st_crs(ottawa_food)

sf::st_crs(ons_gen3_shp)




bounding_box <- sf::st_bbox(ons_gen3_buffer)

# if our bounding box is too big, split it into smaller ones
make_buffer_grid <- function(bounding_box, side_length=2) {

  bboxes <- list()
  xmin_orig <- bounding_box$xmin
  ymin_orig <- bounding_box$ymin
  xmax_orig <- bounding_box$xmax
  ymax_orig <- bounding_box$ymax

  xlength <- xmax_orig - xmin_orig
  ylength <- ymax_orig - ymin_orig

  for (i in 1:side_length){

    for (j in 1:side_length){

      xmin <- xmin_orig + (i-1) * xlength
      xmax <- xmin_orig + i * xlength
      ymin <- ymin_orig + (j-1) * ylength
      ymax <- ymin_orig + j * ylength

      bbox <- c(xmin, ymin, xmax, ymax)
      names(bbox) <- c("xmin", "ymin", "xmax", "ymax")

    bboxes[[(i-1) * side_length + j]] <- bbox

    }
  }

bboxes
}
