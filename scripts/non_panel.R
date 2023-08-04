### packages
library(dplyr)
library(stringr)
library(sf)
library(tigris)

options(tigris_class = "sf")

### read in data


# Read data
oh_2012 <- vroom::vroom(file = "data/raw_polling_places/oh_2012_geocoded.tsv")

# Ensure your data frame's coordinates are in a suitable format for sf
oh_2012_geocoded_sf <- sf::st_as_sf(oh_2012, coords = c("long", "lat"), crs = 4326)

# Fetch the boundaries for Ohio
ohio <- tigris::states() %>%
  dplyr::filter(STUSPS == "OH")

# Transform data frame to the same CRS as the Ohio boundaries
oh_2012_geocoded_sf <- sf::st_transform(oh_2012_geocoded_sf, sf::st_crs(ohio))

# Check if points are within Ohio
in_ohio <- sf::st_within(oh_2012_geocoded_sf, ohio)

sf::sf_use_s2(FALSE)


# Add results back to the data frame
oh_2012_geocoded_sf <- sf::st_drop_geometry(oh_2012_geocoded_sf) %>%
  dplyr::mutate(In_Ohio = lengths(in_ohio) > 0,
                lon = sf::st_coordinates(oh_2012_geocoded_sf)[,1],
                lat = sf::st_coordinates(oh_2012_geocoded_sf)[,2])

# Create a data frame with the correct coordinates
correct_coords_df <- tibble::tibble(
  name = c("ST MARYS CHURCH HALL", "SHARON WOODS TRAINING CENTER", "ALVA N SIDLE AMERICAN LEGION POST"),
  correct_longitude = c(-81.2295872435207, -84.40322339195049, -83.84391309239182),
  correct_latitude = c(39.906476723022465, 39.27799137521471, 41.42958722192429)
)

# Merge the correct_coords_df with your main dataframe
oh_2012_geocoded_sf <- dplyr::left_join(oh_2012_geocoded_sf, correct_coords_df, by = "name")

# Update the longitude and latitude values for the specified locations
oh_2012_geocoded_sf <- oh_2012_geocoded_sf %>%
  dplyr::mutate(
    lon = ifelse(In_Ohio == FALSE & !is.na(correct_longitude), correct_longitude, lon),
    lat = ifelse(In_Ohio == FALSE & !is.na(correct_latitude), correct_latitude, lat)
  ) %>%
  dplyr::select(-correct_longitude, -correct_latitude)

# Convert back to sf object
oh_2012_geocoded_sf <- sf::st_as_sf(oh_2012_geocoded_sf, coords = c("lon", "lat"), crs = 6548)


# find spatial duplicates

# Group by geometry and count
same_geometry <- oh_2012_geocoded_sf %>%
  group_by(geometry) %>%
  summarise(count = n(), .groups = 'drop') %>%
  dplyr::arrange(dplyr::desc(count))



# prepare 2016 data

oh_2016 <- vroom::vroom(file = "data/raw_polling_places/oh_2016_geocoded.tsv")


# Make sure your data frame's coordinates are suitable for sf
oh_2016_geocoded_sf <- sf::st_as_sf(oh_2016, coords = c("long", "lat"), crs = "NAD83")

# Fetch the boundaries for Ohio
ohio <- tigris::states() %>%
  dplyr::filter(STUSPS == "OH")

# Transform data frame to the same CRS as the Ohio boundaries
oh_2016_geocoded_sf <- sf::st_transform(oh_2016_geocoded_sf, sf::st_crs(ohio))

sf::sf_use_s2(TRUE)


# Check if points are within Ohio
in_ohio <- sf::st_within(oh_2016_geocoded_sf, ohio)

sf::sf_use_s2(FALSE)

# Add results back to the data frame, creating lat and lon columns
oh_2016_geocoded_sf <- sf::st_drop_geometry(oh_2016_geocoded_sf) %>%
  dplyr::mutate(In_Ohio = lengths(in_ohio) > 0,
                lon = sf::st_coordinates(oh_2016_geocoded_sf)[,1],
                lat = sf::st_coordinates(oh_2016_geocoded_sf)[,2])

# Create a data frame with the correct coordinates (placeholder)
correct_coords_df <- tibble::tibble(
  name = c("ST MARYS CHURCH HALL"),
  correct_longitude = c(-81.2295872435207),
  correct_latitude = c(39.906476723022465)
)

# Merge the correct_coords_df with your main dataframe
oh_2016_geocoded_sf <- dplyr::left_join(oh_2016_geocoded_sf, correct_coords_df, by = "name")

# Update the longitude and latitude values for the specified locations
oh_2016_geocoded_sf <- oh_2016_geocoded_sf %>%
  dplyr::mutate(
    lon = ifelse(In_Ohio == FALSE & !is.na(correct_longitude), correct_longitude, lon),
    lat = ifelse(In_Ohio == FALSE & !is.na(correct_latitude), correct_latitude, lat)
  ) %>%
  dplyr::select(-correct_longitude, -correct_latitude)

# Convert back to sf object
oh_2016_geocoded_sf <- sf::st_as_sf(oh_2016_geocoded_sf, coords = c("lon", "lat"), crs = "NAD83")



# neighbourhood statistics for each polling place


crsuggest::suggest_crs(oh_2012_geocoded_sf)
crsuggest::suggest_crs(geo_acs_2016)

geo_acs_2016 %>%
  sf::st_transform(6548) -> geo_acs_2016

oh_2012_geocoded_sf %>%
  sf::st_transform(6548) -> oh_2012_geocoded_sf


oh_2012_geocoded_sf %>%
  sf::st_join(geo_acs_2016,
              suffix = c("_points", "_acs_tracts")) -> points_blocks_df



points_blocks_df %>%
  slice_head(n = 10) %>%
  mapview::mapview()


# for 2016
oh_2016_geocoded_sf %>%
  sf::st_transform(6548) -> oh_2016_geocoded_sf

oh_2016_geocoded_sf %>%
  mapview::mapview()

# save the data as shapefile
sf::st_write(oh_2012_geocoded_sf, "data/clean/2012/oh_2012_geocoded_sf.shp")
sf::st_write(oh_2016_geocoded_sf, "data/clean/2016/oh_2016_geocoded_sf.shp")
sf::st_write(geo_acs_2016, "data/clean/acs/geo_acs_2016.geojson")


##### Start of analysis

library(dplyr)
library(sf)

# Define the distance buffer
buffer_distance <- 5  # 5 meters

# Now we define a function that will identify polling places that have moved
find_moved_polling_places <- function(distance) {
  # We create a buffer around each 2012 point
  oh_2012_buffered <- st_buffer(oh_2012_geocoded_sf, dist = distance)

  # We perform a spatial join between 2012 (buffered) and 2016 data.
  # If a 2016 point falls within a 2012 buffer, it's considered the same polling place
  same_place <- st_join(oh_2012_buffered, oh_2016_geocoded_sf, join = st_intersects)

  # We remove rows where the join was successful, leaving only the "moved" places
  moved_polling_places <- same_place %>%
    filter(is.na(state.y))  # 'state.y' comes from the 2016 data

  return(moved_polling_places)
}

# Now we can easily find the moved polling places for any distance
moved_polling_places <- find_moved_polling_places(5)

# print the moved polling places
any(duplicated(oh_2012_geocoded_sf$precinct_id))

# Create a list of moved polling places ids
moved_ids <- list(moved_polling_places$precinct_id.x)

# Add the dummy variable to the original 2012 dataset using tidyverse style
oh_2012_geocoded_sf <- oh_2012_geocoded_sf %>%
  mutate(was_moved = as.integer(precinct_id %in% moved_ids))

### merge with social data
### probably create buffer (blocks around)
### probably also change in population

### additionally 2016: only precinct boarders and tiger lines ()

