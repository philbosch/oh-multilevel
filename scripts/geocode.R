### packages
library(dplyr)
library(stringr)
library(sf)
library(tigris)

options(tigris_class = "sf")

### read in data


oh_2012 <- vroom::vroom(file = "data/raw_polling_places/Ohio_2012.csv")
oh_2016 <- vroom::vroom(file = "data/raw_polling_places/Ohio_2016.csv")


### geocode with google

oh_2012 %>%
  dplyr::filter(stringr::str_detect(address, "OH"))


# Function to check if a single address follows the expected format
check_address_format <- function(address){
  parts <- str_split(address, ",", simplify = TRUE)
  if(length(parts) != 4) return(FALSE)
  street <- str_trim(parts[1])
  city <- str_trim(parts[2])
  state <- str_trim(parts[3])
  zip <- str_trim(parts[4])

  # Check that the zip code has exactly 5 digits
  if(!str_detect(zip, "^[0-9]{5}$")) return(FALSE)

  # Here, we're assuming that the street, city, and state are at least one character long
  if(nchar(street) < 1 | nchar(city) < 1 | nchar(state) < 1) return(FALSE)

  return(TRUE)
}

# Add a new column to the dataframe, 'correct_format', to indicate if each address follows the correct format
df <- oh_2016 %>%
  mutate(correct_format = sapply(address, check_address_format))

# Display rows with incorrect format
df_incorrect_format <- df[which(df$correct_format == FALSE),]




### geocode


oh_2012_geocoded <- oh_2012 %>%
  tidygeocoder::geocode(address = address, method = "google", verbose = TRUE)

oh_2016_geocoded <- oh_2016 %>%
  tidygeocoder::geocode(address = address, method = "google", verbose = TRUE)

# save data

vroom::vroom_write(oh_2012_geocoded, file = "data/raw_polling_places/oh_2012_geocoded.tsv")
vroom::vroom_write(oh_2016_geocoded, file = "data/raw_polling_places/oh_2016_geocoded.tsv")

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
oh_2012_geocoded_sf <- sf::st_as_sf(oh_2012_geocoded_sf, coords = c("lon", "lat"), crs = 4326)




# Make sure your data frame's coordinates are suitable for sf
oh_2016_geocoded_sf <- sf::st_as_sf(oh_2016, coords = c("long", "lat"), crs = 4326)

# Fetch the boundaries for Ohio
ohio <- tigris::states() %>%
  dplyr::filter(STUSPS == "OH")

# Transform data frame to the same CRS as the Ohio boundaries
oh_2016_geocoded_sf <- sf::st_transform(oh_2016_geocoded_sf, sf::st_crs(ohio))

# Check if points are within Ohio
in_ohio <- sf::st_within(oh_2016_geocoded_sf, ohio)

# Add results back to the data frame, creating lat and lon columns
oh_2016_geocoded_sf <- sf::st_drop_geometry(oh_2016_geocoded_sf) %>%
  dplyr::mutate(In_Ohio = lengths(in_ohio) > 0,
                lon = sf::st_coordinates(oh_2016_geocoded_sf)[,1],
                lat = sf::st_coordinates(oh_2016_geocoded_sf)[,2])

# Create a data frame with the correct coordinates (placeholder)
correct_coords_df <- tibble::tibble(
  name = c("ST MARYS CHURCH HALL"),
  correct_longitude = c(39.906476723022465),
  correct_latitude = c(-81.2295872435207)
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
oh_2016_geocoded_sf <- sf::st_as_sf(oh_2016_geocoded_sf, coords = c("lon", "lat"), crs = 4326)


# Convert the geometry back to 'lat' and 'long' columns for 2012 data
oh_2012_geocoded_df <- oh_2012_geocoded_sf %>%
  dplyr::mutate(
    long = sf::st_coordinates(.)[,1],
    lat = sf::st_coordinates(.)[,2]
  ) %>%
  dplyr::select(-geometry)

# Write 2012 data to tsv file
vroom::vroom_write(oh_2012_geocoded_df, "data/raw_polling_places/oh_2012_geocoded_cleaned.tsv")

# Convert the geometry back to 'lat' and 'long' columns for 2016 data
oh_2016_geocoded_df <- oh_2016_geocoded_sf %>%
  dplyr::mutate(
    long = sf::st_coordinates(.)[,1],
    lat = sf::st_coordinates(.)[,2]
  ) %>%
  dplyr::select(-geometry)

# Write 2016 data to tsv file
vroom::vroom_write(oh_2016_geocoded_df, "data/raw_polling_places/oh_2016_geocoded_cleaned.tsv")

