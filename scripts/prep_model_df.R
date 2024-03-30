
# load packages -----------------------------------------------------------

library(sf)
library(vroom)
library(dplyr)
library(tidycensus)
library(bayesplot)
library(rstanarm)
library(tidybayes)
library(broom.mixed)
library(janitor)
library(brms)
library(ggplot2)
library(ggspatial)
library(ggrepel)
library(scales)
library(ggokabeito)
options(tigris_use_cache = TRUE)


# read data ---------------------------------------------------------------

oh_2012_geocoded_sf <- sf::st_read(dsn = "data/clean/2012/")
oh_2016_geocoded_sf <- sf::st_read(dsn = "data/clean/2016/")
geo_acs_2016 <- sf::st_read(dsn = "data/clean/acs/geo_acs_2016.geojson")


county_spending <- readr::read_delim("data/spending_per_capita_county.csv",
                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

pop_oh_2013 <- get_acs(
  geography = "block group",
  variables = "B01003_001",
  state = "OH",
  year = 2013,
  survey = "acs5"
) %>% select(-c(variable, NAME, moe))

pop_oh_2013 %>%
  mutate(GEOID = as.numeric(GEOID)) %>%
  left_join(geo_acs_2016, by = c("GEOID" = "GEOID")) %>%
  mutate(pop_change = bg_population / estimate) %>%
  sf::st_as_sf(crs = st_crs(oh_2012_geocoded_sf)) -> geo_acs_2016

model_df_ma <- readr::read_csv("data/12_16.csv") %>%
  dplyr::filter(abbr == "OH")

# source function
source(file = "scripts/moved_polling_places.R")

# analysis ----------------------------------------------------------------

# Adjust the distance to match your criteria and data CRS units (e.g., meters if in a meter-based CRS)
moved_polling_places <- find_moved_polling_places(oh_2012_geocoded_sf, oh_2016_geocoded_sf, 10)  # For example, 50 meters

# Assuming 'oh_2012_geocoded_sf' is your original 2012 dataset
# And 'moved_polling_places' contains the polling places identified as having moved or closed

# Convert sf objects to tibbles for easier manipulation with dplyr
oh_2012_df <- st_set_geometry(oh_2012_geocoded_sf, NULL)
moved_df <- st_set_geometry(moved_polling_places, NULL)

# Create a dummy indicator for moved/closed polling places in the moved_df dataset
moved_df <- moved_df %>%
  mutate(moved_or_closed = 1) %>%
  select(prcnct_.x, moved_or_closed) %>%
  distinct() # Ensure uniqueness based on 'prcnct_.x'

# Merge this indicator back into the original 2012 dataset using 'prcnct_' as the unique identifier
oh_2012_updated_df <- oh_2012_df %>%
  left_join(moved_df, by = c("prcnct_" = "prcnct_.x")) %>%
  # Fill NA values with 0, indicating these polling places did not move/close
  mutate(moved_or_closed = tidyr::replace_na(moved_or_closed, 0))

# Optionally, convert back to an sf object if you need spatial operations
oh_2012_updated_sf <- st_as_sf(oh_2012_updated_df, geometry = st_geometry(oh_2012_geocoded_sf), crs = st_crs(oh_2012_geocoded_sf))

# 'oh_2012_updated_sf' now includes a 'moved_or_closed' dummy indicator


unique_locations_sf <- oh_2012_updated_sf %>%
  distinct(geometry, .keep_all = TRUE)

### merge with social data
### probably create buffer (blocks around)
### probably also change in population

### additionally 2016: only precinct boarders and tiger lines ()

county_shp <- tigris::counties("OH", year = 2016) %>%
  sf::st_transform(crs = 6548) %>%
  dplyr::select(id_county = GEOID, county_name = NAMELSAD, geometry)



county_spending %>%
  bind_cols(county_col = "County") %>%
  tidyr::unite(county_name,c(County_Name, county_col),sep = " ", remove = TRUE) -> county_spending




county_shp %>%
  st_drop_geometry() %>%
  left_join(county_spending, by = c("county_name" = "county_name")) -> county_spending




# join with a subset



# Define the identifiers for the specific polling places of interest
# Replace 'ID1', 'ID2', 'ID3' with the actual identifiers from your data
polling_places_ids <- c("390359805001", "390359811001", "390490093862")
next_closest_ids <- c("390351381051", "390351381072", "390490093841")


specific_polling_places <- geo_acs_2016 %>%
  filter(GEOID %in% polling_places_ids) %>%
  dplyr::select(GEOID, NAME, tract)

# Now, proceed with the distance calculation and loop as before
# (Ensure you've calculated distances between 'specific_polling_places' and 'geo_acs_2016')
distances <- st_distance(specific_polling_places, geo_acs_2016)



# Initialize a dataframe to store the closest block group for each polling place
closest_bg_second  <- data.frame(polling_place_id = character(), closest_bg_GEOID = character(), stringsAsFactors = FALSE)

# Loop through each polling place to find its closest block group
for (i in 1:nrow(distances)) {
  # Order distances from the current polling place to all block groups and get the index of the second smallest
  ordered_indices <- order(distances[i, ])
  second_closest_index <- ordered_indices[2]  # Get the second element, which is the second closest

  # Retrieve the GEOID of the second closest block group
  second_closest_bg_GEOID <- geo_acs_2016$GEOID[second_closest_index]

  # Add the information to our dataframe
  closest_bg_second <- rbind(closest_bg_second, data.frame(polling_place_id = polling_places_ids[i], second_closest_bg_GEOID = second_closest_bg_GEOID))
}

# Print or inspect 'closest_bg_second' to see the second closest block group GEOIDs for each specified polling place
print(closest_bg_second)



next_closest_pollingplaces <- geo_acs_2016 %>%
  filter(GEOID %in% next_closest_ids) %>%
  st_drop_geometry() %>%
  dplyr::select(starts_with("bg"), estimate, pop_change, starts_with("tr_"))


specific_polling_places %>%
  bind_cols(next_closest_pollingplaces) -> cleaned_specific_polling_places


geo_acs_2016 %>%
  filter(!GEOID %in% polling_places_ids) %>%
  bind_rows(cleaned_specific_polling_places) -> geo_acs_2016

geo_acs_2016 %>%
  mutate(county_id = stringr::str_extract(GEOID, "^.{5}")) %>%
  left_join(county_spending, by = c("county_id" = "id_county")) -> geo_acs_2016

geo_acs_2016 %>%
  select(GEOID, pop_change, bg_pct_nonwhite, geometry, bg_population, Total, county_name) -> subset_bg_data

unique_locations_sf %>%
  sf::st_join(subset_bg_data,
              join = st_within, left = TRUE) -> points_blocks_df



sf::st_drop_geometry(points_blocks_df) -> points_blocks_df_no_geo



# save important data ----------------------------------------------------------------

saveRDS(points_blocks_df, file = "data/replication/points_blocks_df.RDS")
readr::write_csv(points_blocks_df_no_geo, file = "data/replication/points_blocks_df_no_geo.csv")

