### import data ###
library(sf)
library(vroom)
library(janitor)
library(dplyr)
library(tidycensus)
library(data.table)
options(tigris_use_cache = TRUE)



# read in precinct boundaries
precincts <- sf::st_read(dsn = "data/shapefiles/oh_vest_16/")

# read in polling places

oh_2012 <- vroom::vroom(file = "data/raw_polling_places/oh_2012_geocoded_cleaned.tsv")
oh_2016 <- vroom::vroom(file = "data/raw_polling_places/oh_2016_geocoded_cleaned.tsv")




# df_2010 <- vroom(file = "data/blocks/DECENNIALPL2010.P3-Data.csv", col_select = c(GEO_ID, NAME, P003001, P003003, P003004, P003005, P003006, P003007, P003008)) %>%
#   janitor::row_to_names(1) %>%
#   janitor::clean_names()
#
#
# df_2010 %>%
#   mutate(across(.cols = !c(1,2), .fns = as.numeric)) %>%
#   dplyr::filter(total > 0) %>%
#   mutate(share_white = total_population_of_one_race_white_alone/total) -> df_2010



# df_2010 %>%
#   names() %>%
#   str_extract(pattern = "(?<=total_population_of_one_race_).*$") -> clean_names
#
# clean_names[c(1:3, 10)] <- names(df_2010)[c(1:3, 10)]
#
# names(df_2010) <- clean_names
#
# df_2010 %>%
#   mutate(non_white =  black_or_african_american_alone + american_indian_and_alaska_native_alone + asian_alone + native_hawaiian_and_other_pacific_islander_alone + some_other_race_alone) %>%
#   mutate(missing_race = total - (non_white + white_alone)) %>%
#   filter(missing_race > 0) %>%
#   mutate(missing_race_share = missing_race / total) %>%
#   select(total, missing_race, missing_race_share) %>%
#   arrange(desc(missing_race_share))

### calculate share of non-white per block 2010
### calculate share of non-white per block 2020



# 2020 block level data ---------------------------------------------------

# df_2020 <- vroom(file = "data/blocks/DECENNIALPL2020.P3-Data.csv", col_select = c(GEO_ID, NAME, P3_001N, P3_003N, P3_004N, P3_005N, P3_006N, P3_007N, P3_008N)) %>%
#   janitor::row_to_names(1) %>%
#   janitor::clean_names()
#
#
# df_2020 %>%
#   mutate(across(.cols = !c(1,2), .fns = as.numeric)) %>%
#   dplyr::filter(total > 0) %>%
#   mutate(share_white = total_population_of_one_race_white_alone/total) -> df_2020
#
# df_2020 %>%
#   names() %>%
#   str_extract(pattern = "(?<=total_population_of_one_race_).*$") -> clean_names_2020
#
# clean_names_2020[c(1:3, 10)] <- names(df_2020)[c(1:3, 10)]



### quick glimpse if black/two races sheds light on it



# spatial join  -----------------------------------------------------------

# df_2010 %>%
#   dplyr::mutate(geoid10 = str_extract(string = geography, pattern = "(?<=US).*$")) -> df_2010
#
# tiger_10 %>%
#   right_join(df_2010, by = c("geoid10" = "geoid10")) -> geo_df_2010
#
# class(geo_df_2010)
# class(tiger_10)


# endprodukt: für jeden precinct kontextvariablen (jeweils 2010 und 2020)
# dazu: für jeden block die zahlen, checken zu welchem precinct ein block gehört
# aggregation auf precinct ebene
# checks in between: gibt es grosse ausreisser?
# point data der polling places -> merge to precinct data (einmal für 2012 und einmal für 2016)


# TODO: export to arcgis -> check there https://github.com/r-spatial/sf/issues/1902

acs_data_2016 <- clean_acs_data(acs_vars_file = "data/acs_vars.csv", year = 2016)


# Save acs_data to file

vroom::vroom_write(x = acs_data_2016, file = "data/acs_oh_5_year.tsv")

demo_2016 <- get_acs(
  state = "OH",
  geography = "cbg",
  variables = c(total_pop = "B03002_001"),
  geometry = TRUE,
  year = 2016,
) %>%
  dplyr::select(GEOID, NAME, geometry)


geo_acs_2016 <- demo_2016 %>%
  mutate(GEOID = as.numeric(GEOID)) %>%
  dplyr::right_join(acs_data_2016, by = join_by(GEOID == GEOID))





### approach with Google Data
# convert oh_2016 to sf object
oh_2016_sf <- st_as_sf(oh_2016, coords = c("lat", "long"), crs = 4269)

sf::sf_use_s2(FALSE)

# transform the crs to NAD83
oh_2012_geocoded_sf <- sf::st_transform(oh_2012_geocoded_sf, crs = "NAD83")

# Transform to UTM Zone 17N
precincts <- st_transform(precincts, 26917)
polling_places_2012 <- st_transform(oh_2012_geocoded_sf, 26917)
polling_places_2016 <- st_transform(oh_2016_geocoded_sf, 26917)


# join both datasets
joined_data <- st_join(polling_places_2016, precincts, join = st_intersects)

joined_data_long <- st_join(precincts, polling_places_2012, join = st_intersects, left = TRUE)


# count polling places in each precinct
count_polling_places <- joined_data %>%
  group_by(GEOID16) %>%
  summarise(n_polling_places = n_distinct(name), .groups = "drop")


# Plot precinct boundaries
p <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = joined_data, color = "red", alpha = 0.5) +
  ggplot2::theme_minimal() +
  ggplot2::coord_sf()

# Add polling places
p <- p + ggplot2::geom_sf(data = precincts, color = "black", alpha = 0.5)

# Show plot
print(p)

# Filter for rows where there is no match
no_match_precincts <- joined_data_long %>%
  filter(is.na(name))

# Generate the plot
ggplot() +
  geom_sf(data = no_match_precincts) +
  labs(title = "Precincts with No Matched Polling Place",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()


# Filter for rows where there is a match
match_precincts <- joined_data_long %>%
  filter(!is.na(name))

# Generate the plot
ggplot() +
  geom_sf(data = match_precincts) +
  labs(title = "Precincts with Matched Polling Place",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()

match_precincts %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(precinct_id) %>%
  dplyr::summarise(n_polling_places = n()) %>%
  dplyr::arrange(desc(n_polling_places))



################ Approach without panel

# Assume your data are in a simple feature object named polling_places_2012
# Transform to Ohio-specific CRS (e.g., UTM Zone 17N)

# Calculate pairwise distances between all points
distances <- st_distance(polling_places_2012)

# Create a matrix where entry [i, j] is TRUE if points i and j are within 10 meters
close_points <- distances < units::set_units(10, meters)
# We only care about points above the diagonal of this matrix
# (since the matrix is symmetric and the diagonal is self-distances)
close_points[lower.tri(close_points)] <- NA

# A point is a duplicate if there is any TRUE in its row
duplicates <- rowSums(close_points, na.rm = TRUE) > 0

# This will give you a logical vector, where TRUE indicates a duplicate.
# Be aware that this approach considers one of each pair of duplicates as a
