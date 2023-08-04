
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
options(tigris_use_cache = TRUE)


# read data ---------------------------------------------------------------

oh_2012_geocoded_sf <- sf::st_read(dsn = "data/clean/2012/")
oh_2016_geocoded_sf <- sf::st_read(dsn = "data/clean/2016/")
geo_acs_2016 <- sf::st_read(dsn = "data/clean/acs/geo_acs_2016.geojson")

# source function
source(file = "scripts/moved_polling_places.R")

# analysis ----------------------------------------------------------------

# Define the distance buffer
buffer_distance <- 5  # 5 meters


# Now we can easily find the moved polling places for any distance
moved_polling_places <- find_moved_polling_places(5)

# print the moved polling places
any(duplicated(oh_2012_geocoded_sf$prcnct_))

# Create a list of moved polling places ids

# Add the dummy variable to the original 2012 dataset using tidyverse style
oh_2012_geocoded_sf <- oh_2012_geocoded_sf %>%
  mutate(was_moved = as.integer(prcnct_ %in% unlist(moved_ids)))

### merge with social data
### probably create buffer (blocks around)
### probably also change in population

### additionally 2016: only precinct boarders and tiger lines ()

county_shp <- tigris::counties("OH", year = 2016) %>%
  sf::st_transform(crs = 6548) %>%
  dplyr::select(id_county = GEOID, county_name = NAMELSAD, geometry)



oh_2012_geocoded_sf %>%
  sf::st_join(county_shp) -> oh_2012_geocoded_sf



oh_2012_geocoded_sf %>%
  sf::st_join(geo_acs_2016,
              suffix = c("_points", "_acs_tracts")) -> points_blocks_df

sf::st_drop_geometry(points_blocks_df) -> points_blocks_df_no_geo

# analysis 2 ----------------------------------------------------------------

points_blocks_df %>%
  janitor::tabyl(was_moved)


points_blocks_df %>%
  group_by(county_name) %>%
  summarize(count = n()) %>%
  arrange(count)


points_blocks_df_no_geo %>%
  group_by(county_name) %>%
  summarize(closing_rate = mean(was_moved),
            non_insured = weighted.mean(bg_pct_uninsured, w = bg_population)) %>%
  arrange(desc(non_insured))


  ggplot(aes(x = closing_rate)) +
  geom_histogram(color = "white")


stan_glmer(
  was_moved ~ bg_pct_nonwhite + (1|county_name),
  data = points_blocks_df_no_geo, family = binomial,
  prior_intercept = normal(0, 2.5, autoscale = TRUE),
  prior = normal(0, 2.5, autoscale = TRUE),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 5000*2, seed = 84735, cores = 4) -> closing_model


tidy(closing_model, effects = "fixed", conf.int = TRUE, conf.level = 0.70)
