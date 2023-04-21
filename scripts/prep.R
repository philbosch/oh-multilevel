### import data ###
library(tidyverse)
library(sf)
library(vroom)
library(janitor)


precincts <- sf::st_read(dsn = "data/shapefiles/oh_vest_16/")

tiger_10 <- sf::st_read(dsn = "data/shapefiles/tiger_10/") %>%
  janitor::clean_names()

tiger_20 <- sf::st_read(dsn = "data/shapefiles/tiger_20/")%>%
  janitor::clean_names()

unique_polling_places <- vroom("data/unique_polling_places.csv")

unique_polling_places %>%
  select(state, election_year, address, lat, long, county_name) %>%
  filter(state == "OH", election_year == 2012 | election_year == 2016) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4269) -> geo_polls



df_2010 <- vroom(file = "data/blocks/DECENNIALPL2010.P3-Data.csv", col_select = c(GEO_ID, NAME, P003001, P003003, P003004, P003005, P003006, P003007, P003008)) %>%
  janitor::row_to_names(1) %>%
  janitor::clean_names()


df_2010 %>%
  mutate(across(.cols = !c(1,2), .fns = as.numeric)) %>%
  dplyr::filter(total > 0) %>%
  mutate(share_white = total_population_of_one_race_white_alone/total) -> df_2010



df_2010 %>%
  names() %>%
  str_extract(pattern = "(?<=total_population_of_one_race_).*$") -> clean_names

clean_names[c(1:3, 10)] <- names(df_2010)[c(1:3, 10)]

names(df_2010) <- clean_names

df_2010 %>%
  mutate(non_white =  black_or_african_american_alone + american_indian_and_alaska_native_alone + asian_alone + native_hawaiian_and_other_pacific_islander_alone + some_other_race_alone) %>%
  mutate(missing_race = total - (non_white + white_alone)) %>%
  filter(missing_race > 0) %>%
  mutate(missing_race_share = missing_race / total) %>%
  select(total, missing_race, missing_race_share) %>%
  arrange(desc(missing_race_share))

### calculate share of non-white per block 2010
### calculate share of non-white per block 2020



# 2020 block level data ---------------------------------------------------

df_2020 <- vroom(file = "data/blocks/DECENNIALPL2020.P3-Data.csv", col_select = c(GEO_ID, NAME, P3_001N, P3_003N, P3_004N, P3_005N, P3_006N, P3_007N, P3_008N)) %>%
  janitor::row_to_names(1) %>%
  janitor::clean_names()


df_2020 %>%
  mutate(across(.cols = !c(1,2), .fns = as.numeric)) %>%
  dplyr::filter(total > 0) %>%
  mutate(share_white = total_population_of_one_race_white_alone/total) -> df_2020

df_2020 %>%
  names() %>%
  str_extract(pattern = "(?<=total_population_of_one_race_).*$") -> clean_names_2020

clean_names_2020[c(1:3, 10)] <- names(df_2020)[c(1:3, 10)]



### quick glimpse if black/two races sheds light on it



# spatial join  -----------------------------------------------------------

df_2010 %>%
  dplyr::mutate(geoid10 = str_extract(string = geography, pattern = "(?<=US).*$")) -> df_2010

tiger_10 %>%
  right_join(df_2010, by = c("geoid10" = "geoid10")) -> geo_df_2010

class(geo_df_2010)
class(tiger_10)


# endprodukt: für jeden precinct kontextvariablen (jeweils 2010 und 2020)
# dazu: für jeden block die zahlen, checken zu welchem precinct ein block gehört
# aggregation auf precinct ebene
# checks in between: gibt es grosse ausreisser?
# point data der polling places -> merge to precinct data (einmal für 2012 und einmal für 2016)


# TODO: export to arcgis -> check there https://github.com/r-spatial/sf/issues/1902
sf::sf_use_s2(FALSE)

precincts %>%
  st_join(geo_polls, join = st_contains) -> joined_df

joined_df %>%
  drop_na(address) -> joined_df
