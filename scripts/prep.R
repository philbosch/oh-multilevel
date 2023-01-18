### import data ###
library(tidyverse)
library(sf)
library(vroom)
library(janitor)


precincts <- sf::st_read(dsn = "data/shapefiles/oh_vest_16/")
tiger_10 <- sf::st_read(dsn = "data/shapefiles/tiger_10/")
tiger_20 <- sf::st_read(dsn = "data/shapefiles/tiger_20/")


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





### quick glimpse if black/two races sheds light on it

