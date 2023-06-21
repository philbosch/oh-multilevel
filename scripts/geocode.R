### packages
library(dplyr)
library(stringr)

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
