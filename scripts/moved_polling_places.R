find_moved_polling_places <- function(oh_2012_geocoded_sf, oh_2016_geocoded_sf, distance) {
  # Create a buffer around each 2012 point
  oh_2012_buffered <- st_buffer(oh_2012_geocoded_sf, dist = distance)

  # Perform a spatial join between 2012 (buffered) and 2016 data.
  # If a 2016 point falls within a 2012 buffer, it's considered the same polling place
  same_place <- st_join(oh_2012_buffered, oh_2016_geocoded_sf, join = st_intersects)

  # Remove rows where the join was successful, leaving only the "moved" places
  # Note: Adjust 'state.y' based on actual column names in your 2016 data
  moved_polling_places <- same_place %>%
    filter(is.na(state.y))  # Assuming 'state' is a common field; adjust as necessary

  return(moved_polling_places)
}
