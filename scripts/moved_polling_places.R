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
