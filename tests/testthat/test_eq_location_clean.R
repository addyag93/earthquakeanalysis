context('clean location data')
eq_data <- earthquake
test_that('basic location name cleaning function working fine', {
  eq_clean_data <- eq_location_clean(eq_data)
  
  expect_is(eq_clean_data$LOCATION_NAME, 'character')
  
  expect_equal(eq_clean_data$LOCATION_NAME,
               eq_clean_data$LOCATION_NAME %>% 
                 stringr::str_to_lower() %>%
                 stringr::str_to_title())
  
})