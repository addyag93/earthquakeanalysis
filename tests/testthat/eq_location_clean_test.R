context('clean location data')
eq_data <- earthquake
test_that('missing variables throw an error', {
  
  expect_error(eq_location_clean(eq_data %>% dplyr::select(-COUNTRY)),
               'Missing required variable: COUNTRY')
  
  expect_error(eq_location_clean(eq_data %>% dplyr::select(-LOCATION_NAME)),
               'Missing required variable: LOCATION_NAME')
})