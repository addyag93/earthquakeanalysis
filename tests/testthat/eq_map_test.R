context('test map is created')

test_that('map is created ', {
  eq_data <- eq_load_data()
  
  eq_data <- eq_data %>%
    dplyr::filter(COUNTRY == 'UK') %>%
    dplyr::filter(lubridate::year(DATE) >= 2000)
  
  map <- eq_map(eq_data, annot_col = 'DATE')
  
  expect_s3_class(map, 'leaflet')
  expect_s3_class(map, 'htmlwidget')
})