context('test popup labels created correctly')

test_that('popup labels create correctly', {
  eq_data <- eq_load_data()
  cols_eq <- ncol(eq_data)
  
  eq_data <- eq_data %>%
    dplyr::filter(COUNTRY == 'US') %>%
    dplyr::filter(lubridate::year(DATE) >= 2000) %>%
    dplyr::mutate(popup_text = eq_create_label(.))
  
  expect_is(eq_data$popup_text, 'character')
  expect_equal(cols_eq, ncol(eq_data) - 1)
})