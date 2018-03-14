context('geom timiline label  is working properly')

test_that('geomtimeline label is working fine', {
  
  eq_data <- eq_load_data()
  eq_plot <- eq_data %>%
    dplyr::filter(COUNTRY %in% c("USA", "INDIA")) %>%
    dplyr::filter(DATE > '2000-01-01') %>%
    ggplot2::ggplot()+
    geom_timeline_label(ggplot2::aes(x = DATE, y = COUNTRY, label = LOCATION_NAME,
                                     magnitude = EQ_PRIMARY))
  
  expect_s3_class(eq_plot, 'ggplot')
  
  eq_layer_data <- ggplot2::layer_data(eq_plot)
  expect_equal(sum(eq_layer_data$group == eq_layer_data$y), nrow(eq_layer_data))
  
})