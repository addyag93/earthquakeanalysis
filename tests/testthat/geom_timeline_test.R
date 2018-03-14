context('geom timiline is working properly')

test_that('geomtimeline is working fine', {
  
  eq_data <- eq_load_data()
  eq_plot <- eq_data %>%
    dplyr::filter(COUNTRY %in% c("USA", "INDIA")) %>%
    dplyr::filter(DATE > '2000-01-01') %>%
    ggplot2::ggplot()+
    ggplot2::geom_timeline(x= DATE, y= COUNTRY, color= TOTAL_DEATHS, size= EQ_PRIMARY)
  
  expect_s3_class(eq_plot, 'ggplot')
  
  eq_layer_data <- layer_data(eq_plot)
  expect_equal(sum(eq_plot$group == eq_plot$y), nrow(eq_plot))
  
})