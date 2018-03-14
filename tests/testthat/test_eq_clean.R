context('clean data')

# text eq_clean_data() function 
# load data from package
eq_data <- earthquake

test_that('data is  correct', {
  
  eq_clean <- eq_clean_data(eq_data)
  
  expect_equal(ncol(eq_data) + 1, ncol(eq_clean))
  expect_equal(nrow(eq_data), nrow(eq_clean))
  expect_is(eq_clean$DATE, 'Date')
})