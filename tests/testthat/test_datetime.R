context("date time functions")
test_that("fiscal_quarter is the fiscal quarter for the specified fiscal start year", {
  expect_equal(fiscal_quarter(as.Date("2019-12-01")), 2019.4) #Fourth quarter when fiscal_start =1
  expect_equal(fiscal_quarter(as.Date("2019-12-01"),fiscal_start = 7), 2020.2) #Australian fiscal year start

})
