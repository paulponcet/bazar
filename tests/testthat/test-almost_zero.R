context("almost_zero")

test_that("'almost_zero' works", {
  expect_identical(almost_zero(c(0, 10^(-7), 10^(-8))), 
                   c(TRUE, FALSE, TRUE))
})
