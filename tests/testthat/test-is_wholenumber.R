context("is_wholenumber")

test_that("'is_wholenumber' does the job", {
  expect_true(is_wholenumber(c(1L, 10L)) &&
                is_wholenumber(c(1, 10)) && 
                !is_wholenumber(1+10^(-7)) && 
                is_wholenumber(1+10^(-8)))
})
