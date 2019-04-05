context("is_formula")

test_that("'is_formula' does the job", {
  expect_true(!is_formula(4) &&
                is_formula(~ x + y) && 
                is_formula(z ~ x + y))
})
