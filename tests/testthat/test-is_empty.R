context("is_empty")

test_that("'is_empty' does the job", {
  expect_true(!is_empty(4) &&
                is_empty(c()) && 
                is_empty(character(0)) && 
                is_empty(list()) &&
                is_empty(integer(0)) &&
                is_empty(data.frame()))
})
