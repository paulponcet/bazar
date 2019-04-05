context("nlist")

test_that("'nlist' is a named list", {
  expect_true(is_nlist(nlist()) && 
                is_nlist(nlist(x = 1, y = c(2,3))) && 
                is_nlist(as_nlist(list(x = 1, y = c(2,3)))))
})
