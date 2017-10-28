context("id")

test_that("environment can be hashed", {
  expect_silent(compute_id(as.environment(list(x = 1))))
})

test_that("assignment can be hashed", {
  expect_silent(compute_id(substitute(x <- 1)))
})
