context("id")

test_that("environment can be hashed", {
  expect_silent(compute_id(as.environment(list(x = 1))))
})

test_that("assignment can be hashed", {
  expect_silent(compute_id(substitute(x <- 1)))
})

test_that("as_id", {
  expect_s3_class(as_id(random_str(40)), "identifier")

  expect_error(as_id(random_str(1)))
  expect_error(as_id(random_str(39)))
  expect_error(as_id(random_str(41)))
})

test_that("toString for identifier", {
  i <- compute_id(iris)

  expect_equal(nchar(toString(i)), 8L)
  expect_equal(nchar(toString(long(i))), 40L)
})

test_that("print for identifier", {
  s <- random_str(40)
  i <- as_id(s)

  expect_output(print(i), substr(s, 1, 8))
  expect_output(print(long(i)), s)
})

test_that("match short identifier", {
  m <- helper_empty_memory()
  i <- compute_id(iris)
  os_write(m, iris, list(), i)

  j <- toString(i)
  expect_equal(nchar(j), 8)

  expect_equal(match_short(j, m), i)
})
