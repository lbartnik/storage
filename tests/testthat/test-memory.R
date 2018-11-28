context("memory")

test_that("write to store", {
  mm <- helper_empty_memory()

  res <- os_write(mm, iris, list(tag = 'value'), id = as_id('abcdef'))
  expect_true(exists('abcdef', envir = mm, inherits = FALSE))
  expect_equal(toString(res), 'abcdef')
})


test_that("update tags", {
  mm <- helper_sample_memory()

  id <- compute_id(1L)
  expect_true(os_exists(mm, id))
  expect_equal(os_read_tags(mm, id), list(tag = 'a'))

  expect_equal(os_update_tags(mm, id, list(tag = 'x')), id)
  expect_equal(os_read_tags(mm, id), list(tag = 'x'))
})


test_that("read from store", {
  mm <- helper_empty_memory()
  i <- as_id('abcdef')

  res <- os_write(mm, iris, list(tag = 'value'), id = i)
  expect_equal(toString(res), 'abcdef')

  res <- os_read(mm, i)
  expect_named(res, c('object', 'tags'))
  expect_equal(res$object, iris)
  expect_equal(res$tags, list(tag = 'value'))

  res <- os_read_object(mm, i)
  expect_equal(res, iris)

  res <- os_read_tags(mm, i)
  expect_equal(res, list(tag = 'value'))
})


test_that("object exists", {
  mm <- helper_empty_memory()

  expect_length(os_exists(mm, as_id(character(0))), 0)
  expect_false(os_exists(mm, as_id('')))

  i <- as_id('abcdef')
  expect_false(os_exists(mm, i))

  os_write(mm, iris, list(tag = 'value'), id = i)
  expect_true(os_exists(mm, i))
})


test_that("list objects", {
  mm <- helper_sample_memory()

  res <- os_list(mm)
  expect_length(res, 10)
})


test_that("find objects", {
  mm <- helper_sample_memory()

  # single object
  res <- os_find(mm, list(rlang::quo(tag == 'a')))
  expect_length(res, 1)

  obj <- os_read_object(mm, res)
  expect_equal(obj, 1)

  # multiple objects
  res <- os_find(mm, list(rlang::quo(tag %in% c('a', 'b', 'c'))))
  expect_length(res, 3)

  obj <- vapply(res, function (id) os_read_object(mm, as_id(id)), numeric(1))
  expect_length(obj, 3)
  expect_true(setequal(obj, 1:3))
})


test_that("find in empty", {
  mm <- helper_empty_memory()

  res <- os_find(mm, list(rlang::quo(x == 1)))
  expect_length(res, 0)
})


test_that("cannot evaluate", {
  mm <- helper_sample_memory()

  expect_silent(res <- os_find(mm, list(rlang::quo(no_such_tag == 'a'))))
  expect_length(res, 0)
})
