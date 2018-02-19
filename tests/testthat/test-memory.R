context("memory")

test_that("write to store", {
  mm <- helper_empty_memory()

  res <- os_write(mm, iris, list(tag = 'value'), id = 'abcdef')
  expect_true(exists('abcdef', envir = mm, inherits = FALSE))
  expect_equal(res, 'abcdef')
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

  res <- os_write(mm, iris, list(tag = 'value'), id = 'abcdef')
  expect_equal(res, 'abcdef')

  res <- os_read(mm, 'abcdef')
  expect_named(res, c('object', 'tags'))
  expect_equal(res$object, iris)
  expect_equal(res$tags, list(tag = 'value'))

  res <- os_read_object(mm, 'abcdef')
  expect_equal(res, iris)

  res <- os_read_tags(mm, 'abcdef')
  expect_equal(res, list(tag = 'value'))
})


test_that("object exists", {
  mm <- helper_empty_memory()

  expect_length(os_exists(mm, character(0)), 0)
  expect_false(os_exists(mm, ''))
  expect_false(os_exists(mm, 'abcdef'))

  os_write(mm, iris, list(tag = 'value'), id = 'abcdef')
  expect_true(os_exists(mm, 'abcdef'))
})


test_that("list objects", {
  mm <- helper_sample_memory()

  res <- os_list(mm)
  expect_length(res, 10)
})


test_that("find objects", {
  mm <- helper_sample_memory()

  # single object
  res <- os_find(mm, list(lazyeval::lazy(tag == 'a')))
  expect_length(res, 1)

  obj <- os_read_object(mm, res)
  expect_equal(obj, 1)

  # multiple objects
  res <- os_find(mm, list(lazyeval::lazy(tag %in% c('a', 'b', 'c'))))
  expect_length(res, 3)

  obj <- vapply(res, function (id) os_read_object(mm, id), numeric(1))
  expect_length(obj, 3)
  expect_true(setequal(obj, 1:3))
})


test_that("cannot evaluate", {
  mm <- helper_sample_memory()

  expect_silent(res <- os_find(mm, list(lazyeval::lazy(no_such_tag == 'a'))))
  expect_length(res, 0)
})
