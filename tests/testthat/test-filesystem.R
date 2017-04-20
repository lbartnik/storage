context("filesystem")

test_that("store can be created and removed", {
  path <- file.path(tempdir(), "filesystem")
  on.exit(unlink(path, recursive = TRUE, force = TRUE))

  expect_error(filesystem(path, create = FALSE))

  fs <- filesystem(path, create = TRUE)
  expect_true(dir.exists(path))

  os_remove(fs)
  expect_false(dir.exists(path))
})


test_that("write to store", {
  fs <- helper_empty_filesystem()
  on.exit(helper_rm_rf(fs))

  res <- os_write(fs, iris, list(tag = 'value'), id = 'abcdef')
  expect_true(file.exists(file.path(fs, 'ab', 'cd', 'abcdef.rds')))
  expect_true(file.exists(file.path(fs, 'ab', 'cd', 'abcdef_tags.rds')))
  expect_equal(res, 'abcdef')
})


test_that("read from store", {
  fs <- helper_empty_filesystem()
  on.exit(helper_rm_rf(fs))

  res <- os_write(fs, iris, list(tag = 'value'), id = 'abcdef')
  expect_equal(res, 'abcdef')

  res <- os_read(fs, 'abcdef')
  expect_named(res, c('object', 'tags'))
  expect_equal(res$object, iris)
  expect_equal(res$tags, list(tag = 'value'))

  res <- os_read_object(fs, 'abcdef')
  expect_equal(res, iris)

  res <- os_read_tags(fs, 'abcdef')
  expect_equal(res, list(tag = 'value'))
})


test_that("object exists", {
  fs <- helper_empty_filesystem()
  on.exit(helper_rm_rf(fs))

  expect_length(os_exists(fs, character(0)), 0)
  expect_false(os_exists(fs, ''))
  expect_false(os_exists(fs, 'abcdef'))

  os_write(fs, iris, list(tag = 'value'), id = 'abcdef')
  expect_true(os_exists(fs, 'abcdef'))
})


test_that("list objects", {
  fs <- helper_sample_filesystem()
  on.exit(helper_rm_rf(fs))

  res <- os_list(fs)
  expect_length(res, 10)
})


test_that("find objects", {
  fs <- helper_sample_filesystem()
  on.exit(helper_rm_rf(fs))

  # single object
  res <- os_find(fs, list(lazyeval::lazy(tag == 'a')))
  expect_length(res, 1)

  obj <- os_read_object(fs, res)
  expect_equal(obj, 1)

  # multiple objects
  res <- os_find(fs, list(lazyeval::lazy(tag %in% c('a', 'b', 'c'))))
  expect_length(res, 3)

  obj <- vapply(res, function (id) os_read_object(fs, id), numeric(1))
  expect_length(obj, 3)
  expect_true(setequal(obj, 1:3))
})
