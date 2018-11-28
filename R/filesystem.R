#' Filesystem-based object store.
#'
#' @inheritParams object_store
#'
#' @rdname filesystem_os
#' @name filesystem_os
NULL


#' @param path directory path; if does not exist and `create` is `TRUE`,
#'        it will be created.
#' @param create whether to create `path` if does not exist.
#'
#' @rdname filesystem_os
#' @export
filesystem <- function (path = getwd(), create = FALSE) {
  assert_dir(path, create)
  structure(path, class = c('object_store', 'filesystem'))
}


#' @rdname filesystem_os
#' @export
is_filesystem <- function (x) is_object_store(x) && inherits(x, 'filesystem')


#' @param empty_ok is an empty directory considered a valid filesystem
#'        object store.
#'
#' @rdname filesystem_os
#' @export
is_filesystem_dir <- function (path, empty_ok = FALSE)
{
  stopifnot(is.character(path), length(path) == 1)

  all_files <- list.files(path, all.files = TRUE, full.names = TRUE, recursive = TRUE)

  # there needs to be something there
  if (!length(all_files)) return(isTRUE(empty_ok))

  # all files need to be .rds
  if (!identical(length(grep('\\.rds$', all_files)), length(all_files))) return(FALSE)

  # all files need to follow the naming pattern: ab/cd/abcdxxxxxxx....rds
  all_basename <- basename(all_files)
  all_nested <- file.path(substr(all_basename, 1, 2), substr(all_basename, 3, 4), all_basename)
  if (!setequal(file.path(path, all_nested), all_files)) return(FALSE)

  # there need to be _tags.rds and .rds (data) files
  all_tags <- grep('_tags.rds', all_files, value = TRUE)
  all_data <- grep('_tags.rds', all_files, value = TRUE, invert = TRUE)
  if (!identical(length(all_tags), length(all_data))) return(FALSE)

  if (!setequal(stringi::stri_sub(all_tags, 1, -10),
                stringi::stri_sub(all_data, 1, -5)))
  {
    return(FALSE)
  }

  TRUE
}


assert_dir <- function (path, create)
{
  if (dir.exists(path)) return()

  if (isTRUE(create)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(path)) {
      stop('could not create directory ', path, call. = FALSE)
    }
  }
  else {
    stop('directory ', path, ' does not exist but `create` is FALSE',
         call. = FALSE)
  }
}


full_path <- function (store, id, ext, .create = FALSE)
{
  # parent path
  path <- file.path(store, substr(id, 1, 2), substr(id, 3, 4))

  # make sure parent directory exists
  if (isTRUE(.create)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(dirname(path))) {
      stop("cannot create directory for id ", id, call. = FALSE)
    }
  }

  # return the full path
  file.path(path, paste0(id, ext))
}


#' @inheritDotParams base::print
#'
#' @rdname filesystem_os
#' @export
print.filesystem <- function (x, ...)
{
  files <- list.files(x, full.names = TRUE, recursive = TRUE)
  tsize <- sum(vapply(files, file.size, numeric(1)))

  cat('Filesystem Object Store')
  cat('  {', as.character(x), '}\n')
  cat('  ', floor(length(grep('.rds$', files)) / 2), ' objects\n')
  cat('  ', format(`class<-`(tsize, 'object_size'), units = 'auto'))
}


#' @rdname memory_os
#' @export
toString.memory <- function (x, ...) {
  paste0('<filesystem:', x, '>')
}


#' @rdname filesystem_os
#' @export
os_remove.filesystem <- function (store) {
  unlink(as.character(store), recursive = TRUE, force = TRUE)
}


#' @rdname filesystem_os
#' @export
os_remove_objects.filesystem <- function (store, ids = os_list(store))
{
  # remove data and tag files
  ids <- c(ids, paste0(ids, '_tags'))

  # match existing files against object ids
  files <- list.files(store, full.names = TRUE, recursive = TRUE)
  ids <- match(ids, tools::file_path_sans_ext(basename(files)))

  invisible(unlink(files[ids]) == 0)
}




#' @rdname filesystem_os
#' @export
os_write.filesystem <- function (store, object, tags = list(), id = compute_id(object))
{
  stopifnot(is_filesystem(store), is_nonempty_character(id))
  stopifnot(is.list(tags))

  if (os_exists(store, id)) {
    stop("object already present in store", call. = FALSE)
  }

  path <- full_path(store, id, '.rds', .create = TRUE)
  saveRDS(object, path)

  path <- full_path(store, id, '_tags.rds', .create = FALSE)
  saveRDS(tags, path)

  id
}


#' @rdname filesystem_os
#' @export
os_update_tags.filesystem <- function (store, id, tags)
{
  stopifnot(is_filesystem(store), is_nonempty_character(id))
  stopifnot(is.list(tags))

  if (!os_exists(store, id)) {
    stop("object does not exist in store", call. = FALSE)
  }

  path <- full_path(store, id, '_tags.rds', .create = FALSE)
  saveRDS(tags, path)

  id
}


#' @rdname filesystem_os
#' @export
os_read.filesystem <- function (store, id)
{
  stopifnot(is_filesystem(store), is_nonempty_character(id))
  if (!os_exists(store, id)) {
    stop("object `", id, "` does not exist in store", call. = FALSE)
  }

  list(object = os_read_object(store, id),
       tags   = os_read_tags(store, id))
}


#' @rdname filesystem_os
#' @export
os_read_object.filesystem <- function (store, id)
{
  stopifnot(is_filesystem(store), is_nonempty_character(id))
  path <- full_path(store, id, '.rds')
  stopifnot(file.exists(path))
  readRDS(path)
}


#' @rdname filesystem_os
#' @export
os_read_tags.filesystem <- function (store, id)
{
  stopifnot(is_filesystem(store), is_nonempty_character(id))
  path <- full_path(store, id, '_tags.rds')
  stopifnot(file.exists(path))
  readRDS(path)
}


#' @rdname filesystem_os
#' @export
os_exists.filesystem <- function (store, id)
{
  stopifnot(is_filesystem(store), is.character(id))

  if (!length(id)) return(logical(0))
  file.exists(full_path(store, id, '.rds'))
}


#' @rdname filesystem_os
#' @export
#' @importFrom tools file_path_sans_ext
os_list.filesystem <- function (store)
{
  stopifnot(is_filesystem(store))

  # [^s] means ignore all files matching *_tags.rds
  files <- basename(list.files(store, pattern = '*[^s].rds', recursive = TRUE))
  unique(file_path_sans_ext(files))
}


#' @rdname filesystem_os
#' @export
#'
#' @import rlang
#' @importFrom tools file_path_sans_ext
#' @importFrom stringi stri_sub
#'
os_find.filesystem <- function (store, tags)
{
  stopifnot(is_filesystem(store), is.list(tags))

  isq <- vapply(tags, rlang::is_quosure, logical(1))
  stopifnot(all(isq))

  files <- list.files(store, full.names = TRUE, recursive = TRUE,
                      pattern = '*_tags.rds')
  ans <- vapply(files, function (path) {
    values <- readRDS(path)
    all(vapply(tags, function (x, data) isTRUE(try(rlang::eval_tidy(x, data), silent = TRUE)),
               logical(1), data = values))
  }, logical(1))

  stri_sub(basename(files), 1, -10)[ans]
}

