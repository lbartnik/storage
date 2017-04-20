#' Filesystem-based object store.
#'
#' @rdname filesystem_os
#' @name filesystem_os
NULL


#' @rdname filesystem_os
#' @export
filesystem <- function (path = getwd(), create = FALSE)
{
  assert_dir(path, create)
  structure(path, class = c('object_store', 'filesystem'))
}


#' @rdname filesystem_os
#' @export
is_filesystem <- function (x) is_object_store(x) && inherits(x, 'filesystem')


#' @rdname filesystem_os
#' @export
print.filesystem <- function (x)
{
  files <- list.files(store, full.names = TRUE, recursive = TRUE)
  tsize <- sum(vapply(files, file.size, numeric(1)))

  cat('Filesystem Object Store\n')
  cat('  ', floor(length(grep('.rds$', files)) / 2), ' objects\n')
  cat('  ', format(`class<-`(tsize, 'object_size'), units = 'auto'))
}


assert_dir <- function (path, create)
{
  if (!dir.exists(path) && isTRUE(create)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  else {
    stop('directory ', path, ' does not exist but `create` is FALSE',
         call. = FALSE)
  }

  if (!dir.exists(path)) {
    stop('could not create directory ', path, call. = FALSE)
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

  # return the fill path
  file.path(path, paste0(id, ext))
}


#' @rdname filesystem_os
#' @export
os_remove.filesystem <- function (x) {
  unlink(as.character(x), recursive = TRUE, force = TRUE)
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


os_read.filesystem <- function (store, id)
{
  stopifnot(is_filesystem(store), is_nonempty_character(id))
  list(object = os_read_object(store, id),
       tags   = os_read_tags(store, id))
}

os_read_object.filesystem <- function (store, id)
{
  stopifnot(is_filesystem(store), is_nonempty_character(id))
  readRDS(full_path(store, id, '.rds'))
}

os_read_tags.filesystem <- function (store, id)
{
  stopifnot(is_filesystem(store), is_nonempty_character(id))
  readRDS(full_path(store, id, '_tags.rds'))
}


os_exists.filesystem <- function (store, id)
{
  stopifnot(is_filesystem(store), is.character(id))

  if (!length(id)) return(logical(0))
  file.exists(full_path(store, id, '.rds'))
}


#' @importFrom tools file_path_sans_ext
os_list.filesystem <- function (store)
{
  stopifnot(is_filesystem(store))

  # [^s] means ignore all files matching *_tags.rds
  files <- basename(list.files(store, pattern = '*[^s].rds', recursive = TRUE))
  unique(file_path_sans_ext(files))
}


#' @importFrom lazyeval lazy_eval
#' @importFrom tools file_path_sans_ext
#' @importFrom stringi stri_sub
#'
os_find.filesystem <- function (store, lazy_tags)
{
  stopifnot(is_filesystem(store), is.list(lazy_tags))

  cls <- vapply(lazy_tags, class, character(1))
  stopifnot(all(cls == 'lazy'))

  files <- list.files(store, full.names = TRUE, recursive = TRUE,
                      pattern = '*_tags.rds')
  ans <- vapply(files, function (path) {
    values <- readRDS(path)
    all(vapply(lazy_tags, lazy_eval, logical(1), data = values))
  }, logical(1))

  stri_sub(basename(files), 1, -10)[ans]
}

