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
  structure(path, class = c('storage', 'filesystem'))
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


#' @rdname filesystem_os
#' @export
is_filesystem <- function (x) is_storage(x) && inherits(x, 'filesystem')




full_path <- function (store, id, ext, .create = FALSE)
{
  # parent path
  path <- file.path(storage, substr(id, 1, 2), substr(id, 3, 4))

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
os_write.filesystem <- function (store, object, tags, id = compute_id(object))
{
  stopifnot(is_filesystem(sha1), is_nonempty_character(id))

  path <- full_path(store, id, '.rds', .create = TRUE)
  saveRDS(object, path)

  path <- full_path(store, id, '_tags.rds', .create = TRUE)
  saveRDS(tags, path)
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

read_tags.filesystem <- function (store, id)
{
  stopifnot(is_filesystem(store), is_nonempty_character(id))
  readRDS(full_path(store, id, '_tags.rds'))
}



#' @importFrom tools file_path_sans_ext
list_ids.filesystem <- function (storage)
{
  stopifnot(is_filesystem(storage))

  # [^s] means ignore _tags.rds
  files <- basename(list.files(storage, pattern = '*[^s].rds', recursive = TRUE))
  unique(file_path_sans_ext(files))
}


find_id.filesystem <- function (storage, id)
{
  stopifnot(is_filesystem(storage))
  file.exists(full_path(storage, id, '.rds'))
}

