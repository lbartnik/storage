#' Memory-based object store.
#'
#' @rdname memory_os
#' @name memory_os
NULL


#' @rdname memory_os
#' @export
memory <- function ()
{
  structure(new.env(), class = c('object_store', 'memory'))
}


#' @rdname memory_os
#' @export
is_memory <- function (x) is_object_store(x) && inherits(x, 'memory')


#' @rdname memory_os
#' @export
print.memory <- function (x, ...)
{
  cat('Memory Object Store\n')
  cat('  ', floor(length(grep('.rds$', files)) / 2), ' objects\n')
}


#' @rdname memory_os
#' @export
os_remove.memory <- function (x) {
  rm(list = ls(envir = x, all.names = TRUE), envir = x)
}



#' @rdname memory_os
#' @export
os_write.memory <- function (store, object, tags = list(), id = compute_id(object))
{
  stopifnot(is_memory(store), is_nonempty_character(id))
  stopifnot(is.list(tags))

  if (os_exists(store, id)) {
    stop("object already present in store", call. = FALSE)
  }

  # it's environment-based, so reference semantics = writes are persistent
  store[[id]] <- list(object = object, tags = tags)
  id
}


#' @rdname memory_os
#' @export
os_read.memory <- function (store, id)
{
  stopifnot(is_memory(store), is_nonempty_character(id))
  if (!os_exists(store, id)) {
    stop("object `", id, "` does not exist in store", call. = FALSE)
  }

  store[[id]]
}


#' @rdname memory_os
#' @export
os_read_object.memory <- function (store, id)
{
  stopifnot(is_memory(store), is_nonempty_character(id))
  store[[id]]$object
}


#' @rdname memory_os
#' @export
os_read_tags.memory <- function (store, id)
{
  stopifnot(is_memory(store), is_nonempty_character(id))
  store[[id]]$tags
}


#' @rdname memory_os
#' @export
os_exists.memory <- function (store, id)
{
  stopifnot(is_memory(store), is.character(id))

  if (!length(id)) return(logical(0))
  !is.na(match(id, ls(envir = store)))
}


#' @rdname memory_os
#' @export
#' @importFrom tools file_path_sans_ext
os_list.memory <- function (store)
{
  stopifnot(is_memory(store))
  ls(envir = store)
}


#' @rdname memory_os
#' @export
#'
#' @importFrom lazyeval lazy_eval
#' @importFrom tools file_path_sans_ext
#' @importFrom stringi stri_sub
#'
os_find.memory <- function (store, lazy_tags)
{
  stopifnot(is_memory(store), is.list(lazy_tags))

  cls <- vapply(lazy_tags, class, character(1))
  stopifnot(all(cls == 'lazy'))

  tags <- eapply(store, function (el) {
    all(vapply(lazy_tags, function (x, data) isTRUE(try(lazy_eval(x, data), silent = TRUE)),
               logical(1), data = el$tags))
  })

  names(which(unlist(tags)))
}