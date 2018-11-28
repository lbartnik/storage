#' Public interface for every object store.
#'
#' Basic operations:
#' \enumerate{
#'   \item store (write) an object with its tags
#'   \item restore (read) an object given its id
#'   \item find objects (ids) by tags
#'   \item extra utility calls
#' }
#'
#' @rdname object_store
#' @name object_store
#'
#' @param store object store.
#' @param id object identifier; see [compute_id].
#' @param ids multiple object identifiers.
NULL


#' @param x object to be tested.
#'
#' @rdname object_store
#' @export
is_object_store <- function (x) inherits(x, 'object_store')


#' @rdname object_store
#' @export
os_remove <- function (store) UseMethod("os_remove")


#' @rdname object_store
#' @export
os_remove_objects <- function (store, ids = os_list(store)) UseMethod("os_remove_objects")


#' @param object R object to be written into object store.
#' @param tags a named `list` of tags for an object.
#'
#' @return \code{os_write} returns object id.
#' @rdname object_store
#' @export
os_write <- function (store, object, tags, id = compute_id(object)) UseMethod("os_write")


#' @rdname object_store
#' @export
os_update_tags <- function (store, id, tags) UseMethod("os_update_tags")


#' @rdname object_store
#' @export
os_read <- function (store, id) UseMethod("os_read")


#' @rdname object_store
#' @export
os_read_object <- function (store, id) UseMethod("os_read_object")


#' @rdname object_store
#' @export
os_read_tags <- function (store, id) UseMethod("os_read_tags")


#' @rdname object_store
#' @export
os_exists <- function (store, id) UseMethod("os_exists")


#' @rdname object_store
#' @export
os_find <- function (store, tags) UseMethod('os_find')


#' @rdname object_store
#' @export
os_list <- function (store) UseMethod("os_list")
