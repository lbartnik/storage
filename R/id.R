#' Object identifier.
#'
#' @param x object to compute identifier for (`compute_id`), the
#'        identifier to print or cast to `character` (`print` and
#'        `toString`), a `character` value to cast as an identifier
#'        (`as_id`) or a value to be tested (`is_id`).
#'
#' @rdname identifier
#' @name identifier
#'
#' @examples
#' i <- compute_id(iris)
#'
#' toString(i) # returns 8-character string
#' toString(long(i)) # returns 40-character string
#'
#' print(i) # prints 8 characters
#' print(long(i)) # prints 40 characters
NULL

#' @description `compute_id` calculates an unique identifier based on the
#' contents of the object `x`.
#'
#' @return Object of class `"identifier"`.
#'
#' @rdname identifier
#' @export
compute_id <- function (x) UseMethod("compute_id")


#' @description the default implementation of `compute_id` uses the
#' __sha1__ algorithm implemented in [digest::digest].
#'
#' @rdname identifier
#' @export
compute_id.default <- function (x) {
  y <- digest::digest(x, algo = 'sha1')

  stopifnot(identical(length(y), 1L), identical(nchar(y), 40L),
            identical(grep('^[0-9a-f]*$', y), 1L))
  as_id(y)
}

#' @description `as_id` turns a 40-character string into an identifier.
#'
#' @rdname identifier
#' @export
as_id <- function (x) {
  stopifnot(is.character(x))
  structure(x, class = 'identifier')
}


#' @description `is_id` tests whether `x` is an identifier.
#'
#' @rdname identifier
#' @export
is_id <- function (x) inherits(x, 'identifier')


#' @inheritDotParams base::print
#'
#' @rdname identifier
#' @export
print.identifier <- function (x, ...) {
  cat('<identifier:', toString(x), '>\n', sep='')
  invisible(x)
}


#' @rdname identifier
toString.identifier <- function (x, ...) {
  if (isTRUE(attr(x, 'long'))) return(unclass(x))
  substr(unclass(x), 1, 8)
}


#' @description `long` forces printing of all 40 characters of an
#' identifier.
#'
#' @rdname identifier
#' @export
long <- function (x) {
  attr(x, 'long') <- TRUE
  x
}


#' `match_short` searches for an identifier matching the given short
#' (8-character) string. If more than one identifier matches `short`,
#' throws an exception. If no identifiers match, returns `NULL`.
#'
#' @param short 8-character string to match.
#' @param store object store where `short` is expected to be matched.
#'
#' @rdname identifier
#' @export
match_short <- function (short, store) {
  ids <- os_list(store)
  ids <- stringi::stri_subset_fixed(ids, short)

  if (identical(length(ids), 0L)) {
    return(NULL)
  }

  if (!identical(length(ids), 1L)) {
    stop('more than one identifier matches ', short, call. = FALSE)
  }

  as_id(ids)
}
