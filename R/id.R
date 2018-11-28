#' Object identifier.
#'
#' @param x object to compute identifier for, the identifier to print
#'        or cast to `character` or a `character` value to cast as
#'        identifier.
#'
#' @rdname identifier
#' @name identifier
#'
#' @examples
#' i <- compute_id(iris)
#'
#' toString(i) # returns 8-character string
#' toString(full(i)) returns 40-character string
#'
#' print(i) # prints 8 characters
#' print(full(i)) # prints 40 characters
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
compute_id.default <- function (x) as_id(digest::digest(x, algo = 'sha1'))


#' @description `as_id` turns a 40-character string into an identifier.
#'
#' @rdname identifier
#' @export
as_id <- function (x) {
  stopifnot(is.character(x), identical(length(x), 1L), identical(nchar(x), 40L))
  stopifnot(identical(grep('^[0-9a-f]*$', x), 1L))

  structure(x, class = 'identifier')
}


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
