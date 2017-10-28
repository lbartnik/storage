#' @export
compute_id <- function (x) UseMethod("compute_id")


#' @export
compute_id.default <- function (x) digest::sha1(x)

#' @export
compute_id.environment <- function (x) compute_id(as.list(x))

#' @export
`compute_id.<-` <- function (x) compute_id(as.list(x))
