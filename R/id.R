#' @export
compute_id <- function (x) UseMethod("compute_id")


#' @export
compute_id.default <- function (x) digest::sha1(x)
