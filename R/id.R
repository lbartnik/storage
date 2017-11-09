#' @export
compute_id <- function (x) UseMethod("compute_id")


#' @export
compute_id.default <- function (x) digest::digest(x, algo = 'sha1')


#' @export
shorten <- function (x) substr(x, 1, 8)


#' @export
enlongate <- function (x, storage) {
  ids <- os_list(storage)
  stringi::stri_subset_fixed(ids, x)
}
