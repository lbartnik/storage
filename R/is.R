is_scalar_character <- function (x) {
  is.character(x) && (length(x) == 1)
}

is_nonempty_character <- function (x) {
  is_scalar_character(x) && (nchar(x) > 0)
}

is_all_named <- function (x) {
  if (!length(x)) return(TRUE)
  !is.null(names(x)) && all(names(x) != "")
}

is_all <- function (x, what) {
  if (!length(x)) return(TRUE)
  all(vapply(lapply(x, what), isTRUE, logical(1)))
}
