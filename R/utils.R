recode_chr <- function(.x, ..., .default = NULL, .missing = NULL) {
  x <- vec_cast(.x, character())
  mapping <- vec_unchop(list2(...))
  stopifnot(is_named(mapping))
  new <- unname(mapping[x])
  vec_slice(new, is.na(new)) <- .default %||% NA
  vec_slice(new, is.na(x)) <- .missing %||% NA
  new
}

coalesce <- function(x, y) {
  missing <- is.na(x)
  y <- vec_recycle(y, size = vec_size(x))
  vec_slice(x, missing) <- vec_slice(y, missing)
  x
}

colorize <- function(x, color) {
  color <- vec_recycle(color, size = vec_size(x))
  mapply(crayon::style, x, color, SIMPLIFY = TRUE, USE.NAMES = FALSE)
}
