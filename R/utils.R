
recode_chr <- function(.x, ..., .default = NULL, .missing = NULL) {
  .x <- vec_cast(.x, character())
  mapping <- vec_unchop(list2(...))
  stopifnot(rlang::is_named(mapping))
  new <- unname(mapping[.x])
  vec_slice(new, is.na(new)) <- .default %||% NA
  vec_slice(new, is.na(.x)) <- .missing %||% NA
  new
}
