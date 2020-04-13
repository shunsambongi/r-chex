#' A list of checks
#' @param ... checks
#' @export
checklist <- function(...) {
  x <- rlang::quos(...)
  structure(x, class = c("chex_checklist", class(x)))
}

#' @export
run_check.chex_checklist <- function(check, x) {
  check_that(x, !!!check)
}
