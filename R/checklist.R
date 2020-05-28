#' A list of checks
#' @param ... checks
#' @export
checklist <- function(...) {
  x <- rlang::quos(...)
  structure(x, class = c("chex_checklist", class(x)))
}

#' @export
as_check.chex_checklist <- function(x, ...) {
  checklist <- force(x)
  function(x) {
    check_that(x, !!!checklist)
  }
}
