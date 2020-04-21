# as_check ----------------------------------------------------------------

#' Create a checker
#'
#' @param x object to convert to check
#' @param ... other arguments passed onto methods
#'
#' @export
as_check <- function(x, ...) {
  UseMethod("as_check")
}

#' @export
as_check.quosure <- function(x, ...) {
  out <- as_check(eval_tidy(x), ...)
  if (is.null(description(out))) {
    description(out) <- description(x)
  }
  out
}

#' @export
as_check.function <- function(x, description = NULL, ...) {
  ellipsis::check_dots_empty()
  if (!missing(description)) {
    description(x) <- description
  }
  x
}

#' @export
as_check.logical <- function(x, ...) {
  force(x)
  f <- function(...) {
    x
  }
  as_check(f, ...)
}


# check_list --------------------------------------------------------------

#' List of checks
#'
#' @param ... list of checks
#'
#' @export
check_list <- function(...) {
  checks <- unname(enquos(...))
  structure(checks, class = c("chex_check_list", class(checks)))
}

#' @export
as_check.chex_check_list <- function(x, ...) {
  checks <- force(x)
  function(x) {
    check_that(x, !!!checks)
  }
}
