#' @export
as_check <- function(x, ...) {
  UseMethod("as_check")
}

#' @export
as_check.quosure <- function(x, ...) {
  ellipsis::check_dots_empty()
  out <- as_check(eval_tidy(x))
  if (is.null(description(out))) {
    description(out) <- description(x)
  }
  out
}

#' @export
as_check.function <- function(x, description = NULL, ...) {
  ellipsis::check_dots_empty()
  description(x) <- description
  x
}
