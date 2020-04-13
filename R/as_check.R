
as_check <- function(x, ...) {
  UseMethod("as_check")
}

as_check.quosure <- function(x, ...) {
  out <- as_check(eval_tidy(x))
  if (is.null(description(out))) {
    description(out) <- description(x)
  }
  out
}

as_check.function <- function(x, description = NULL, ...) {
  description(x) <- description
  x
}
