#' Run some checks
#'
#' @param x An object to run your checks against
#' @param ... Checks to run against the object
#'
#' @export
check_that <- function(x, ...) {
  quos <- unname(enquos(...))
  results <- lapply(quos, do_check, x)
  vec_unchop(results)
}

do_check <- function(quo, x) {
  tryCatch(
    {
      check <- as_check(quo)
      description <- description(check)
      result <- with_handlers(
        check(x), chex_skip = check_cnd(SKIP, description)
      )
      if (is_result(result)) {
        return(result)
      }
      with_handlers(
        {
          result <- as_result(result)
          update(result, description = description)
        },
        vctrs_error_incompatible_cast = check_cnd(INVALID, description)
      )
    },
    error = check_cnd(ERROR, description(quo))
  )
}

check_cnd <- function(status, description) {
  function(cnd) {
    new_result(status, conditionMessage(cnd), description)
  }
}


# skip --------------------------------------------------------------------

#' Skip the current check
#'
#' Call this function from inside of a check function to stop checking and quit
#' the check early.
#'
#' @param details A character vector of size 1 with any details about why the
#'   check was skipped
#'
#' @export
skip_check <- function(details = NULL) {
  signal(details %||% NA_character_, class = "chex_skip")
}


# as_check ----------------------------------------------------------------

#' Create a checker
#'
#'
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
  description(x) <- description
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
