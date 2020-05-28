#' Perform checks
#'
#' @param .x an object to check
#' @param ... checks
#' @export
check_that <- function(.x, ...) {
  checks <- rlang::quos(...)
  checks <- purrr::imap(checks, function(check, name) {
    if (name != "") description(check) <- name
    check
  })
  checks <- unname(checks)
  results <- purrr::map(checks, do_check, .x)
  vec_unchop(results)
}

do_check <- function(check, input) {
  start <- Sys.time()
  tryCatch({
    check <- as_check(check)

    dur <- capture_duration({
      rv <- check(input)
    })

    status <- as_status(rv)

    desc <- if (is_result(rv)) {
      description(rv)
    } else {
      description(check) %||% NA
    }

    result_(status, desc, dur)
  }, error = function(e) {
    dur <- Sys.time() - start
    result_(
      status_("FAIL", conditionMessage(e)),
      description(check) %||% NA,
      duration = dur
    )
  })
}

capture_duration <- function(code) {
  start <- Sys.time()
  force(code)
  Sys.time() - start
}


# as_status ---------------------------------------------------------------

#' Convert return value to status
#'
#' This is not the same as [vctrs::vec_cast()] to `chex_status`. These are the
#' types that `do_check()` will recognize as valid return values.
#' @keywords internal
as_status <- function(x, ...) {
  UseMethod("as_status")
}

as_status.chex_preset <- function(x, reason_from = NULL, ...) {
  out <- status_(attr(x, "text"), ...)
  if (is.null(reason_from)) {
    return(out)
  }
  out <- vec_recycle(out, vec_size(reason_from))
  reason(out) <- reason(reason_from)
  out
}

as_status.chex_result <- function(x, ...) vec_cast(x, new_status())

as_status.chex_status <- function(x, ...) x

as_status.logical <- function(x, ...) vec_cast(x, new_status())

# as_check ----------------------------------------------------------------

#' Convert object to check
#'
#' A check is function that takes one argument and returns a pass or fail value.
#' There are a few types which are recognized as pass or fail.
#' * logical values `TRUE` and `FALSE` which are recognized as pass and fail,
#' respectively
#' * `chex::PASS` or the result of calling [chex::PASS()] with a reason
#' * likewise for `chex::FAIL`
#' * A result object created by [result()]
#'
#' @param x an object to convert
#' @param ... other args passed onto methods
#' @aliases check
#' @export
as_check <- function(x, ...) {
  UseMethod("as_check")
}

#' @export
as_check.quosure <- function(x, ...) {
  out <- as_check(rlang::eval_tidy(x), ...)
  if (is.null(description(out))) {
    description(out) <- description(x)
  }
  out
}

#' @export
as_check.function <- function(x, description = NULL, ...) {
  out <- rlang::as_closure(x)
  if (!missing(description)) {
    stopifnot(is_string(description))
    description(out) <- description
  }
  out
}

#' @export
as_check.logical <- function(x, ...) {
  force(x)
  as_check(function(y) {
    x
  }, ...)
}

#' @export
as_check.formula <- function(x, ...) {
  as_check(rlang::as_function(x), ...)
}
