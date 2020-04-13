#' Perform checks
#'
#' @param .x an object to check
#' @param ... checks
#' @export
check_that <- function(.x, ...) {
  checks <- purrr::map(rlang::quos(...), function(quo) {
    check <- rlang::eval_tidy(quo)
    if (is.null(description(check))) {
      description(check) <- rlang::as_label(quo)
    }
    check
  })
  results <- purrr::map2(unname(checks), names(checks), function(check, name) {
    log_result(generate_result(check, name, .x))
  })
  vec_unchop(results)
}


generate_result <- function(check, name, .x) {
  start <- Sys.time()
  tryCatch({
    dur <- capture_duration({
      withCallingHandlers(
        rv <- run_check(check, .x),
        chex_current_check = function(c) {
          invokeRestart("chex_current_check", check)
        },
        chex_update_description = function(c) {
          description(check) <<- conditionMessage(c)
        }
      )
    })

    status <- as_status(rv)

    desc <- if (name != "") {
      name
    } else if (is_result(rv)) {
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


# run_check ---------------------------------------------------------------

#' Run check
#'
#' A check is usually a function that takes one argument and returns a pass or
#' fail value. There are a few types which are recognized as pass or fail.
#' * logical values `TRUE` and `FALSE` which are recognized as pass and fail,
#' respectively
#' * `chex::PASS` or the result of calling [chex::PASS()] with a reason
#' * likewise for `chex::FAIL`
#' * A result object created by [result()]
#'
#' In addition to functions, checks can also be one of the following:
#' * logical values, which get converted to pass or fail values directly
#' * formulas, which get converted into check functions using
#' [rlang::as_function()]
#' * custom R objects with a method for `run_check` which returns
#' one of the valid pass/fail values mentioned above
#'
#' @param check a object used to check the value of `x`
#' @param x an object to check
#' @aliases check
#' @export
run_check <- function(check, x) {
  UseMethod("run_check")
}

#' @export
run_check.function <- function(check, x) {
  check(x)
}

#' @export
run_check.logical <- function(check, x) {
  check
}

#' @export
run_check.formula <- function(check, x) {
  rlang::as_function(check)(x)
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


# current check -----------------------------------------------------------

#' Get the current check
#'
#' @export
current_check <- function() {
  current <- withRestarts(chex_current_check = identity, {
    rlang::signal("", "chex_current_check")
    stop(
      "`current_check()` should only be called inside a check function.",
      call. = FALSE
    )
  })
  structure(current, class = c("chex_current_check", class(current)))
}

update_description <- function(description) {
  rlang::signal(description, "chex_update_description")
  invisible(description)
}

#' @export
`description<-.chex_current_check` <- function(x, value) {
  update_description(value)
  NextMethod()
}
