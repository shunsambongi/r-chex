
# constructor -------------------------------------------------------------

new_result <- function(
  status = new_status(),
  description = character(),
  duration = new_duration()
) {
  fields <- list2(
    status = vec_assert(status, new_status()),
    description = vec_assert(description, character()),
    duration = vec_assert(duration, new_duration()),
  )
  new_rcrd(fields, class = c("chex_result", "chex_logical"))
}

#' Create a result
#'
#' @param status the result status. This can be one of:
#'   * logical vector (`TRUE` / `FALSE`)
#'   * character vector (`"PASS"`, `"FAIL"`, etc.)
#'   * `chex::PASS` or `chex::FAIL`
#'   * value of [chex::PASS()] or [chex::FAIL()]
#' @param description the description of the result
#' @export
result <- function(status, description) {
  status <- if (is.character(status)) {
    status_(status)
  } else {
    as_status(status)
  }
  description <- vec_cast(description, character())
  duration <- vec_cast(NA, new_duration())
  c(status, description, duration) %<-% vec_recycle_common(
    status,
    description,
    duration,
  )
  new_result(status, description, duration)
}

result_ <- function(status, description, duration) {
  out <- result(status, description)
  duration(out) <- duration
  out
}

#' Test if object is a result
#' @param x an object to test
#' @export
is_result <- function(x) inherits(x, "chex_result")


# formatting --------------------------------------------------------------

#' @export
format.chex_result <- function(x, ...) {
  bullet <- fmt_bullet(x)
  body <- fmt_body(x)
  dur <- fmt_duration(x)
  reason <- fmt_reason(x)

  paste0(bullet, " ", body, dur, reason)
}

fmt_style <- function(x) {
  purrr::map(color(x), cli::make_ansi_style)
}

fmt_bullet <- function(x, style = fmt_style(x)) {
  purrr::map2(style, bullet(x), rlang::exec)
}

fmt_body <- function(x) {
  status <- status_chr(x)
  desc <- description(x)
  out <- paste(status, "...", desc)

  passed <- status == "PASS"
  vec_slice(out, passed) <- unclass(
    cli::col_silver(vec_slice(out, passed))
  )
  out
}

fmt_duration <- function(x) {
  dur <- duration(x)
  out <- prettyunits::pretty_dt(dur)
  out <- paste0(" (", out, ")")

  threshold <- 0.5
  long <- dur >= threshold
  vec_slice(out, long) <- unclass(
    cli::col_cyan(vec_slice(out, long))
  )
  short <- dur < threshold
  vec_slice(out, short) <- ""
  out
}

fmt_reason <- function(x, style = fmt_style(x)) {
  reason <- reason(x)
  out <- purrr::map2_chr(style, reason, rlang::exec)

  no_reason <- is.na(reason)
  vec_slice(out, no_reason) <- ""
  vec_slice(out, !no_reason) <- paste0("\n", vec_slice(out, !no_reason))
  out
}


# output ------------------------------------------------------------------

#' @export
vec_ptype_abbr.chex_result <- function(x, ...) "rslt"

#' @export
vec_ptype_full.chex_result <- function(x, ...) "result"

#' @export
obj_print_header.chex_result <- function(x, ...) {
  cli::cat_rule(
    left = "RESULTS",
    right = sprintf("[%d]", vec_size(x))
  )
  invisible(x)
}

#' @export
obj_print_data.chex_result <- function(x, ...) {
  if (!length(x)) {
    cli::cat_line("<no results>", col = "silver")
    return(invisible(x))
  }

  cli::cat_line(format(x))

  invisible(x)
}


# coercion ----------------------------------------------------------------

#' @export
vec_ptype2.chex_result.chex_result <- function(x, y, ...) new_result()


# casting -----------------------------------------------------------------

#' @export
vec_cast.chex_result.chex_result <- function(x, to, ...) x
#' @export
vec_cast.chex_result.logical <- function(x, to, ...) {
  result(x, NA_character_)
}
#' @export
vec_cast.logical.chex_result <- function(x, to, ...) {
  vec_cast(as_status(x), to, ...)
}
#' @export
vec_cast.chex_status.chex_result <- function(x, to, ...) {
  status(x)
}

#' @export
as.data.frame.chex_result <- function(
  x, row.names = NULL, optional = FALSE, ...
) {
  out <- vec_data(x)
  out$status <- status_chr(x)
  out$reason <- reason(x)
  as.data.frame(x = out, row.names = row.names, optional = optional, ...)
}


# wrangle -----------------------------------------------------------------

# zzz.R
filter.chex_result <- function(.data, ...) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Missing required package `dplyr`", call. = FALSE)
  }
  x <- dplyr::as_tibble(.data)
  .data <- dplyr::filter(x, ...)
  status <- new_status(.data$status, .data$reason)
  new_result(status, .data$description, .data$duration)
}


# ops ---------------------------------------------------------------------

#' @export
`!.chex_result` <- function(x) {
  !as.logical(x)
}

# data --------------------------------------------------------------------

update_rcrd <- function(x, field, value) {
  field(x, field) <- vec_recycle(value, vec_size(x))
  x
}

#' @export
status.chex_result <- function(x, ...) {
  field(x, "status")
}

#' @export
`status<-.chex_result` <- function(x, value) {
  if (is.character(value)) {
    value <- new_preset(value)
  }
  value <- if (is_preset(value)) {
    status <- field(x, "status")
    as_status(value, reason_from = status)
  } else {
    vec_cast(value, new_status())
  }
  update_rcrd(x, "status", value)
}

status_chr <- function(x, ...) {
  as.character(status(x))
}

#' @export
description.chex_result <- function(x, ...) {
  field(x, "description")
}

#' @export
`description<-.chex_result` <- function(x, value) {
  update_rcrd(x, "description", value)
}

#' @export
duration.chex_result <- function(x, ...) {
  stopifnot(is_result(x))
  field(x, "duration")
}

#' @export
`duration<-.chex_result` <- function(x, value) {
  update_rcrd(x, "duration", value)
}

#' @export
reason.chex_result <- function(x, ...) {
  reason(status(x))
}

#' @export
`reason<-.chex_result` <- function(x, value) {
  status <- field(x, "status")
  status <- update_rcrd(status, "reason", value)
  field(x, "status") <- status
  x
}

color.chex_result <- function(x, ...) {
  x <- status_chr(x)
  color(x)
}

bullet.chex_result <- function(x, ...) {
  x <- status_chr(x)
  bullet(x)
}
