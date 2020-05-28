
# logical base class ------------------------------------------------------

#' @export
`==.chex_logical` <- function(e1, e2) {
  if (is_result(e1) && is_result(e2)) {
    return(vec_equal(e1, e2, na_equal = TRUE))
  }
  if (is.character(e1)) {
    e1 <- new_preset(e1)
  }
  if (is.character(e2)) {
    e2 <- new_preset(e2)
  }
  if (is_preset(e2)) {
    tmp <- e1
    e1 <- e2
    e2 <- tmp
  }

  if (is_preset(e1) && is_preset(e2)) {
    x <- as_status(e1)
    y <- as_status(e2)
  } else if (is_preset(e1)) {
    y <- vec_cast(e2, new_status())
    x <- as_status(e1, reason_from = y)
  } else {
    x <- vec_cast(e1, new_status())
    y <- vec_cast(e2, new_status())
  }

  vec_equal(x, y, na_equal = TRUE)
}

#' @export
vec_math.chex_logical <- function(.fn, .x, ...) {
  status <- as_status(.x)
  switch(
    .fn,
    all = all(as.logical(status), ...),
    any = any(as.logical(status), ...),
    vec_math_base(.fn, .x, ...)
  )
}


# preset ------------------------------------------------------------------

new_preset <- function(text) {
  out <- function(reason) {
    status_(text, reason)
  }
  structure(out, text = text, class = c("chex_preset", "chex_logical"))
}

is_preset <- function(x) {
  inherits(x, "chex_preset")
}


# casting -----------------------------------------------------------------

#' @export
as.logical.chex_preset <- function(x, ...) {
  as.logical(as_status(x))
}

#' @export
as.character.chex_preset <- function(x, ...) {
  as.character(as_status(x))
}

#' @export
as.double.chex_preset <- function(x, ...) {
  as.double(as.logical(x))
}

#' @export
as.integer.chex_preset <- function(x, ...) {
  as.integer(as.double(x))
}

# output ------------------------------------------------------------------

#' @export
print.chex_preset <- function(x, ...) {
  cli::cat_line("<preset>")
  cli::cat_line(format(as_status(x)))
  invisible(x)
}


#' Preset status
#'
#' @param reason explanation of status
#' @name preset
#' @aliases presets
NULL

#' @export
#' @rdname preset
PASS <- new_preset("PASS")

#' @export
#' @rdname preset
FAIL <- new_preset("FAIL")
