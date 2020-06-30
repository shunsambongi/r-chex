#' Get/set data fields
#' @name data
#' @param x object used to get or set fields
#' @param value new value to set
#' @param ... other args passed on to methods
NULL

# status ------------------------------------------------------------------

#' @export
#' @rdname data
status <- function(x, ...) {
  UseMethod("status")
}

#' @export
#' @rdname data
`status<-` <- function(x, value) {
  UseMethod("status<-")
}


# description -------------------------------------------------------------

#' @export
#' @rdname data
description <- function(x, ...) {
  UseMethod("description")
}

#' @export
description.default <- function(x, ...) {
  attr(x, "description")
}

#' @export
#' @rdname data
`description<-` <- function(x, value) {
  UseMethod("description<-")
}

#' @export
`description<-.default` <- function(x, value) {
  stopifnot(is_string(value) || is.null(value))
  attr(x, "description") <- value
  x
}

#' Set object description
#'
#' @param x object being described
#' @param description the description
#' @export
set_description <- function(x, description) {
  description(x) <- description
  x
}


# reason ------------------------------------------------------------------

#' @export
#' @rdname data
reason <- function(x, ...) {
  UseMethod("reason")
}

#' @export
#' @rdname data
`reason<-` <- function(x, value) {
  UseMethod("reason<-")
}


# duration ----------------------------------------------------------------

#' @export
#' @rdname data
duration <- function(x, ...) {
  UseMethod("duration")
}

#' @export
#' @rdname data
`duration<-` <- function(x, value) {
  UseMethod("duration<-")
}


# color -------------------------------------------------------------------

color <- function(x, ...) {
  UseMethod("color")
}

color.character <- function(x, ...) {
  recode_chr(
    x,
    PASS = "green",
    FAIL = "red",
    .default = "blue",
    .missing = "blue"
  )
}


# bullet ------------------------------------------------------------------

bullet <- function(x, ...) {
  UseMethod("bullet")
}

bullet.character <- function(x, ...) {
  recode_chr(
    x,
    PASS = cli::symbol$tick,
    FAIL = cli::symbol$cross,
    .default = "?",
    .missing = "?"
  )
}
