
new_status <- function(x = character()) {
  vec_assert(x, character())
  new_vctr(toupper(x), class = "chex_status")
}

methods::setOldClass(c("chex_status", "vctrs_vctr"))

#' `status` vector
#'
#' This creates a character vector that represents the result status of a check.
#' Any status that is created will be changed to all uppercase, i.e.
#' `status("pass") == status("PASS")`.
#'
#' @param x
#' * For `status()`: A character vector
#' * For `is_status()`: An object to test
#' * For `as_status()`: An object to cast
#'
#' @export
status <- function(x) {
  x <- vec_cast(x, character())
  new_status(x)
}

#' @export
#' @rdname status
is_status <- function(x) {
  vec_is(x, new_status())
}

#' @export
#' @rdname status
as_status <- function(x) {
  vec_cast(x, new_status())
}


# output ------------------------------------------------------------------

#' @export
vec_ptype_full.chex_status <- function(x, ...) "status"

#' @export
vec_ptype_abbr.chex_status <- function(x, ...) "stts"


# casting / coercion ------------------------------------------------------

#' @method vec_ptype2 chex_status
#' @export
#' @export vec_ptype2.chex_status
#' @rdname chex-vctrs
vec_ptype2.chex_status <- function(x, y, ...) {
  UseMethod("vec_ptype2.chex_status", y)
}

#' @method vec_ptype2.chex_status default
#' @export
vec_ptype2.chex_status.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
}

#' @method vec_ptype2.chex_status chex_status
#' @export
vec_ptype2.chex_status.chex_status <- function(x, y, ...) new_status()

#' @method vec_ptype2.chex_status character
#' @export
vec_ptype2.chex_status.character <- function(x, y, ...) new_status()
#' @method vec_ptype2.character chex_status
#' @export
vec_ptype2.character.chex_status <- function(x, y, ...) new_status()

#' @method vec_ptype2.chex_status chex_result
#' @export
vec_ptype2.chex_status.chex_result <- function(x, y, ...) new_status()
#' @method vec_ptype2.chex_result chex_status
#' @export
vec_ptype2.chex_result.chex_status <- function(x, y, ...) new_status()

#' @method vec_cast chex_status
#' @export
#' @export vec_cast.chex_status
#' @rdname chex-vctrs
vec_cast.chex_status <- function(x, to, ...) {
  UseMethod("vec_cast.chex_status")
}

#' @method vec_cast.chex_status default
#' @export
vec_cast.chex_status.default <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  vec_default_cast(x, to, x_arg, to_arg)
}

#' @method vec_cast.chex_status chex_status
#' @export
vec_cast.chex_status.chex_status <- function(x, to, ...) x

#' @method vec_cast.chex_status character
#' @export
vec_cast.chex_status.character <- function(x, to, ...) new_status(x)
#' @method vec_cast.character chex_status
#' @export
vec_cast.character.chex_status <- function(x, to, ...) vec_data(x)

#' @method vec_cast.chex_status logical
#' @export
vec_cast.chex_status.logical <- function(x, to, ...) {
  x <- c("FAIL", "PASS")[x + 1L]
  new_status(x)
}
#' @method vec_cast.logical chex_status
#' @export
vec_cast.logical.chex_status <- function(x, to, ...) {
  recode_chr(
    .x = vec_data(x),
    !!SKIP    := NA,
    !!PASS    := TRUE,
    # OTHER = NA,
    !!WARNING := getOption("chex.allow_warning", TRUE),
    !!FAIL    := FALSE,
    !!ERROR   := FALSE,
    !!INVALID := FALSE,
  )
}

#' @method vec_cast.integer chex_status
#' @export
vec_cast.integer.chex_status <- function(x, to, ...) {
  recode_chr(
    .x = vec_data(x),
    !!SKIP    := 0L,
    !!PASS    := 100L,

    # OTHER
    .default   = 200L,
    .missing   = 200L,

    !!WARNING := 300L,
    !!FAIL    := 400L,
    !!ERROR   := 400L,
    !!INVALID := 400L,
  )
}

#' @method vec_cast.chex_status chex_result
#' @export
vec_cast.chex_status.chex_result <- function(x, to, ...) field(x, "status")
#' @method vec_cast.chex_result chex_status
#' @export
vec_cast.chex_result.chex_status <- function(x, to, ...) {
  result(x)
}


# equality / comparison ---------------------------------------------------

#' @export
vec_proxy_compare.chex_status <- function(x, ...) {
  vec_cast(x, integer())
}

#' @export
vec_proxy_equal.chex_status <- function(x, ...) {
  vec_cast(x, integer())
}


# aggregation -------------------------------------------------------------

#' @export
vec_math.chex_status <- function(.fn, .x, ...) {
  .x <- as.logical(.x[.x != SKIP])
  switch(.fn,
    all = all(.x, ...),
    any = any(.x, ...),
    vec_math_base(.f, .x, ...)
  )
}



# presets -----------------------------------------------------------------

#' Preset Statuses
#'
#' @name preset-status
NULL

#' @export
#' @rdname preset-status
#' @format
SKIP <- new_status("SKIP")

#' @export
#' @rdname preset-status
#' @format
PASS <- new_status("PASS")

#' @export
#' @rdname preset-status
#' @format
OTHER <- new_status("OTHER")

#' @export
#' @rdname preset-status
#' @format
WARNING <- new_status("WARNING")

#' @export
#' @rdname preset-status
#' @format
FAIL <- new_status("FAIL")

#' @export
#' @rdname preset-status
#' @format
ERROR <- new_status("ERROR")

#' @export
#' @rdname preset-status
#' @format
INVALID <- new_status("INVALID")
