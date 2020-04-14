new_result <- function(
  status = character(),
  details = character(),
  description = character()
) {
  fields <- list2(
    status = vec_assert(status, character()),
    details = vec_assert(details, character()),
    description = vec_assert(description, character()),
  )
  new_rcrd(fields, class = "chex_result")
}

# for compatibility with the S4 system
methods::setOldClass(c("chex_result", "vctrs_rcrd", "vctrs_vctr"))

#' @export
result <- function(
  status,
  details = chex::details(status) %||% NA_character_,
  description = NA_character_
) {
  c(status, details, description) %<-% vec_recycle_common(
    !!!vec_cast_common(
      status,
      details,
      description,
      .to = character()
    )
  )
  new_result(status, details, description)
}

#' @export
as_result <- function(x, ...) {
  vec_cast(x = x, to = new_result(), ...)
}

#' @export
is_result <- function(x, size = NULL) {
  vec_is(x, new_result(), size)
}


# output ------------------------------------------------------------------

#' @export
format.chex_result <- function(x, ...) {
  status(x)
}

#' @export
obj_print_header.chex_result <- function(x, ...) {
  if (isTRUE(getOption("chex.verbose", FALSE))) {
    cli::cat_rule("checkdata")
    cli::cat_line("version: ", utils::packageVersion("chex"))
    cli::cat_line()
  }
  invisible(x)
}

#' @export
obj_print_data.chex_result <- function(x, ...) {
  if (vec_is_empty(x)) return(invisible(x))
  cli::cat_rule("CHECKS")
  desc <- coalesce(
    description(x),
    crayon::silver("(check #", vec_seq_along(x), ")", sep = "")
  )
  details <- format_details(x)
  out <- paste(symbol(x), desc, "...", status(x))
  out <- paste0(out, details)

  cli::cat_line(out)
  invisible(x)
}

format_details <- function(x, bullet = cli::symbol$arrow_right, level = 2L) {
  bullet <- bullet %||% " "
  details <- vec_chop(details(x))
  wrapped <- strwrap(
    paste(bullet, details),
    indent = (level - 1L) * 2L,
    exdent = level * 2L,
    simplify = FALSE
  )
  out <- vapply(wrapped, paste, character(1L), collapse = "\n")
  out <- colorize(out, color(x))
  missing <- is.na(details)
  vec_slice(out, !missing) <- paste0("\n", vec_slice(out, !missing))
  vec_slice(out, missing) <- ""
  out
}


# casting / coercion ------------------------------------------------------

#' @method vec_ptype2 chex_result
#' @export
#' @export vec_ptype2.chex_result
#' @rdname chex-vctrs
vec_ptype2.chex_result <- function(x, y, ...) {
  UseMethod("vec_ptype2.chex_result", y)
}

#' @method vec_ptype2.chex_result default
#' @export
vec_ptype2.chex_result.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
}

#' @method vec_ptype2.chex_result chex_result
#' @export
vec_ptype2.chex_result.chex_result <- function(x, y, ...) new_result()

#' @method vec_ptype2.chex_result character
#' @export
vec_ptype2.chex_result.character <- function(x, y, ...) new_result()

#' @method vec_ptype2.character chex_result
#' @export
vec_ptype2.character.chex_result <- function(x, y, ...) new_result()

#' @method vec_cast chex_result
#' @export
#' @export vec_cast.chex_result
#' @rdname chex-vctrs
vec_cast.chex_result <- function(x, to, ...) {
  UseMethod("vec_cast.chex_result")
}

#' @method vec_cast.chex_result default
#' @export
vec_cast.chex_result.default <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  vec_default_cast(x, to, x_arg, to_arg)
}

#' @method vec_cast.chex_result chex_result
#' @export
vec_cast.chex_result.chex_result <- function(x, to, ...) x

#' @method vec_cast.character chex_result
#' @export
vec_cast.character.chex_result <- function(x, to, ...) status(x)

#' @method vec_cast.chex_result character
#' @export
vec_cast.chex_result.character <- function(x, to, ...) {
  result(x)
}

#' @method vec_cast.chex_result logical
#' @export
vec_cast.chex_result.logical <- function(x, to, ...) {
  status <- c("fail", "pass")[x + 1]
  details <- details(x) %||% NA_character_
  description <- description(x) %||% NA_character_
  result(status, details, description)
}

#' @method vec_cast.logical chex_result
#' @export
vec_cast.logical.chex_result <- function(x, to, ...) {
  recode_chr(
    .x = x,
    pass = TRUE,
    warning = getOption("chex.allow_warning", FALSE),
    fail = FALSE,
    error = FALSE,
    invalid = FALSE,
  )
}

#' @method vec_cast.data.frame chex_result
#' @export
vec_cast.data.frame.chex_result <- function(x, to, ...) {
  new_data_frame(unclass(x))
}

#' @export
as.data.frame.chex_result <- function(x, ...) {
  vec_cast(x, new_data_frame())
}


# comparison / equality ---------------------------------------------------

#' @export
vec_proxy_compare.chex_result <- function(x, ...) {
  recode_chr(
    x,
    skip = 0L,
    pass = 10L,
    warning = 20L,
    fail = 30L,
    invalid = 30L,
    error = 30L,
  )
}

#' @export
vec_proxy_equal.chex_result <- function(x, ...) {
  vec_proxy_compare(x, ...)
}

# aggregate / summary -----------------------------------------------------

#' @export
vec_math.chex_result <- function(.fn, .x, ...) {
  switch(.fn,
    all = all(as.logical(.x), ...),
    any = any(as.logical(.x), ...),
    vec_math_base(.fn, .x, ...)
  )
}

#' @importFrom stats aggregate
#' @export
aggregate.chex_result <- function(x, ...) {
  x <- vec_slice(x, x != "skip")
  desc <- crayon::silver("(aggregated)")
  result <- as_result(all(x))
  update(result, description = desc)
}

#' @export
summary.chex_result <- function(object, ...) {
  agg <- aggregate(object)
  list(
    status = status(agg),
    details = details(agg),
    total = vec_size(object),
    counts = vec_count(object)
  )
}


# fields ------------------------------------------------------------------

#' @importFrom stats update
#' @export
update.chex_result <- function(object, description = NULL) {
  if (!missing(description)) {
    description(object) <- description
  }
  object
}

status <- function(x, ...) {
  UseMethod("status")
}

status.chex_result <- function(x, ...) {
  field(x, "status")
}

status.logical <- function(x, ...) {
  c("fail", "pass")[x + 1L]
}

#' @export
details <- function(x, ...) {
  UseMethod("details")
}

#' @export
details.default <- function(x, ...) {
  attr(x, "details", exact = TRUE) %||% comment(x)
}

#' @export
details.chex_result <- function(x, ...) {
  field(x, "details")
}

#' @export
`details<-` <- function(x, value) {
  UseMethod("details<-")
}

#' @export
`details<-.default` <- function(x, value) {
  comment(x) <- value
  x
}

#' @export
`details<-.chex_result` <- function(x, value) {
  field(x, "details") <- value
  x
}

description.chex_result <- function(x, ...) {
  field(x, "description")
}

`description<-.chex_result` <- function(x, value) {
  field(x, "description") <- value
  x
}


# virtual fields ----------------------------------------------------------

symbol <- function(x, ...) {
  syms <- recode_chr(
    .x = x,
    .default = "?",
    .missing = "?",
    fail = cli::symbol$cross,
    error = cli::symbol$cross,
    invalid = cli::symbol$cross,
    warning = cli::symbol$warning,
    pass = cli::symbol$tick,
    skip = "-",
  )
  colorize(syms, color(x))
}

color <- function(x, ...) {
  recode_chr(
    .x = x,
    .default = "magenta",
    .missing = "magenta",
    fail = "red",
    error = "red",
    invalid = "red",
    warning = "yellow",
    pass = "green",
    skip = "silver",
  )
}


# DELETE ME ---------------------------------------------------------------

test_result <- function() {
  result(
    status = c("pass", "fail", "error", "warning", "skip"),
    details = c(NA, "something bad", "`...` is not empty.

We detected these problematic arguments:
* `a`

These dots only exist to allow future extensions and should be empty.
Did you misspecify an argument?
", NA, NA),
    description = c("first test", "second", "third", "penultimate", "last")
  )
}
