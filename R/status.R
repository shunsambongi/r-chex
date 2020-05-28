
# constructors ------------------------------------------------------------

new_status <- function(text = character(), reason = character()) {
  fields <- list2(
    text = vec_assert(text, character()),
    reason = vec_assert(reason, character()),
  )
  new_rcrd(fields, class = c("chex_status", "chex_logical"))
}

status_ <- function(text, reason = NA) {
  c(text, reason) %<-% vec_cast_common(text, reason, .to = character())
  reason[reason == ""] <- NA_character_
  c(text, reason) %<-% vec_recycle_common(text, reason)
  new_status(text, reason)
}

is_status <- function(x) {
  inherits(x, "chex_status")
}


# formatting --------------------------------------------------------------

#' @export
format.chex_status <- function(x, ...) {
  as.character(x)
}
#' @export
vec_ptype_abbr.chex_status <- function(x, ...) {
  "stts"
}
#' @export
vec_ptype_full.chex_status <- function(x, ...) {
  "status"
}


# coercion ----------------------------------------------------------------

#' @export
vec_ptype2.chex_status.chex_status <- function(x, y , ...) new_status()
#' @export
vec_ptype2.chex_status.logical <- function(x, y, ...) new_status()
#' @export
vec_ptype2.logical.chex_status <- function(x, y, ...) new_status()


# casting -----------------------------------------------------------------


#' @export
vec_cast.chex_status.chex_status <- function(x, to, ...) x

#' @export
vec_cast.chex_status.logical <- function(x, to, ...) {
  status_(c("FAIL", "PASS")[x + 1L])
}
#' @export
vec_cast.logical.chex_status <- function(x, to, ...) {
  out <- c(FAIL = FALSE, PASS = TRUE)[field(x, "text")]
  unname(out)
}
#' @export
vec_cast.chex_status.character <- function(x, to, ...) status_(x)
#' @export
vec_cast.character.chex_status <- function(x, to, ...) field(x, "text")


# data --------------------------------------------------------------------

#' @export
reason.chex_status <- function(x, ...) {
  field(x, "reason")
}

#' @export
`reason<-.chex_status` <- function(x, value) {
  field(x, "reason") <- value
  x
}
