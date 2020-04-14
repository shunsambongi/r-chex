color <- function(x, ...) {
  UseMethod("color")
}

color.character <- function(x, ...) {
  color(as_status(x))
}

color.chex_result <- function(x, ...) {
  color(as_status(x))
}

color.chex_status <- function(x, ...) {
  recode_chr(
    .x = vec_data(x),
    .default   = "magenta",
    .missing   = "magenta",
    !!SKIP    := "blue",
    !!PASS    := "green",
    !!WARNING := "yellow",
    !!FAIL    := "red",
    !!ERROR   := "red",
    !!INVALID := "red",
  )
}

colorize <- function(x, color = chex::color(x), ...) {
  color <- vec_recycle(color, size = vec_size(x))
  mapply(crayon::style, x, color, SIMPLIFY = TRUE, USE.NAMES = FALSE)
}
