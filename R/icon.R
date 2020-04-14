icon <- function(x, ...) {
  UseMethod("icon")
}

icon.character <- function(x, ...) {
  icon(as_status(x))
}

icon.chex_result <- function(x, ...) {
  icon(as_status(x))
}

icon.chex_status <- function(x, ...) {
  tick <- cli::symbol$tick
  cross <- cli::symbol$cross
  warning <- cli::symbol$warning

  out <- recode_chr(
    .x = vec_data(x),
    .default   = "?",
    .missing   = "?",
    !!SKIP    := "-",
    !!PASS    := tick,
    !!WARNING := warning,
    !!FAIL    := cross,
    !!ERROR   := cross,
    !!INVALID := cross,
  )
  colorize(out, color(x))
}
