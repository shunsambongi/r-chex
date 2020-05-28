.onLoad <- function(...) {
  vctrs::s3_register("dplyr::filter", "chex_result")
}

