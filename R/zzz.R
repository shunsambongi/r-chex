.onLoad <- function(...) {
  init_logging()
  vctrs::s3_register("dplyr::filter", "chex_result")
}
