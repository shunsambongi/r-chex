.onLoad <- function(...) {
  logger::log_appender(identity, namespace = "chex")
  vctrs::s3_register("dplyr::filter", "chex_result")
}

