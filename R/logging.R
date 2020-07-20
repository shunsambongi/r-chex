log_result <- function(result) {
  pass_level <- getOption("CHEX_PASS_LOG_LEVEL", logger::SUCCESS)
  fail_level <- getOption("CHEX_FAIL_LOG_LEVEL", logger::WARN)

  description <- description(result)
  for (i in 1:vec_seq_along(result)) {
    res <- result[i]
    desc <- description[i]
    level <- if (res == PASS) {
      pass_level
    } else if (res == FAIL) {
      fail_level
    } else {
      next
    }
    logger::log_level(level, desc, namespace = "chex")
  }
  invisible(result)
}
