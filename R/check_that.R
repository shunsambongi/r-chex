#' @export
check_that <- function(x, ...) {
  checks <- lapply(unname(enquos(...)), as_check)
  results <- lapply(checks, do_check, x)
  vec_unchop(results)
}

do_check <- function(check, x) {
  description <- description(check)

  tryCatch(
    {
      result <- check(x)
      if (is_result(result)) {
        return(result)
      }
      result <- as_result(result)
      update(result, description = description)
    },
    error = check_cnd("error", description),
    chex_skip = check_cnd("skip", description)
  )
}

check_cnd <- function(status, description) {
  function(cnd) {
    new_result(status, conditionMessage(cnd), description)
  }
}

#' @export
skip_check <- function(details = NULL) {
  signal(details %||% NA_character_, class = "chex_skip")
}
