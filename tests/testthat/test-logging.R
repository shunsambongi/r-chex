library(mockery)
library(withr)

local_logging_mock <- function(.local_envir = parent.frame()) {
  m <- mock()
  local_mock("logger::log_level" = m, .local_envir = .local_envir)
  invisible(m)
}

test_that("logging works for passing results", {
  m <- local_logging_mock()
  log_result(result("PASS", "test description"))

  expect_called(m, n = 1)
  args <- mock_args(m)
  expect_equal(args[[1]][[1]], logger::SUCCESS)
  expect_equivalent(args[[1]][[2]], "test description")
})

test_that("logging works for failing results", {
  m <- local_logging_mock()
  log_result(result("FAIL", "test description 2"))

  expect_called(m, n = 1)
  args <- mock_args(m)
  expect_equal(args[[1]][[1]], logger::WARN)
  expect_equivalent(args[[1]][[2]], "test description 2")
})

test_that("logging respects options", {
  local_options(list(
    "CHEX_PASS_LOG_LEVEL" = logger::INFO,
    "CHEX_FAIL_LOG_LEVEL" = logger::ERROR
  ))
  m <- local_logging_mock()

  log_result(result("PASS", "pass description"))
  log_result(result("FAIL", "fail description"))

  expect_called(m, n = 2)
  args <- mock_args(m)
  expect_equal(args[[1]][[1]], logger::INFO)
  expect_equal(args[[2]][[1]], logger::ERROR)
})

test_that("logging multiple results works", {
  m <- local_logging_mock()
  log_result(result(c("FAIL", "PASS"), c("a", "b")))

  expect_called(m, n = 2)
  args <- mock_args(m)
  expect_equal(args[[1]][[1]], logger::WARN)
  expect_equivalent(args[[1]][[2]], "a")
  expect_equal(args[[2]][[1]], logger::SUCCESS)
  expect_equivalent(args[[2]][[2]], "b")
})

test_that("log_result returns the results", {
  m <- local_logging_mock()
  r <- log_result(result("PASS", "a"))
  expect_s3_class(r, "chex_result")
})

test_that("log_result does nothing if the logger package is not installed", {
  m <- local_logging_mock()
  local_mock("requireNamespace" = function(...) FALSE)
  r <- log_result(result("PASS", "a"))
  expect_called(m, n = 0)
  expect_s3_class(r, "chex_result")
})

test_that("check_that calls log_result", {
  m <- local_logging_mock()
  check_that(mtcars, is.data.frame, is.character)
  expect_called(m, n = 2)
})

test_that("formatting is skipped", {
  m <- local_logging_mock()
  log_result(result("PASS", "{fakevar}"))
  args <- mock_args(m)
  expect_true(attr(args[[1]][[2]], "skip_formatter"))
})
