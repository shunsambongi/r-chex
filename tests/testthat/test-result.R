expect_reason <- function(x, reason, ...) {
  expect_true(vec_equal(reason(x), reason, na_equal = TRUE), ...)
}

expect_description <- function(x, description, ...) {
  expect_true(vec_equal(description(x), description, na_equal = TRUE), ...)
}

expect_pass <- function(x, ...) {
  expect_s3_class(x, "chex_result")
  expect_identical(field(field(x, "status"), "text"), "PASS")
}

expect_fail <- function(x, ...) {
  expect_s3_class(x, "chex_result")
  expect_identical(field(field(x, "status"), "text"), "FAIL")
}

test_that("results work", {
  r <- result(TRUE, NA)
  expect_pass(r)
  expect_reason(r, NA)
  expect_description(r, NA)

  r <- result(TRUE, "test")
  expect_pass(r)
  expect_reason(r, NA)
  expect_description(r, "test")

  r <- result(status_("FAIL"), "test")
  expect_fail(r)
  expect_reason(r, NA)
  expect_description(r, "test")

  r <- result(status_("PASS", "test reason"), "test")
  expect_pass(r)
  expect_reason(r, "test reason")
  expect_description(r, "test")

  r <- result(PASS, "test")
  expect_pass(r)
  expect_reason(r, NA)
  expect_description(r, "test")

  r <- result(PASS("test reason"), "test")
  expect_pass(r)
  expect_reason(r, "test reason")
  expect_description(r, "test")
})

test_that("casting works", {
  # logical
  r <- result("PASS", "test")
  expect_equal(as.logical(r), TRUE)

  # data.frame

})

test_that("equality works", {
  # result vs result
  expect_true(result("PASS", NA) == result("PASS", NA))
  expect_true(result("PASS", "test") == result("PASS", "test"))
  expect_false(result("PASS", NA) == result("PASS", "test"))

  # result vs status
  expect_true(result("PASS", NA) == status_("PASS"))
  expect_true(result(status_("PASS", "test"), NA) == status_("PASS", "test"))
  expect_false(result("PASS", NA) == status_("PASS", "test"))

  # result vs preset
  expect_true(result("PASS", NA) == PASS)
  expect_true(PASS == result("PASS", NA))
  expect_true(result("FAIL", NA) == FAIL)
  expect_false(result("PASS", NA) == FAIL)

  # result vs character
  expect_true(result("PASS", NA) == "PASS")
  expect_true("PASS" == result("PASS", NA))
  expect_false(result("PASS", NA) == "FAIL")
})


# aggregation -------------------------------------------------------------

test_that("aggregation works", {
  len <- 20
  every <- result(rep(TRUE, len), NA)
  some <- vec_assign(every, (1:len) <= 10, result(FALSE, NA))
  expect_length(every, len)
  expect_length(some, len)
  expect_true(any(every))
  expect_true(any(some))
  expect_true(all(every))
  expect_false(all(some))

  r <- result(c(TRUE, NA), NA)
  expect_equal(all(r), NA)
  expect_true(all(r, na.rm = TRUE))

  r <- result(c(FALSE, NA), NA)
  expect_equal(any(r), NA)
  expect_false(any(r, na.rm = TRUE))
})
