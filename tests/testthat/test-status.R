test_that("status constructor works", {
  s <- status_("PASS")
  expect_s3_class(s, "chex_status")
  expect_length(s, 1)
  expect_equal(reason(s), NA_character_)

  s <- status_("PASS", "")
  expect_equal(reason(s), NA_character_)

  s <- status_(letters, "x")
  expect_length(s, 26)
  expect_equal(reason(s), rep("x", 26))
  expect_equal(as.character(s), letters)
})

test_that("casting works", {
  s <- vec_cast(status_("PASS"), new_status())
  expect_equal(s, status_("PASS"))

  s <- vec_cast(c(TRUE, FALSE), new_status())
  expect_equal(s, status_(c("PASS", "FAIL")))

  s <- as_status(c(TRUE, FALSE))
  expect_equal(s, status_(c("PASS", "FAIL")))

  l <- as.logical(status_(c("PASS", "FAIL")))
  expect_equal(l, c(TRUE, FALSE))
})


test_that("equality work", {
  no_reason <- status_("PASS")
  reason <- status_("PASS", "test")
  reason2 <- status_("PASS", "other")
  reason3 <- PASS("test")
  chr <- "PASS"

  expect_true(status_("PASS") == status_("PASS"))
  expect_true(status_("PASS", "test") == status_("PASS", "test"))

  expect_true(status_("PASS", "test") == PASS("test"))
  expect_true(PASS("test") == status_("PASS", "test"))

  expect_true(status_("PASS") == PASS)
  expect_true(PASS == status_("PASS"))

  expect_true(status_("PASS", "test") == PASS)
  expect_true(PASS == status_("PASS", "test"))

  expect_true(status_("PASS") == "PASS")
  expect_true("PASS" == status_("PASS"))

  expect_true(status_("PASS", "test") == "PASS")
  expect_true("PASS" == status_("PASS", "test"))

  expect_false(status_("PASS", "test") == status_("PASS"))
  expect_false(status_("PASS", "test") == status_("PASS", "other"))
})

test_that("aggregation works", {
  n <- 20
  every <- status_(rep("PASS", n))
  some <- status_(c(rep("PASS", n / 2), rep("FAIL", n / 2)))
  expect_length(every, n)
  expect_length(some, n)

  expect_true(any(some))
  expect_true(any(every))
  expect_false(all(some))
  expect_true(all(every))
})
