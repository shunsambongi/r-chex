test_that("check_that works", {
  r <- check_that(mtcars, is.data.frame)
  expect_s3_class(r, "chex_result")
})

test_that("run_check works", {
  r <- run_check(is.data.frame, mtcars)
  expect_true(r)

  r <- run_check(TRUE, mtcars)
  expect_true(r)

  r <- run_check(FALSE, mtcars)
  expect_false(r)

  r <- run_check(~.x, TRUE)
  expect_true(r)

  r <- run_check(~.x, FALSE)
  expect_false(r)
})

describe("description priority", {
  test1 <- function(x) TRUE
  it("should default to the call if no description exists", {
    result <- check_that(mtcars, test1)
    expect_equal(description(result), "test1")
  })

  test2 <- set_description(test1, "x")
  it("should use the description if it is set", {
    result <- check_that(mtcars, test2)
    expect_equal(description(result), "x")
  })

  test3 <- function(x) {
    result(PASS, "y")
  }
  test4 <- set_description(test3, "z")
  test5 <- function(x) {
    update_description("update")
    result(PASS, "y")
  }
  it("should always use the result description over check description", {
    result <- check_that(mtcars, test3, test4, test5)
    expect_equal(description(result), c("y", "y", "y"))
  })
})

test_that("current_check works", {
  x <- NULL
  test <- function(x) {
    x <<- current_check()
    TRUE
  }

  check_that(mtcars, test)
  expect_type(x, "closure")
  expect_equal(description(x), "test")

  r <- check_that(mtcars, "named" = test)
  expect_type(x, "closure")
  expect_equal(description(x), "test")
  expect_equal(description(r), "named")

  check_that(mtcars, set_description(test, "set"))
  expect_type(x, "closure")
  expect_equal(description(x), "set")

  y <- NULL
  test <- function(x) {
    y <<- set_description(current_check(), "dynamic")
    TRUE
  }
  result <- check_that(mtcars, test)
  expect_equal(description(result), "dynamic")

  result <- check_that(mtcars, "named" = test)
  expect_equal(description(y), "dynamic")
  expect_equal(description(result), "named")
})
