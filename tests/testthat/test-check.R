test_that("check_that works", {
  r <- check_that(mtcars, is.data.frame)
  expect_s3_class(r, "chex_result")
})

test_that("do_check works", {
  r <- do_check(is.data.frame, mtcars)
  expect_s3_class(r, "chex_result")
})
