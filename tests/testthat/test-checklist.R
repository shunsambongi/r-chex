test_that("checklist works", {
  x <- checklist(is.data.frame, is.character)
  result <- run_check(x, mtcars)
  expect_length(result, 2)
  expect_true(result[1] == PASS)
  expect_true(result[2] == FAIL)
})
