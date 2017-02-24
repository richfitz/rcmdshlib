context("util")

test_that("force colour", {
  res <- compiler_output_styles(TRUE)
  expect_is(res$error, "crayon")
})

test_that("shortcircuit operator", {
  expect_equal(NULL %||% 1, 1)
  expect_equal(0 %||% 1, 0)
})
