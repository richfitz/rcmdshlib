context("util")

test_that("force colour", {
  res <- compiler_output_styles(TRUE)
  expect_is(res$error, "crayon")
})
