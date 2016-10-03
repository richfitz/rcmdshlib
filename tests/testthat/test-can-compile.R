context("can_compile")

test_that("can_compile", {
  expect_true(can_compile())
  expect_true(can_compile())
  expect_true(can_compile(skip_cache=TRUE))
})
