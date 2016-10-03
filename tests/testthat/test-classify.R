context("classify")

test_that("empty compiler output", {
  expect_equal(classify_compiler_output(character(0)),
               structure(list(type = character(0), value = list()),
                         class = "compiler_output"))
  expect_equal(format(classify_compiler_output(character(0))), "")
})

test_that("parse gcc warnings", {
  ret <- classify_compiler_output(readLines("logs/gcc_warnings.txt"))
  expect_equal(ret$type,
               c("command", "context", "warning",
                 "context", "warning", "command"))
  format(ret, use_colour = FALSE)
  format(ret, use_colour = TRUE)
})


test_that("compilation warning", {
  expect_warning(
    handle_compiler_output(readLines("logs/gcc_warnings.txt"), FALSE),
    "There were 2 compiler warnings")
})

test_that("compilation failure", {
  str <- readLines("logs/gcc_error.txt")
  ret <- classify_compiler_output(str)
  expect_equal(ret$type,
               c("command", "error", "error", "context", "warning", "error",
                 "note", "error", "context", "error", "error", "command"))
  format(ret)
  expect_output(print(ret), "Error 1")
})

test_that("unclassifiable output", {
  txt <- readLines("logs/gcc_warnings.txt")
  cmp <- classify_compiler_output(txt)
  extra <- "here's another string"
  res <- classify_compiler_output(c(txt, extra))

  expect_equal(res$type, c(cmp$type, "unknown"))
  expect_equal(res$value, c(cmp$value, extra))
  format(res)
})
