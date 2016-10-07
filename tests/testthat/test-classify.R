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

test_that("parse clang error", {
  ret <- classify_compiler_output(readLines("logs/clang_error.txt"))
  expect_equal(ret$type,
               c("command", "error", "error", "command"))
  format(ret)
})

test_that("compilation warning", {
  expect_warning(
    handle_compiler_output(readLines("logs/gcc_warnings.txt"),
                           verbose = FALSE, quiet = FALSE, use_colour = FALSE),
    "There were 2 compiler warnings")
  expect_warning(
    handle_compiler_output(readLines("logs/gcc_warnings.txt"),
                           verbose = TRUE, quiet = FALSE, use_colour = FALSE),
    "There were 2 compiler warnings")
  expect_silent(
    handle_compiler_output(readLines("logs/gcc_warnings.txt"),
                           verbose = FALSE, quiet = TRUE, use_colour = FALSE))
})

test_that("compilation failure", {
  str <- readLines("logs/gcc_error.txt")
  ret <- classify_compiler_output(str)
  expect_equal(ret$type,
               c("command", "error", "error", "context", "warning", "error",
                 "note", "error", "context", "error", "error", "command"))
  format(ret)
  expect_output(print(ret), "Error 1")

  output <- readLines("logs/gcc_error.txt")
  attr(output, "status") <- 1

  expect_silent(
    handle_compiler_output(output, verbose = FALSE, quiet = TRUE,
                           fail_on_error = FALSE, use_colour = FALSE))
  expect_error(suppressMessages(
    handle_compiler_output(output, verbose = FALSE, quiet = TRUE,
                           fail_on_error = TRUE, use_colour = FALSE)),
    "Error compiling source")
  expect_message(try(
    handle_compiler_output(output, verbose = FALSE, quiet = TRUE,
                           fail_on_error = TRUE, use_colour = FALSE),
    silent = TRUE),
    "unknown type name")

  expect_warning(suppressMessages(
    handle_compiler_output(output, verbose = FALSE, quiet = FALSE,
                           fail_on_error = FALSE, use_colour = FALSE)),
    "There were 6 compiler errors")
  expect_warning(suppressMessages(
    handle_compiler_output(output, verbose = FALSE, quiet = FALSE,
                           fail_on_error = FALSE, use_colour = FALSE)),
    "There was 1 compiler warning")
  expect_message(suppressWarnings(
    handle_compiler_output(output, verbose = FALSE, quiet = FALSE,
                           fail_on_error = FALSE, use_colour = FALSE)),
    "unknown type name")
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

test_that("roundtrip", {
  for (f in dir("logs", full.names = TRUE)) {
    txt <- readLines("logs/gcc_warnings.txt")
    expect_equal(as.character(classify_compiler_output(txt)), txt)
  }
})
