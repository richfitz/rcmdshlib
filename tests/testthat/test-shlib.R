context("shlib")

test_that("hello", {
  code <- '#include <R.h>\nvoid test() {Rprintf("Hello world\\n");}'
  writeLines(code, "hello.c")
  res <- shlib("hello.c")
  expect_is(res$output, "compiler_output")
  expect_equal(res$dll, paste0("hello", .Platform$dynlib.ext))

  dyn.load(res$dll)
  expect_output(.C("test", "hello"), "Hello world")
  dyn.unload(res$dll)
  file.remove(c("hello.c", "hello.o", res$dll))
})

test_that("clean", {
  expect_silent(
    res <- shlib("test.c", preclean = TRUE, clean = FALSE, verbose = FALSE))
  expect_true(file.exists("test.o"))
  expect_message(res <- shlib("test.c", preclean = TRUE, clean = TRUE))
  expect_false(file.exists("test.o"))
})

test_that("compilation failure", {
  path <- tempfile(fileext = ".c")
  writeLines("this is a test", path)
  expect_error(shlib(path),
               "Error compiling source")
})

test_that("different output", {
  res <- shlib("test.c", output = "foo.dylib",
               preclean = TRUE, clean = TRUE, verbose = FALSE)
  expect_equal(res$dll, "foo.dylib")
  expect_true(file.exists(res$dll))
  expect_false(file.exists(paste0("test", .Platform$dylib.ext)))
})

test_that("debug dll", {
  if (!is_windows()) {
    expect_error(shlib("test.c", debug = TRUE, preclean = TRUE),
                 "The 'debug' option is valid only on Windows")
  }
})
