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
