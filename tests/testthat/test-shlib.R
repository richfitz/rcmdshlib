context("shlib")

test_that("hello", {
  code <- '#include <R.h>\nvoid test() {Rprintf("Hello world\\n");}'
  writeLines(code, "hello.c")
  res <- shlib("hello.c")
  expect_true(res$success)
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

  res <- shlib(path, stop_on_error = FALSE, warn_on_warning = FALSE,
               verbose = FALSE)
  expect_false(res$success)
  expect_is(res$output, "compiler_output")
  expect_identical(res$dll, NA_character_)
})

test_that("different output", {
  filename <- hello_c(tempfile(fileext = ".c"))
  base <- tools::file_path_sans_ext(filename)
  default_dll <- paste0(base, .Platform$dynlib.ext)
  dest <- paste0("myoutput", .Platform$dynlib.ext)
  res <- shlib(filename, output = dest,
               preclean = TRUE, clean = TRUE, verbose = FALSE)
  expect_equal(res$dll, dest)
  expect_true(file.exists(res$dll))
  expect_false(file.exists(default_dll))
  file.remove(c(filename, dest))
})

test_that("Invalid output", {
  expect_error(shlib("test.c", output = 1),
               "'output' must be a scalar character")
  expect_error(shlib("test.c", output = NA_character_),
               "'output' must be a scalar character")
  expect_error(shlib("test.c", output = c("a", "b")),
               "'output' must be a scalar character")
})

test_that("missing input files", {
  expect_error(shlib("file1.c"), "File does not exist: file1.c")
  expect_error(shlib(c("file1.c", "file2.c")),
                     "Files do not exist: file1.c, file2.c")
})

test_that("debug dll", {
  if (!is_windows()) {
    expect_error(shlib("test.c", debug = TRUE, preclean = TRUE),
                 "The 'debug' option is valid only on Windows")
  }
})

test_that("shlib_filenames changes slashes on windows (only)", {
  testthat::with_mock(
    `rcmdshlib:::is_windows` = function() TRUE,
    `base::file.exists` = function(...) TRUE,
    expect_equal(shlib_filenames("foo\\bar.c", NULL, FALSE)$filenames,
                 "foo/bar.c"))
  testthat::with_mock(
    `rcmdshlib:::is_windows` = function() FALSE,
    `base::file.exists` = function(...) TRUE,
    expect_equal(shlib_filenames("foo\\bar.c", NULL, FALSE)$filenames,
                 "foo\\bar.c"))
})

test_that("single filename with chdir = TRUE", {
  path <- tempfile2()
  dir.create(path)
  filename <- hello_c(file.path(path, "hello.c"))

  res <- shlib_filenames(filename, NULL, chdir = TRUE)
  expect_equal(res$filenames, basename(filename))
  expect_equal(res$dll, paste0("hello", .Platform$dynlib.ext))
  expect_equal(res$wd, path)

  ## If output is given, and in a different directory, it is not remapped
  dll <- tempfile2(fileext = .Platform$dynlib.ext)
  res <- shlib_filenames(filename, dll, chdir = TRUE)
  expect_equal(res$filenames, basename(filename))
  expect_equal(res$dll, dll)
  expect_equal(res$wd, path)

  ## If output is given, and in the same diretory, it is remapped
  dll <- tempfile2(tmpdir = path, fileext = .Platform$dynlib.ext)
  res <- shlib_filenames(filename, dll, chdir = TRUE)
  expect_equal(res$filenames, basename(filename))
  expect_equal(res$dll, basename(dll))
  expect_equal(res$wd, path)
})

test_that("multiple filenames with chdir = TRUE", {
  path <- tempfile2()
  dir.create(path)
  filenames <- c(hello_c(file.path(path, "file1.c")),
                 hello_c(file.path(path, "file2.c")))

  res <- shlib_filenames(filenames, NULL, chdir = TRUE)
  expect_equal(res$filenames, basename(filenames))
  expect_equal(res$dll, paste0("file1", .Platform$dynlib.ext))
  expect_equal(res$wd, path)

  ## If output is given, and in a different directory, it is not remapped
  dll <- tempfile2(fileext = .Platform$dynlib.ext)
  res <- shlib_filenames(filenames, dll, chdir = TRUE)
  expect_equal(res$filenames, basename(filenames))
  expect_equal(res$dll, dll)
  expect_equal(res$wd, path)

  ## If output is given, and in the same diretory, it is remapped
  dll <- tempfile2(tmpdir = path, fileext = .Platform$dynlib.ext)
  res <- shlib_filenames(filenames, dll, chdir = TRUE)
  expect_equal(res$filenames, basename(filenames))
  expect_equal(res$dll, basename(dll))
  expect_equal(res$wd, path)
})

test_that("multiple filenames with given chdir", {
  path <- tempfile2()
  dir.create(path)
  filenames <- c(hello_c(file.path(path, "file1.c")),
                 hello_c(file.path(path, "file2.c")))

  res <- shlib_filenames(filenames, NULL, chdir = tempdir2())
  expect_equal(res$filenames, filenames)
  expect_equal(res$dll, file.path(path, paste0("file1", .Platform$dynlib.ext)))
  expect_equal(res$wd, tempdir2())

  ## If output is given, and in a different directory, it is not remapped
  dll <- tempfile2(tmpdir = path, fileext = .Platform$dynlib.ext)
  res <- shlib_filenames(filenames, dll, chdir = tempdir2())
  expect_equal(res$filenames, filenames)
  expect_equal(res$dll, dll)
  expect_equal(res$wd, tempdir2())

  ## If output is given, and in the same diretory, it is remapped
  dll <- tempfile2(fileext = .Platform$dynlib.ext)
  res <- shlib_filenames(filenames, dll, chdir = tempdir2())
  expect_equal(res$filenames, filenames)
  expect_equal(res$dll, basename(dll))
  expect_equal(res$wd, tempdir2())
})

test_that("with chdir = TRUE, files must have same path", {
  path1 <- tempfile2()
  path2 <- tempfile2()
  dir.create(path1)
  dir.create(path2)
  filenames <- c(hello_c(file.path(path1, "file1.c")),
                 hello_c(file.path(path2, "file2.c")))
  expect_error(shlib_filenames(filenames, NULL, chdir = TRUE),
               "All source files must be in same directory")
})

test_that("output path must be existing directory if given", {
  path <- tempfile2()
  dir.create(path)
  filename <- hello_c(file.path(path, "hello.c"))
  out <- file.path(tempfile2(), paste0("out", .Platform$dynlib.ext))
  expect_error(shlib_filenames(filename, out, FALSE),
               "does not exist")
  writeLines("", dirname(out))
  expect_error(shlib_filenames(filename, out, FALSE),
               "but is not a directory")
})

test_that("chdir as string must exist", {
  path <- tempfile2()
  dir.create(path)
  filename <- hello_c(file.path(path, "hello.c"))
  expect_error(shlib_filenames(filename, NULL, tempfile2()),
               "'chdir' must be an existing directory")
})

test_that("Invalid chdir input", {
  path <- tempfile2()
  dir.create(path)
  filename <- hello_c(file.path(path, "hello.c"))
  expect_error(shlib_filenames(filename, NULL, 1),
               "Invalid input for 'chdir'")
})

test_that("invalid output extension", {
  path <- tempfile2()
  dir.create(path)
  filename <- hello_c(file.path(path, "hello.c"))
  expect_error(shlib_filenames(filename, "hello.txt", FALSE),
               "'output' must end with")
})

test_that("change directory on compilation", {
  path <- tempfile2()
  dir.create(path)
  owd <- getwd()
  filename <- hello_c(file.path(path, "hello.c"))
  expect_message(
    res <- shlib(filename, chdir = TRUE),
    "Compiling in '")
  expect_identical(getwd(), owd)
  expect_true(file.exists(res$dll))
  expect_equal(res$dll,
               normalizePath(file.path(path,
                                       paste0("hello", .Platform$dynlib.ext))))
})
