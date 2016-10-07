## ---
## title: "rcmdshlib"
## author: "Rich FitzJohn"
## date: "`r Sys.Date()`"
## output: rmarkdown::html_vignette
## vignette: >
##   %\VignetteIndexEntry{rcmdshlib}
##   %\VignetteEngine{knitr::rmarkdown}
##   %\VignetteEncoding{UTF-8}
## ---

##+ echo = FALSE, results = "hide"
prepare <- function(code) {
  file <- tempfile(fileext = ".c")
  writeLines(code, file)
  writeLines(c("```c", code, "```"))
  file
}

## This package provides a thin wrapper around `R CMD SHLIB`.  It is
## aimed at people who need to create shared libraries
## programmatically from R, and may be particularly useful on Windows
## machines where R is not in the `PATH`.  It is heavily influenced by
## [Gabor Csardi](https://github.com/gaborcsardi)'s excellent
## [rcmdcheck](https://github.com/MangoTheCat/rcmdcheck) package.

## Comapred with running `R CMD check` directly from the command line,
## or via `system`, `rcmdcheck` provides:

## * ability to suppress compiler output unless there are warnings or errors
## * detection of compilation failure, with sensible conversion into an R
##   error
## * detection of compilation *warnings*, with conversion into R warnings
## * colour coded output on ANSI compatible terminals (via
##   [crayon](https://github.com/gaborcsardi/crayon)
## * an object that holds the results of compilation that can be inspected
##   later

## The package provides only two functions.

## The `can_compile` function tries to compile a very simple program
## to determine if the compilation environment is feasible:
rcmdshlib::can_compile()

## It caches its result within a session so carries little cost to use.

## The `shlib` function takes a vector of filenmaes and creates a
## shared library (`.so` or `.dll`) with them.

##+ echo = FALSE, results = "asis"
file <- prepare(c("#include <R.h>",
                  "#include <Rinternals.h>",
                  "SEXP add2(SEXP a, SEXP b) {",
                  "  return ScalarReal(REAL(a)[0] + REAL(b)[0]);",
                  "}"))

## This file is stored in the temporary file `r file`
file

## Compile the file; this will print to the screen compilation output
## unless you pass in `verbose = FALSE`
res <- rcmdshlib::shlib(file)

## The `res` object here is a list with two elements: `$dll` contains
## the full path to the created shared library:
res$dll

## And `$output` contains the compilation output (which is an object
## of class `compiler_output` which has custom `format` and `print`
## methods defined in the `shlib` package).
res$output

## If the compilation failed, then even when `verbose = FALSE` you'll
## get an error:
##+ echo = FALSE, results = "asis"
file_error <- prepare(c("#include <R.h>",
                        "SEXP add2(SEXP a, SEXP b) {",
                        "  return ScalarReal(REAL(a)[0] + REAL(b)[0]);",
                        "}"))
##+ echo = TRUE, error = TRUE
res2 <- rcmdshlib::shlib(file_error, verbose = TRUE)

## This is thrown as an R error so can be detected with `try`, etc.
