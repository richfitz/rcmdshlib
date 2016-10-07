##' Test if compilation appears possible.  This tries compiling a
##' trivial C program with \code{R CMD SHLIB} (using
##' \code{\link{shlib}}).  Results are cached between runs within
##' a session so this should be fast to rely on.
##'
##' If this function believes you can't compile, and if \code{gcc}
##' can't be found on the path, a diagnostc message will be printed.
##' This will of course not be very interesting if you use a different
##' compiler to gcc!  But the most likely people affected here are
##' Windows users; if you get this ensure that you have rtools
##' installed.  If you have \code{devtools} installed,
##' \code{devtools::find_rtools()} may be helpful for diagnosing
##' compiler issues.
##'
##' @title Test if compilation is possible
##'
##' @param verbose Be verbose when running commands?
##'
##' @param skip_cache Try again to compile, skipping the cached value?
##'
##' @return A logical scalar
##'
##' @export
##' @examples
##' can_compile() # will take ~0.1s the first time
##' can_compile() # should be basically instantaneous
can_compile <- function(verbose = FALSE, skip_cache = FALSE) {
  ## should offer a verbose option
  ## should check if GCC is in the path?
  if (skip_cache || is.null(cache$can_compile)) {
    tmp <- tempfile()
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE))
    file <- file.path(tmp, "hello.c")
    writeLines("#include <R.h>", file)
    can_compile <- TRUE
    tryCatch(shlib(file, verbose = verbose),
             error = function(e) can_compile <<- FALSE)
    ## This is an attempt at giving a slightly more informative
    ## message when compilation fails
    if (!can_compile && Sys.which("gcc") == "") {
      message("I don't see gcc on the PATH") # nocov
    }
    cache$can_compile <- can_compile
  }
  isTRUE(cache$can_compile)
}

cache <- new.env(parent = emptyenv())
