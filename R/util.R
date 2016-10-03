collector <- function(init=character(0)) {
  res <- init
  list(add=function(x, ...) res <<- c(res, sprintf(x, ...)),
       length=function(x) length(res), # used only in debugging below
       get=function() res)
}

vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}


##
##   Return the longest path prefix (taken character-by-character)
##   that is a prefix of all paths in list. If list is empty, return
##   the empty string (''). Note that this may return invalid paths
##   because it works a character at a time.
path_commonprefix <- function(paths) {
  if (length(paths) == 0L) {
    ""
  } else {
    ## s1 = min(m)
    ## s2 = max(m)
    ## for i, c in enumerate(s1):
    ##     if c != s2[i]:
    ##         return s1[:i]
    ## return s1
  }
}

## I honestly don't know if this is needed...
common_dirname <- function(filenames) {
  if (!any(file.exists(filenames))) {
    stop("File does not exist: ",
         paste(filenames[!file.exists(filenames)], collapse = ", "))
  }
  filenames <- normalizePath(filenames, mustWork = TRUE)
  dirname <- dirname(filenames)
  if (length(unique(dirname)) == 1L) {
    filenames <- basename(filenames)
  } else {
    stop("writeme")
  }

  list(dirname = dirname, filenames = filenames)
}

is_windows <- function() {
  .Platform$OS.type == "windows"
}
