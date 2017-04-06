collector <- function(init=character(0)) {
  res <- init
  list(add = function(x, ...) res <<- c(res, sprintf(x, ...)),
       length = function(x) length(res), # used only in debugging below
       get = function() res)
}

vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}

is_windows <- function() {
  .Platform$OS.type == "windows"
}

assert_files_exist <- function(filenames) {
  err <- !file.exists(filenames)
  if (any(err)) {
    stop(ngettext(sum(err),
                  "File does not exist: ",
                  "Files do not exist: "),
         paste(filenames[err], collapse = ", "))
  }
}

`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

is_directory <- function(filename) {
  res <- file.info(filename, extra_cols = FALSE)$isdir
  res & !is.na(res)
}

is_true <- function(x) {
  identical(as.vector(x), TRUE)
}
is_false <- function(x) {
  identical(as.vector(x), FALSE)
}
