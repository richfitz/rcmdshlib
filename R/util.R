collector <- function(init = character(0)) {
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

ends_with <- function(string, end) {
  len <- nchar(string)
  substr(string, len - nchar(end) + 1L, len) == end
}

system3 <- function(command, args) {
  res <- suppressWarnings(system2(command, args, stdout = TRUE, stderr = TRUE))
  code <- attr(res, "status") %||% 0
  attr(res, "status") <- NULL
  list(success = code == 0,
       code = code,
       output = res)
}
