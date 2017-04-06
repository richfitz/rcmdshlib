hello_c <- function(filename = tempfile()) {
  code <- '#include <R.h>\nvoid test() {Rprintf("Hello world\\n");}'
  writeLines(code, filename)
  filename
}

tempfile2 <- function(...) {
  gsub("\\", "/", tempfile(...), fixed = TRUE)
}

tempdir2 <- function() {
  gsub("\\", "/", tempdir(), fixed = TRUE)
}
