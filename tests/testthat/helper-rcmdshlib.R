hello_c <- function(filename = tempfile()) {
  code <- '#include <R.h>\nvoid test() {Rprintf("Hello world\\n");}'
  writeLines(code, filename)
  filename
}
