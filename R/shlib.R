##' Compile a shared library using \code{R CMD SHLIB}.  Options exist
##' for converting the compiler output to R messages, warnings and
##' errors.
##'
##' @title Compile shared library
##' @param filenames Vector of filenames
##' @param verbose Be verbose (print compiler output to screen)
##' @param output Name to use for the file output (passed to
##'   \code{SHLIB} with \code{--output} -- by default the name of the
##'   first file is used with the extension changed to
##'   \code{.Platform$dynlib.ext}).
##' @param clean Clean intermediate targets (passed to \code{SHLIB} as
##'   \code{--clean})
##' @param preclean Clean intermediate targets before running (passed
##'   to \code{SHLIB} as \code{--clean})
##' @param dry_run Don't run anything, but print commands (passed to
##'   \code{SHLIB} as \code{--dry-run})
##' @param debug Create a debug dll (windows only; passed to
##'   \code{SHLIB} as \code{--debug})
##' @param stop_on_error If an error in compilation is thrown, throw
##'   an error (TRUE by default).  If set to \code{FALSE}, then the
##'   compilation output will be returned (the \code{dll} element will
##'   be set to \code{NA}).
##' @param warn_on_warning If a the compiler returns a warning, issue
##'   an R warning (TRUE by default).
##' @param chdir One of \code{FALSE} (the default) indicating that the
##'   working directory will not be changed, \code{TRUE} indicating
##'   that the working directory should be set to the directory
##'   containing source files, or a character string indicating the
##'   directory to set immediately prior to compilation.  If
##'   \code{TRUE}, then all files listed in \code{filenames} must be
##'   in the same directory.
##' @param use_colour Use ANSI escape colours (via the \code{crayon}
##'   package) if on a ANSI compatibile terminal (does not support
##'   Rstudio, Rgui or Rterm).  If \code{NULL} (the default) then
##'   colour will be used where support is detected by
##'   \code{crayon::has_color()}
##' @return Invisibly, A list with four elements: \code{success} - a
##'   logical indicating if the command was successful (this can only
##'   be \code{FALSE} if \code{stop_on_error} is \code{FALSE}),
##'   \code{output} - an object of class \code{compiler_output}
##'   containing compiler output, \code{dll} - the path to the created
##'   shared library, or \code{NA_character_} if the command failed,
##'   and \code{base} - the dll name without leading path or extension
##'   (suitable to use as the \code{PACKAGE} argument to
##'   \code{.Call}).  If the command changed directory then the
##'   normalized (and therefore absolute) path to the dll is returned.
##' @export
##' @author Rich FitzJohn
##' @examples
##' # A small piece of C code saved as add.c within a temporary directory:
##' txt <- c("#include <R.h>",
##'         "#include <Rinternals.h>",
##'         "SEXP add2(SEXP a, SEXP b) {",
##'         "  return ScalarReal(REAL(a)[0] + REAL(b)[0]);",
##'         "}")
##' path <- tempfile()
##' dir.create(path)
##' filename <- file.path(path, "add.c")
##' writeLines(txt, filename)
##'
##' # Compile the object:
##' res <- rcmdshlib::shlib(filename)
##' res
##'
##' # Load it into R:
##' dyn.load(res$dll)
##'
##' # And call it
##' .Call("add2", pi, 1, PACKAGE = res$base)
##' # Cleanup
##' dyn.unload(res$dll)
##' unlink(path, recursive = TRUE)
shlib <- function(filenames, verbose = TRUE,
                  output = NULL, clean = FALSE, preclean = FALSE,
                  dry_run = FALSE, debug = FALSE,
                  stop_on_error = TRUE, warn_on_warning = TRUE,
                  chdir = FALSE, use_colour = NULL) {
  ## TODO: Allow setting include paths here explicitly.  This requires
  ## a bit of faff to get through without using a Makevars file.
  ##
  ## From the look of something (was it Gabor's docs) we might be able
  ## to pass -L and -l directly through though.
  if (debug && !is_windows()) {
    stop("The 'debug' option is valid only on Windows")
  }

  dat <- shlib_filenames(filenames, output, chdir)

  opts <- c(character(0),
            c("--output", dat$dll),
            if (clean) "--clean",
            if (preclean) "--preclean",
            if (dry_run) "--dry-run",
            if (debug) "--debug")

  args <- c("CMD", "SHLIB", dat$filenames, opts)

  ## This approach requires that callr is tweaked to allow capture of
  ## standard error
  ##   collector <- compiler_classifier(use_colour)
  ##   callback <- if (verbose || dry_run) collector$show else collector$add
  ##   callr::rcmd_safe("SHLIB", args, callback = callback)
  ##   dat <- collector$get()

  Sys.setenv(R_TESTS = "")
  if (!is.null(dat$wd)) {
    if (verbose) {
      message(sprintf("Compiling in '%s'", dat$wd))
    }
    owd <- setwd(dat$wd)
    on.exit(setwd(owd))
  }
  ## TODO: pass through the libraries here as well probably; I've done
  ## that somewhere else and it's not too awful (context?)
  res <- system3(file.path(R.home(), "bin", "R"), args)

  status <- res$status
  success <- res$success
  output <- handle_compiler_output(res$output, success, verbose, stop_on_error,
                                   warn_on_warning, use_colour)
  if (!success) {
    base <- dll <- NA_character_
  } else if (is.null(dat$wd)) {
    dll <- dat$dll
    base <- dat$base
  } else {
    dll <- normalizePath(dat$dll, mustWork = TRUE)
    base <- dat$base
  }

  invisible(list(success = success,
                 output = output,
                 dll = dll,
                 base = base))
}

## NOTE: This separation exists primarily to facilitate testing.
handle_compiler_output <- function(output, success, verbose,
                                   stop_on_error = TRUE,
                                   warn_on_warning = TRUE,
                                   use_colour = FALSE) {
  output <- classify_compiler_output(output)
  nw <- sum(output$type == "warning")

  if (!success && stop_on_error) {
    message(format(output, use_colour = use_colour))
    stop("Error compiling source", call. = FALSE)
  } else if (nw > 0 && warn_on_warning) {
    message(format(output, use_colour = use_colour))
    fmt1 <- "There was %d compiler warning"
    fmt2 <- "There were %d compiler warnings"
    warning(sprintf(ngettext(nw, fmt1, fmt2), nw, "warning"), call. = FALSE)
  } else if (verbose) {
    message(format(output, use_colour = use_colour))
  }

  invisible(output)
}

## NOTE: In clang, context around an error does not come with any
## leading whitespace and there is a little bit saying "n errors
## generated" which will be lumped in with this.
compiler_classifier <- function(use_colour) {
  types <- collector()
  values <- collector()
  last_type <- "MISSING"
  compiler <- "gcc"

  ## These probably need tweaking depending on the compiler:
  re_command <- sprintf("^(%s|make:)\\s", compiler)
  re_context <- "^([[:alnum:]._]+): (In|At) ([[:alnum:]]+)\\s.*:$"
  re_info <- "^([[:alnum:]/._]+):([0-9]+)(:[0-9]+)?: (warning|error|note):.*$"
  re_continue <- "^\\s+"
  continue <- FALSE

  add <- function(x) {
    if (last_type == "MISSING") {
      compiler <<- sub("^(.+?)\\s.*$", "\\1", x)
      re_command <<- sprintf("^(%s|make:)\\s", compiler)
    }

    if (grepl(re_command, x)) {
      type <- "command"
      continue <<- FALSE
    } else if (grepl(re_context, x)) {
      type <- "context"
      continue <<- FALSE
    } else if (grepl(re_info, x)) {
      type <- sub(re_info, "\\4", x)
      continue <<- TRUE
    } else if (continue || grepl(re_continue, x)) {
      type <- "continue"
    } else {
      type <- "unknown"
      continue <<- TRUE
    }
    last_type <<- type
    types$add(type)
    values$add(x)
  }
  get <- function() {
    output_compress(list(type = types$get(), value = values$get()))
  }

  list(add = add, get = get)
}

output_compress <- function(x) {
  j <- 0L
  idx <- integer(length(x$type))
  for (i in seq_along(idx)) {
    if (x$type[[i]] != "continue") {
      j <- j + 1
    }
    idx[[i]] <- j
  }

  ret <- list(type = vcapply(split(x$type, idx), "[[", 1L, USE.NAMES = FALSE),
              value = unname(split(x$value, idx)))
  class(ret) <- "compiler_output"
  ret
}

## I wonder about the possibility of marking up the output more than
## this; rather than just colouring the output line-by-line we could
## pick out file/line/col, etc.
##
## For text only output it would b nice to use a 1 char prefix perhaps.
compiler_output_styles <- function(use_colour) {
  if (is.null(use_colour)) {
    use_colour <- crayon::has_color()
  }
  if (use_colour) {
    cols <- c(command = "bold",
              error = "red",
              warning = "yellow",
              note = "blue",
              context = "darkgrey",
              error_continue = "#FF8080",
              warning_continue = "#FFFF80",
              note_continue = "#8080FF")
    ## context_continue?
    ##
    ## For the info *around* the error (generated by desaturating these
    ## colours by 1/2):
    ##
    ##   m <- col2rgb(cols)
    ##   m <- rgb2hsv(m[1, ], m[2, ], m[3, ])
    ##   cols_info <- setNames(hsv(m[1, ], m[2, ] / 2, m[3, ]), names(cols))
    lapply(cols, crayon::make_style)
  } else {
    prefix <- c(command = ">",
                error = "E",
                warning = "W",
                note = "N",
                context = "I",
                unknown = " ",
                ##
                error_continue = ".",
                warning_continue = ".",
                note_continue = "."
                ## context_continue?
                ## unknown_continue?
                )
    f <- function(prefix) {
      fmt <- sprintf("[%s] %%s", prefix)
      function(x) {
        sprintf(fmt, x)
      }
    }
    lapply(prefix, f)
  }
}

##' @export
format.compiler_output <- function(x, ..., use_colour = NULL) {
  styles <- compiler_output_styles(use_colour)

  type <- x$type
  value <- x$value

  for (i in seq_along(type)) {
    t <- type[[i]]
    s <- styles[[t]]
    if (!is.null(s)) {
      v <- value[[i]]
      if (length(v) > 1) {
        sc <- styles[[paste0(t, "_continue")]] %||% identity
        value[[i]] <- c(s(v[[1L]]), sc(v[-1L]))
      } else {
        value[[i]] <- s(v)
      }
    }
  }

  paste(sprintf("%s", unlist(value, use.names = FALSE)), collapse = "\n")
}

##' @export
print.compiler_output <- function(x, ..., use_colour = NULL) {
  str <- format.compiler_output(x, use_colour = use_colour)
  cat(paste0(str, "\n"))
  invisible(x)
}

##' @export
as.character.compiler_output <- function(x, ...) {
  unlist(x$value)
}

classify_compiler_output <- function(txt, use_colour = FALSE) {
  f <- compiler_classifier(use_colour)
  for (i in txt) {
    f$add(i)
  }
  f$get()
}

## Mostly to help with testing:
shlib_filenames <- function(filenames, output, chdir) {
  assert_files_exist(filenames)
  if (is_windows()) {
    ## This is needed or gcc can't find the paths, as backslashes get
    ## lost along the way.
    filenames <- gsub("\\", "/", filenames, fixed = TRUE)
  }

  if (is.null(output)) {
    dll <- paste0(tools::file_path_sans_ext(filenames[[1L]]),
                  .Platform$dynlib.ext)
  } else {
    if (length(output) == 1L && is.character(output) && !is.na(output)) {
      dll <- output
    } else {
      stop("'output' must be a scalar character")
    }
    if (!ends_with(output, .Platform$dynlib.ext)) {
      stop("'output' must end with ", .Platform$dynlib.ext)
    }
    ## Check that the output directory is sane; this only has to be
    ## done when output is explicitly given because otherwise it's
    ## derived from the first filename (which must be in an existing
    ## directory).
    output_dir <- dirname(output)
    if (!file.exists(output_dir)) {
      stop(sprintf("output directory '%s' does not exist", output_dir))
    } else if (!is_directory(output_dir)) {
      stop(sprintf("output directory '%s' exists, but is not a directory",
                   output_dir))
    }
  }

  if (is.character(chdir) && length(chdir) == 1L) {
    if (!is_directory(chdir)) {
      stop("If given as a string, 'chdir' must be an existing directory")
    }
    wd <- chdir
  } else if (is_true(chdir)) {
    wd <- dirname(filenames)
    if (length(filenames) > 1L) {
      if (length(unique(wd)) > 1L) {
        stop("All source files must be in same directory if chdir = TRUE")
      }
      wd <- wd[[1L]]
    }
  } else if (is_false(chdir)) {
    wd <- NULL
  } else {
    stop("Invalid input for 'chdir'")
  }

  if (!is.null(wd)) {
    ## It might be nice to use a common root here but that's going to
    ## require pathr.
    if (all(dirname(filenames) == wd)) {
      filenames <- basename(filenames)
    }
    if (dirname(dll) == wd) {
      dll <- basename(dll)
    }
  }

  base <- tools::file_path_sans_ext(basename(dll))
  list(filenames = filenames, dll = dll, wd = wd, base = base)
}
