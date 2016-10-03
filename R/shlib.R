##' Compile a shared library using \code{R CMD SHLIB}.
##'
##' @title Compile shared library
##' @param filenames Vector of filenames
##' @param verbose Be verbose (print compiler output to screen)
##' @param output Name to use for the file output (passed to
##'   \code{SHLIB} with \code{--output} -- by default the name of the
##'   first file is used).
##' @param clean Clean intermediate targets (passed to \code{SHLIB} as
##'   \code{--clean})
##' @param preclean Clean intermediate targets before running (passed
##'   to \code{SHLIB} as \code{--clean})
##' @param dry_run Don't run anything, but print commands (passed to
##'   \code{SHLIB} as \code{--dry-run})
##' @param debug Create a debug dll (windows only; passed to
##'   \code{SHLIB} as \code{--debug})
##' @param use_colour Use ANSI escape colours (via the \code{crayon}
##'   package) if on a ANSI compatibile terminal (does not support
##'   Rstudio, Rgui or Rterm).  If \code{NULL} (the default) then
##'   colour will be used where support is detected by
##'   \code{crayon::has_color()}
##' @export
##' @author Rich FitzJohn
shlib <- function(filenames, verbose = TRUE,
                  output = NULL, clean = FALSE, preclean = FALSE,
                  dry_run = FALSE, debug = FALSE, use_colour = NULL) {
  ## TODO: Allow setting include paths here explicitly.  This requires
  ## a bit of faff to get through without using a Makevars file.
  ##
  ## From the look of something (was it Gabor's docs) we might be able
  ## to pass -L and -l directly through though.
  if (debug && !is_windows()) {
    stop("The 'debug' option is valid only on Windows")
  }

  ## Determine the common root (not sure if this is 100% needed)
  assert_files_exist(filenames)

  if (is.null(output)) {
    dll <- paste0(tools::file_path_sans_ext(filenames[[1L]]),
                  .Platform$dynlib.ext)
  } else {
    if (length(output) == 1L && is.character(output) && !is.na(output)) {
      dll <- output
    } else {
      stop("'output' must be a scalar character")
    }
  }

  opts <- c(character(0),
            c("--output", dll),
            if (clean) "--clean",
            if (preclean) "--preclean",
            if (dry_run) "--dry-run",
            if (debug) "--debug")

  args <- c("CMD", "SHLIB", filenames, opts)

  ## This approach requires that callr is tweaked to allow capture of
  ## standard output.
  ## collector <- compiler_classifier(use_colour)
  ## callback <- if (verbose || dry_run) collector$show else collector$add
  ## callr::rcmd_safe("SHLIB", args, callback = callback)
  ## dat <- collector$get()

  output <- suppressWarnings(system2(file.path(R.home(), "bin", "R"), args,
                                     stdout=TRUE, stderr=TRUE))

  invisible(list(output = handle_compiler_output(output, verbose),
                 dll = dll))
}

## NOTE: This separation exists primarily to facilitate testing.
handle_compiler_output <- function(output, verbose, use_colour = FALSE) {
  status <- attr(output, "status")
  error <- !is.null(status) && status != 0L
  output <- classify_compiler_output(output)

  if (error) {
    message(format(output, use_colour = use_colour))
    stop("Error compiling source", call. = FALSE)
  }

  w <- output$type == "warning"
  if (any(w)) {
    str <- ngettext(sum(w),
                    "There was 1 compiler warning:\n",
                    sprintf("There were %d compiler warnings:\n", sum(w)))
    warning(str, format(output, use_colour = use_colour), call. = FALSE)
  } else if (verbose) {
    message(format(output, use_colour = use_colour))
  }
  output
}

compiler_classifier <- function(use_colour) {
  types <- collector()
  values <- collector()
  last_type <- "MISSING"
  compiler <- "gcc"

  ## These probably need tweaking depending on the compiler:
  re_command <- sprintf("^(%s|make:)\\s", compiler)
  re_context <- '^([[:alnum:]._]+): (In|At) ([[:alnum:]]+)\\s.*:$'
  re_info <- '^([[:alnum:]._]+):([0-9]+)(:[0-9]+)?: (warning|error|note):.*$'
  re_continue <- '^\\s+'
  continue <- FALSE
  continue_type <- ""

  styles <- compiler_output_styles(use_colour)

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
      continue_type <<- type
    } else if (continue && grepl(re_continue, x)) {
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
  show <- function(x) {
    add(x)
    type <- if (continue) paste0(continue_type, "_continue") else last_type
    s <- styles[[type]]
    if (!is.null(s)) {
      x <- s(x)
    }
    message(x)
  }

  list(add = add, get = get, show = show)
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
                error_continue = ".",
                warning_continue = ".",
                note_continue = ".")
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
format.compiler_output <- function(x, ..., use_colour = TRUE) {
  styles <- compiler_output_styles(use_colour)

  type <- x$type
  value <- x$value

  for (i in seq_along(type)) {
    t <- type[[i]]
    s <- styles[[t]]
    if (!is.null(s)) {
      v <- value[[i]]
      if (length(v) > 1) {
        sc <- styles[[paste0(t, "_continue")]]
        value[[i]] <- c(s(v[[1L]]), sc(v[-1L]))
      } else {
        value[[i]] <- s(v)
      }
    }
  }

  paste(sprintf("%s", unlist(value, use.names = FALSE)), collapse = "\n")
}

##' @export
print.compiler_output <- function(x, ..., use_colour = TRUE) {
  str <- format.compiler_output(x, use_colour = use_colour)
  cat(paste0(str, "\n"))
}

classify_compiler_output <- function(txt, use_colour = FALSE) {
  f <- compiler_classifier(use_colour)
  for (i in txt) {
    f$add(i)
  }
  f$get()
}
