#' Start a Logging Session
#'
#' `log_start()` begins diverting message stream (stderr) output to the
#' specified log file. Only one logging session can be active at a time.
#'
#' @param file The log file name. If path components are included they are
#'   assumed to be relative to `dir`.
#'
#' @param dir The log directory. `log_dir()`, the default, returns the current
#'   log directory, or `NULL` if none is set. The latter case will cause `file`
#'   to be created in or relative to your current working directory.
#'
#' @param append Should an existing file be appended to?
#'
#' @param force If `append = FALSE`, should an existing file be overwritten?
#'
#' @return The path to the log file (invisibly)
#'
#' @export
log_start <- function(
  file,
  dir = log_dir(),
  append = TRUE,
  force = FALSE
) {
  # Check args
  file <- path_create(dir, file)
  append <- assert_bool(append)
  force <- assert_bool(force)

  file_exists <- fs::file_exists(file)

  # Check that a sink is not already open
  assert_all(
    sink.number("message") == 2L,
    message = paste0(
      "A message sink is already open; starting a log would invalidate ",
      "this sink."
    )
  )

  # Check that `file` doesn't already exist if `append = FALSE`
  assert_any(
    append,
    !file_exists,
    force,
    message = paste0(
      "A file already exists at the location specified by `file`. ",
      "To append to this file, set `append = TRUE` (the default). ",
      "To overwrite this file, set `force = TRUE`."
    )
  )

  # Create `file` (if it doesn't already exist) and open a connection
  fs::file_create(file)
  open <- if (append) "at" else "wt"

  con <- tryCatch(
    file(file, open = open, encoding = "UTF-8"),
    error = function(error) {
      if (!file_exists) fs::file_delete(file)
      rlang::cnd_signal(error)
    }
  )
  # Set `.log_connection` option to handle closing later
  options(.log_connection_previous_ = getOption(".log_connection_"))
  options(.log_connection_ = con)

  # Create sink for stderr stream; if this fails for some reason, clean up.
  tryCatch(
    sink(con, append = append, type = "message", split = FALSE),
    error = function(error) {
      close(con)
      if (!file_exists) fs::file_delete(file)
      rlang::cnd_signal(error)
    }
  )
  # Set `.log_sink` option to handle closing later
  options(.log_sink_previous_ = getOption(".log_sink_"))
  options(.log_sink_ = TRUE)

  invisible(file)
}

#' End a Logging Session
#'
#' `log_end()` ends a logging session by closing both the message sink and the
#' file connection to the log file.
#'
#' @param sink The message sink used to divert stderr output to the log file
#'
#' @param con The open file connection to the log file
#'
#' @return The path to the log file (invisibly)
#'
#' @export
log_end <- function(
  sink = getOption(".log_sink_"),
  con = getOption(".log_connection_")
) {
  # Check args

  # If args are set to their defaults, check whether either are `NULL`
  sink_expr <- rlang::enexpr(sink)
  con_expr  <- rlang::enexpr(con)
  if (sink_expr == rlang::expr(getOption(".log_sink_"))) {
    assert(
      !is.null(sink),
      message = paste0(
        rlang::expr_label(sink_expr), " == NULL; ",
        "do you have a logging session (and associated sink) open?"
      )
    )
  }
  if (con_expr == rlang::expr(getOption(".log_connection_"))) {
    assert(
      !is.null(con),
      message = paste0(
        rlang::expr_label(con_expr), " == NULL; ",
        "do you have a logging session (and associated log file) open?"
      )
    )
  }

  sink <- assert_bool(sink)
  assert_all(
    rlang::inherits_all(con, c("file", "connection")),
    isOpen(con),
    message = paste0(
      "`con` must be an open file connection.\n",
      "If you have started a logging session with `log_start()`, ",
      "this should be supplied by default."
    )
  )

  # End message sink diversion
  if (sink.number("message") == 2L) {
    rlang::warn(
      "no message sink currently open; skipping ending of message diversion"
    )
  } else {
    sink(type = "message")
  }

  # Get path to log file for return value
  path <- path_create(summary(con)[["description"]])

  # Close connection to log file
  tryCatch(
    close(con),
    error = function(error) {
      rlang::abort(
        paste0(
          "Could not close log file in `log_end()`:\n",
          geterrmessage()
        ),
        trace = rlang::trace_back(top = rlang::env_parent())
      )
    }
  )

  invisible(path)
}

#' Set/Get Logging Directory
#'
#' `log_dir()` gets and sets the `.log_dir` option and the `coviData_log_dir`
#' environment variable. The latter can be set persistently on Windows using
#' `persistent = TRUE`.
#'
#' @param dir The path to the log directory. If missing, `log_dir` returns the
#'   current value of `.log_dir`
#'
#' @param persistent Should the `coviData_log_dir` environment variable persist
#'   after ending the current R session?
#'
#' @return The current logging directory if `dir` is missing, or a named list
#'   of the old logging directory if used to change the logging directory
#'
#' @export
log_dir <- function(dir, persistent = FALSE) {
  if (rlang::is_missing(dir)) {
    log_dir <- getOption(".log_dir")
    coviData_log_dir <- Sys.getenv("coviData_log_dir")
    if (coviData_log_dir == "") coviData_log_dir <- NULL
    if (is.null(log_dir)) log_dir <- coviData_log_dir
    return(log_dir)
  }

  assert_all(
    rlang::is_scalar_character(dir),
    message = "`dir` must be a (scalar) string"
  )
  dir <- gsub("/", "\\\\",  path_create(dir))

  if (persistent && is_windows()) {
    system(paste0('setx coviData_log_dir "', dir, '"'))
  } else if (persistent) {
    rlang::warn(
      paste(
        "Log directory not set persistently;",
        "this functionality is currently only available on windows"
      )
    )
  }
  Sys.setenv(coviData_log_dir = dir)

  invisible(options(.log_dir = path_create(Sys.getenv("coviData_log_dir"))))
}
