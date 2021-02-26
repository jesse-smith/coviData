log_start <- function(
  file,
  append = TRUE,
  force = FALSE
) {
  # Check args
  file <- path_create(file)
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
      rlang::cnd_signal(cnd)
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
}


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
}
