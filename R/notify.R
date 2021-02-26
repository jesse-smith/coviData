#' Send an Email Notification with blastula
#'
#' `notify` sends a notification email using a Visual Basic interface to Outlook
#'
#' @param to Character. The recipient(s) of the email.
#'
#' @param subject Character. The subject of the email.
#'
#' @param body Character. The body of the email. If `html = FALSE`, this should
#'   be plain text. If `html = TRUE`, this should be HTML text.
#'
#' @param fail Should failure cause an error?
#'
#' @param html Is the `body` HTML or plain text? All messages are sent in HTML;
#'   this argument determines how messages are parsed.
#'
#' @keywords internal
#'
#' @export
notify <- function(
  to,
  subject = "",
  body = "",
  html = FALSE,
  fail = FALSE
) {

  if (!is_windows()) {
    msg <- paste0(
      "`notify()` currently only works on Windows",
      " due to dependencies on Outlook and Visual Basic"
    )
    rlang::abort(msg)
  }

  temp_script <- fs::file_temp("script_", ext = "vbs")
  on.exit(fs::file_delete(temp_script), add = TRUE)

  mail_to <- paste0("mail.Recipients.Add(\"", to, "\")", collapse = "\n  ")

  if (html) {
    body <- html_to_vb(body, collapse = " ")
  } else {
    body <- str_to_vb_html(body, collapse = "<br>")
  }

  mail_body <- stringr::str_glue("mail.HTMLBody = \"{body}\"")

  script_contents <- stringr::str_glue(
    "With CreateObject(\"Outlook.Application\")",
    "  Set mail = .CreateItem(olMailItem)",
    "  mail.Subject = \"{subject}\"",
    "  {mail_to}",
    "  mail.Recipients.ResolveAll()",
    "  {mail_body}",
    "  mail.Send()",
    "End With",
    .sep = "\n"
  )

  writeLines(script_contents, con = temp_script)

  run_vbs <- rlang::expr({
    capture.output(system2("cscript.exe", temp_script)) %>%
      stringr::str_remove_all("^\\[[0-9]+\\]") %>%
      stringr::str_flatten("\n") %>%
      rlang::inform(class = "cmd_output")
  })

  if (fail) eval(run_vbs) else try(eval(run_vbs))
}

#' `entrace()` + `notify()`
#'
#' @description
#' `ennotify()` adds a backtrace to base R errors and sends a notification
#' email with the failing function and a full backtrace. However, it cannot
#' capture the condition object associated with the error, and thus cannot
#' show error messages. Logging errors is highly recommended.
#'
#' The helpers `ennotify_to()` and `ennotify_context()` assist in setting
#' global parameters to make your message more informative (and go to the right
#' people!). See below for details.
#'
#' Set `ennotify()` as your error handler using
#'
#' `options(error = quote(coviData::ennotify()),`
#' ` rlang_backtrace_on_error = "full")`
#'
#' There are two options used by `ennotify()` to customize output.
#'
#' The first is `"ennotify_to"`; this is a character vector containing
#' recipient email addresses for error notifications. This can be set most
#' easily using `ennotify_to(to = your_recipients)`, and retrieved most easily
#' using `ennotify_to()` with no arguments
#'
#' The second is `"ennotify_context"`; this allows the user to fill in the
#' context in which the error occurred. This should make sense in the sentence
#' "`foo()` in {ennotify_context} encountered an error." This can be set most
#' easily using `ennotify_context(context = your_context)`, and retrieved most
#' easily using `ennotify_context()` with no arguments.
#'
#' `ennotify_inform()` allows easy switching of message printing for
#' `ennotify_context()`. This is mostly useful for printing logging messages.
#'
#' If `"ennotify_to"` is unset, the function currently emails me (Jesse Smith)
#' every time an error is processed, so please **do not** use this before
#' consulting me first. This function is only exported to allow usage in scripts
#' and other Shelby County R packages.
#'
#' In the event of an internal error (within the `ennotify()` function), the
#' error will be printed to stderr, but execution will continue.
#'
#' @param ... Character vectors of recipient email addresses
#'
#' @param context Character. The current error context, if one occurs.
#'
#' @param inform Should `ennotify_context()` print a message when called?
#'
#' @keywords internal
#'
#' @export
ennotify <- function() {

  # Add traceback if needed
  rlang::entrace()
  coviData::notify(
    to = err_notify_to(),
    subject = err_notify_subject(),
    body = err_notify_body()
  )
}

#' @rdname ennotify
#'
#' @export
ennotify_to <- function(...) {
  try({
    dots_empty <- rlang::dots_n(...) == 0L
    if (dots_empty) return(options("ennotify_to")[[1L]])

    to <- unlist(rlang::list2(...))
    coviData::assert_any(
      all(is.character(to), stringr::str_detect(to, ".+@[^.]+[.].+")),
      is.null(to),
      message = paste(
        "`ennotify_to` must be either a character vector of email addresses",
        "or `NULL`"
      )
    )

    invisible(options(ennotify_to = to))
  })
}

#' @rdname ennotify
#'
#' @export
ennotify_context <- function(context, inform = ennotify_inform()) {
  try({
    if (rlang::is_missing(context)) return(options("ennotify_context")[[1L]])

    coviData::assert_any(
      is.character(context),
      is.null(context),
      message = "`ennotify_context` must be either a character string or `NULL`"
    )

    inform <- rlang::is_true(inform)
    if (inform) rlang::inform(paste0(stringr::str_to_sentence(context), "..."))
    invisible(options(ennotify_context = context))
  })
}

#' @rdname ennotify
#'
#' @export
ennotify_inform <- function(inform) {
  try({
    if (rlang::is_missing(inform)) return(options("ennotify_inform")[[1L]])

    coviData::assert_any(
      rlang::is_true(inform),
      rlang::is_false(inform),
      is.null(inform),
      message = "`ennotify_inform` must be `TRUE`, `FALSE`, `NULL`"
    )

    invisible(options(ennotify_inform = inform))
  })
}

try_notify <- function(expr) {
  tryCatch(eval(expr), error = cnd_notify)
}

cnd_notify <- function(cnd) {
  cnd <- rlang::cnd_entrace(cnd)
  trace <- cnd$trace
  notify(
    to = err_notify_to(),
    subject = err_notify_subject(trace = trace),
    body = err_notify_body(trace = trace)
  )
}

#' Convert String to HTML Suitable for Inclusion in VB Script
#'
#' @param string A character vector
#'
#' @param collapse An optional character string to separate the results. Not
#'   `NA_character_`.
#'
#' @return An HTML string
str_to_vb_html <- function(string, collapse = NULL) {
  string %>%
    paste0(collapse = collapse) %>%
    stringr::str_replace_all("[\r]?\n", replacement = "<br>") %>%
    stringr::str_replace_all("\t", replacement = "    ") %>%
    stringr::str_replace_all("\"", replacement = "\"\"")
}

html_to_vb <- function(html, collapse = NULL) {
  html %>%
    paste0(collapse = collapse) %>%
    stringr::str_squish() %>%
    stringr::str_replace_all("\"", replacement = "\"\"")
}

str_trace <- function(trace) {
   paste0(capture.output(print(trace)), "\n", collapse = "")
}

str_trace_fn <- function(trace) {
  paste0(rlang::call_name(trace[["calls"]][[1L]]), "()")
}

err_notify_to <- function(
  to = ennotify_to(),
  default = "Jesse.Smith@shelbycountytn.gov"
) {
  try((if (rlang::is_empty(to)) default else to))
}

err_notify_subject <- function(
  context = ennotify_context(),
  trace = rlang::last_error()[["trace"]]
) {
  try({
    if (rlang::is_empty(context)) context <- str_trace_fn(trace)
    paste0("Error in `", context, "` at ", format(Sys.time(), "%H:%M %Y-%m-%d"))
  })
}

err_notify_body <- function(
  context = ennotify_context(),
  trace = rlang::last_error()[["trace"]]
) {
  try({
    fn <- str_trace_fn(trace)
    trace_string <- str_trace(trace)
    in_ <- if (rlang::is_empty(context)) NULL else " in "

    paste0(
      "`", fn, "` encountered the following error ", in_, context, ":",
      "\n\n",
      geterrmessage(),
      "\n\n",
      "The full traceback is:\n",
      trace_string,
      "\n\n",
      "This error should have been logged; ",
      "see the associated log file for details.",
      "\n\n",
      "Note: This message was generated automatically."
    )
  })
}
