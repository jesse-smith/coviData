#' Send an Email Notification with blastula
#'
#' `notify` sends a notification email using a Visual Basic interface to Outlook
#'
#' @param to Character. The recipient(s) of the email.
#'
#' @param subject Character. The subject of the email.
#'
#' @param body Character. The body of the email. If `HTML = FALSE`, this should
#'   be plain text. If `HTML = TRUE`, this should be HTML text.
#'
#' @param html Should the body of the email be rendered as HTML?
#'
#' @param fail Should failure cause an error?
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
    mail_body <- stringr::str_glue("mail.HTMLBody = \"{body}\"")
  } else {
    body <- vb_newline_notify(body)
    mail_body <- stringr::str_glue("mail.Body = \"{body}\"")
  }

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

  readr::write_lines(script_contents, file = temp_script, na = "")

  run_vbs <- rlang::expr(
    capture.output(system2("cscript.exe", temp_script)) %>%
      stringr::str_remove_all("^\\[[0-9]+\\]") %>%
      stringr::str_flatten("\n") %>%
      rlang::inform(class = "cmd_output")
  )

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
  try({
    # Add traceback if needed
    rlang::entrace()

    # Get traceback
    trace <- rlang::trace_back()

    # Get function from objects
    fn <- trace[["calls"]][[1L]] %>% rlang::call_name() %>% paste0("()")

    trace_string <- capture.output(print(trace)) %>% paste0("\n", collapse = "")

    # Send notification
    to <- ennotify_to()
    if (rlang::is_empty(to)) {
      to <- "jesse.smith@shelbycountytn.gov"
    }

    context <- ennotify_context()
    if (rlang::is_empty(context)) {
      in_ <- ""
      context <- ""
      subject <- paste0(
        "Error in `", fn, "` at ", format(Sys.time(), "%H:%M %Y-%m-%d")
      )
    } else {
      in_ <- " in "
      subject <- paste0(
        "Error", in_, context, " at ", format(Sys.time(), "%H:%M %Y-%m-%d")
      )
    }

    body <- paste0(
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

    coviData::notify(
      to = to,
      subject = subject,
      body = body
    )
  })
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

#' Replace LF and CRLF with VB Constants for `notify()`
#'
#' @param string A string to perform replacement on
#'
#' @return The string with (CR)LF replaced with "& VbCrLf &"
vb_newline_notify <- function(string) {
  string %>%
    stringr::str_replace_all("[\r]?[\n]", replacement = "\" & VbCrLf & \"") %>%
    stringr::str_remove_all("\"{2}") %>%
    stringr::str_replace_all("\\s+([&]\\s*)+\\s+", replacement = " & ") %>%
    stringr::str_replace_all("[ ]{2,}", replacement = " ")
}
