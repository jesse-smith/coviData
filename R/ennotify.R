
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
#' There are several options used by `ennotify()` to customize output. The
#' easiest way to set these options is to use `ennotify_set_options()`.
#'
#' The first is `"ennotify_to"`; this is a character vector containing
#' recipient email addresses for error notifications. This can be set most
#' easily using `ennotify_to(to = your_recipients)`, and retrieved most easily
#' using `ennotify_to()` with no arguments.
#'
#' The second is `"ennotify_context"`; this allows the user to fill in the
#' context in which the error occurred. This should make sense in the sentence
#' "`foo()` in {ennotify_context} encountered an error." This can be set most
#' easily using `ennotify_context(context = your_context)`, and retrieved most
#' easily using `ennotify_context()` with no arguments.
#'
#' The third is `ennotify_inform`; this allows easy switching of message
#' printing for `ennotify_context()`. This is mostly useful for printing logging
#' messages. This can be set most easily using `ennotify_inform(TRUE)` or
#' `ennotify_inform(FALSE)`, and retrieved with `ennotify_inform()`.
#'
#' If `"ennotify_to"` is unset, the function currently emails me (Jesse Smith)
#' every time an error is processed, so please **do not** use this before
#' consulting me first. This function is only exported to allow usage in scripts
#' and other Shelby County R packages.
#'
#' In the event of an internal error, the error will be printed to stderr, but
#' execution will continue.
#'
#' @param ... Character vectors of recipient email addresses
#'
#' @param context Character. The context for an error, if one occurs.
#'
#' @param inform Should `ennotify_context()` print a message to stderr when
#'   called?
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
ennotify_set_options <- function(
  ...,
  .inform = TRUE,
  .backtrace = c("full", "collapse", "branch", "reminder")
) {
  try({
    to <- try(rlang::flatten_chr(rlang::list2(...)))
    .backtrace <- try(rlang::arg_match(.backtrace)[[1L]])

    coviData::ennotify_context(
      paste0("setting `ennotify_inform = ", .inform, "`"),
      inform = .inform
    )
    p_inform <- coviData::ennotify_inform(.inform)

    coviData::ennotify_context("setting `ennotify()` as error handler")
    p_error <- options(error = quote(coviData::ennotify()))

    coviData::ennotify_context(
      paste0("setting `rlang_backtrace_on_error = '", .backtrace, "'`")
    )
    p_backtrace <- options(rlang_backtrace_on_error = .backtrace)

    coviData::ennotify_context(paste0("setting `ennotify_to` to: ", to))
    p_to <- coviData::ennotify_to(...)

    invisible(try(c(p_backtrace, p_error, p_inform, p_to)))
  })
}

#' @rdname ennotify
#'
#' @export
ennotify_to <- function(...) {
  try({
    dots_empty <- rlang::dots_n(...) == 0L
    if (dots_empty) return(getOption("ennotify_to"))

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
    if (rlang::is_missing(context)) return(getOption("ennotify_context"))

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
    if (rlang::is_missing(inform)) return(getOption("ennotify_inform"))

    coviData::assert_any(
      rlang::is_bool(inform),
      is.null(inform),
      message = "`ennotify_inform` must be boolean or `NULL`"
    )

    invisible(options(ennotify_inform = inform))
  })
}

try_notify <- function(expr) {
  tryCatch(eval(expr), error = cnd_notify)
}

cnd_notify <- function(cnd) {
  cnd <- rlang::cnd_entrace(cnd)
  trace <- cnd[["trace"]]
  notify(
    to = err_notify_to(),
    subject = err_notify_subject(trace = trace),
    body = err_notify_body(trace = trace)
  )
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
