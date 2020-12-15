# The following was modified from https://stackoverflow.com/questions/26811679/sending-email-in-r-via-outlook

#' Send an Email Notification with Outlook
#'
#' `notify` sends a notification email via Outlook. This assumes that Outlook
#' is installed on your machine and that you've already set up an account.
#'
#' @param to A character vector containing recipient email addresses. Required.
#'
#' @param subject A character string containing the email subject. Required.
#'
#' @param body A character string containing the email body; if `html = TRUE`,
#'   this will be parsed as HTML. Optional.
#'
#' @param cc A character vector containing email address to "cc". Optional.
#'
#' @param html Should the `body` be treated as plain text or HTML? The default
#'   is plain text (`FALSE`).
#'
#' @keywords internal
#'
#' @export
notify <- function(
  to,
  subject,
  body = NULL,
  cc = NULL,
  html = FALSE
) {

  # Withr and RDCOMClient are suggested for coviData but necessary for
  # `notify()`
  rdcom_ns <- requireNamespace("RDCOMClient", quietly = TRUE)
  withr_ns <- requireNamespace("withr", quietly = TRUE)

  if (!rdcom_ns) {
    rdcom_message = paste0(
      "`notify()` requires the RDCOMClient package to be installed. ",
      "To install this package, run:\n\n",
      "`install.packages(",
        "'RDCOMClient', ",
        "repos = 'http://www.omegahat.net/R'",
      ")`\n"
    )
  } else {
    rdcom_message <- ""
  }

  if (!withr_ns) {
    withr_message <- paste0(
      "`notify()` requires the withr package to be installed. ",
      "To install this package, run:\n\n",
      "`install.packages('withr')`\n"
    )
  } else {
    withr_message = ""
  }

  assertthat::assert_that(
    rdcom_ns, withr_ns,
    msg = paste0(rdcom_message, withr_message)
  )

  send_email <- rlang::expr({

    to <- stringr::str_flatten(to, "; ")
    cc <- if (rlang::is_empty(cc)) "" else stringr::str_flatten(cc, "; ")
    body <- if (rlang::is_empty(body)) "" else body

    outlook <- RDCOMClient::COMCreate("Outlook.Application")

    email <- outlook$CreateItem(0)
    email[["to"]] <- to
    email[["cc"]] <- cc
    email[["subject"]] <- subject

    if (rlang::is_true(html)) {
      email[["htmlbody"]] <- body
    } else {
      email[["body"]] <- body
    }

    email$Send()

    remove(outlook, email)
  })

  withr::with_namespace("RDCOMClient", eval(send_email), warn.conflicts = FALSE)
}

#' Send an Email Notification on Error
#'
#' `error_notify()` generates an error handler that sends an email notification
#' when an expression generates an error. It relies on
#' \code{\link[coviData:notify]{notify()}} and can optionally avoid terminating
#' the larger expression it was called from. This is useful for running scripts
#' with independent components.
#'
#' @inheritParams notify
#'
#' @param abort Should execution terminate after reporting the error?
#'
#' @return The input error condition, with a backtrace
#'
#' @keywords internal
#'
#' @export
error_notify <- function(
  abort = TRUE,
  to = "jesse.smith@shelbycountytn.gov",
  .call = NULL,
  subject = NULL,
  body = NULL,
  cc = NULL,
  html = FALSE,
  operation = NULL
) {
  
  purrr::walk(rlang::fn_fmls_syms(), ~ force(.x))

  args <- rlang::fn_fmls() %>% extract(rlang::fn_fmls_names(notify))

  force(args)

  function(.error) {
  
    .trace <- .error[["trace"]]
    
    .bottom <- vctrs::vec_size(.trace)
    
    if (!is.null(.call)) {
      # Do nothing
    } else if (!is.null(operation)) {
      .call <- operation
    } else if (!is.null(.trace)) {
      .call <- .trace[[.bottom]]
    } else {
      .call <- .error[["call"]]
    }

    br <- if (rlang::is_true(html)) "<br>" else "\n"
    
    if (is.null(args$subject) {
      args$subject <- paste(
        .call, "in", error_file, "Failed at", Sys.time()
      )
    }

    if (is.null(args$body)) {
      args$body <- paste0(
        "The following error occurred while executing ", .call, ":", br, br,
        .error$message, br, br,
        "See the associated log for details."
      )
    }

    eval(rlang::call2(notify, !!!args))

    generate_error <- rlang::expr({
      if (rlang::is_true(abort)) {
        rlang::cnd_signal(.error)
      } else {
        try(rlang::cnd_signal(.error))
      }
    })

    withr::with_options(
      list(rlang_backtrace_on_error = "branch", error = rlang::entrace),
      eval(generate_error)
    )

  }
}

option_error <- function() {

  nframe <- sys.nframe() - 1L
  info <- rlang:::signal_context_info(nframe)
  bottom <- sys.frame(info[[2L]])
  
  
  trace <- rlang::trace_back(bottom = bottom)
  
  if (rlang::trace_length(trace) <= 0L) {
    return(NULL)
  }
  
  stop_call <- sys.call(-1L)
  stop_frame <- sys.frame(-1L)

  cnd <- stop_frame[["cond"]]

  from_stop <- rlang::is_call(
    stop_call,
    name = "stop",
    ns = c("", "base")
  )

  if (from_stop && inherits(cnd, "rlang_error")) {
    return(NULL)
  } else if (from_stop && is.null(cnd)) {
    msg_call <- quote(.makeMessage(..., domain = domain))
    msg <- rlang::eval_bare(msg_call, stop_frame)
  } else if (from_stop) {
    msg <- cnd$message
  } else {
    msg <- geterrmessage()
  }

  err <- rlang::error_cnd(
    message = msg,
    error = cnd,
    trace = trace,
    parent = cnd
  )

  backtrace_lines <- rlang:::format_onerror_backtrace(err)

  if (vctrs::vec_size(backtrace_lines) > 0L) {
    rlang:::cat_line(backtrace_lines)
  }
  NULL
}
