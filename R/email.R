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
#' @param importance The importance (priority, etc) to attach to the email. The
#'   default is `"Normal"`; alternatives are `"Low"` or `"High"`.
#'
#' @keywords internal
#'
#' @export
notify <- function(
  to,
  subject,
  body = NULL,
  cc = NULL,
  html = FALSE,
  importance = c("Normal", "Low", "High")
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

    email[["importance"]] <- purrr::when(
      standardize_string(importance[[1]]),
      . == "Low" ~ "1",
      . == "High" ~ "3",
      ~ "2"
    )

    email$Send()

    remove(outlook, email)
  })

  withr::with_namespace("RDCOMClient", eval(send_email), warn.conflicts = FALSE)
}
