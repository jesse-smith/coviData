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
    stringr::str_replace_all("[\"]", replacement = "\"\"")
}

html_to_vb <- function(html, collapse = NULL) {
  html %>%
    paste0(collapse = collapse) %>%
    stringr::str_squish() %>%
    stringr::str_replace_all("\"", replacement = "\"\"")
}
