#' Send an Email Notification with Outlook
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
#' @param html Is the `body` HTML or plain text? All messages are sent in HTML;
#'   this argument determines how messages are parsed.
#'
#' @param fail Should failure cause an error?
#'
#' @param attach Character vector of paths to files (or `NULL`)
#'
#' @keywords internal
#'
#' @export
notify <- function(
  to,
  subject = "",
  body = "",
  html = FALSE,
  fail = FALSE,
  attach = NULL
) {

  if (!is_windows()) {
    msg <- paste0(
      "`notify()` currently only works on Windows",
      " due to dependencies on Outlook and Visual Basic"
    )
    rlang::abort(msg)
  }

  # Check arguments
  coviData::assert(
    rlang::is_character(to),
    message = "`to` must be a `character` vector of email addresses"
  )
  coviData::assert(
    rlang::is_string(subject),
    message = "`subject` must be a string"
  )
  coviData::assert(
    rlang::is_character(body),
    message = "`body` must be a `character` vector"
  )
  coviData::assert_bool(html)
  coviData::assert_bool(fail)

  attach <- path_create(attach)
  coviData::assert_all(
    rlang::is_true(rlang::is_character(attach)),
    vec_is_empty(attach) || rlang::is_true(as.vector(fs::file_exists(attach))),
    message = paste0(
      "`attach` must be a `character` vector of existing file paths or `NULL`"
    )
  )

  temp_script <- fs::file_temp("script_", ext = "vbs")
  on.exit(fs::file_delete(temp_script), add = TRUE)

  mail_to <- paste0("mail.Recipients.Add(\"", to, "\")", collapse = "\n  ")

  if (html) {
    body <- html_to_vb(body, collapse = " ")
  } else {
    body <- str_to_vb_html(body, collapse = "<br>")
  }

  mail_body <- stringr::str_glue("mail.HTMLBody = \"{body}\"")

  if (vec_is_empty(attach)) {
    attms_add <- ""
  } else {
    attach <- path_create(attach)
    attms_add <- paste0(
      "attms.Add \"", path_create(attach), "\"",
      collapse = "\n  "
    )
  }

  script_contents <- stringr::str_glue(
    "With CreateObject(\"Outlook.Application\")",
    "  Set mail = .CreateItem(olMailItem)",
    "  mail.Subject = \"{subject}\"",
    "  {mail_to}",
    "  mail.Recipients.ResolveAll()",
    "  {mail_body}",
    "  Set attms = mail.Attachments",
    if (!is.null(attach)) "  {attms_add}" else "",
    "  mail.Send()",
    "End With",
    .sep = "\n"
  )

  writeLines(script_contents, con = temp_script)

  run_vbs <- rlang::expr({
    utils::capture.output(system2("cscript.exe", temp_script)) %>%
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
