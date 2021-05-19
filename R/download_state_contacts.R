#' Download "Automated Contacts for State Monitoring" Report
#'
#' `download_state_contacts()` saves the
#' "Automated Contacts for State Monitoring" report for transformation and
#' upload to the Tennessee DoH REDcap.
#'
#' @param date The date to use in the file name
#'
#' @param api_token API token for integrated data tool
#'
#' @param directory Directory to save under; should usually be left alone.
#'
#' @param report_id REDcap ID of report
#'
#' @param force Should the new file overwrite any existing files?
#'
#' @export
download_state_contacts <- function(
  date = Sys.Date(),
  api_token = Sys.getenv("redcap_NIT_token"),
  directory = "V:/State Contact Monitoring Spreadsheets/REDcap Exports",
  report_id = "176",
  force = FALSE
) {

  report_id <- rlang::arg_match(report_id)[[1L]]

  download_interview_report(
    date = date,
    api_token = api_token,
    report_id = report_id,
    headers = "label",
    directory = directory,
    new_file  = paste0("state_contact_monitoring_", date, ".csv"),
    force = force
  )
}
