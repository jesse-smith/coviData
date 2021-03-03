#' Download "Contacts for State Monitoring" Report
#'
#' `download_state_contacts()` saves the "Contacts for State Monitoring" report
#' for transformation and upload to the Tennessee DoH REDcap.
#'
#' @param date The date to use in the file name
#'
#' @param api_token API token for integrated data tool
#'
#' @param directory Directory to save under; should usually be left alone.
#'
#' @param force Should the new file overwrite any existing files?
#'
#' @export
download_state_contacts <- function(
  date = Sys.Date(),
  api_token = Sys.getenv("redcap_NIT_token"),
  directory = "V:/State Contact Monitoring Spreadsheets/REDcap Exports",
  force = FALSE
) {
  download_interview_report(
    date = date,
    api_token = api_token,
    report_id = "83",
    directory = directory,
    new_file  = paste0("state_contact_monitoring_", date, ".csv"),
    force = force
  )
}
