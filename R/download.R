#' Download Data from the Integrated Data Tool REDcap Project
#'
#' \code{download_integrated_data} connects to the Integrated Data Tool REDcap
#' project via an API token and downloads the data currently available in the
#' \strong{case interviews} report.
#'
#' \code{download_integrated_data} downloads data as above and saves to the file
#' directory and file specified in \code{directory} and \code{new_file} (by
#' default, it saves to directory
#' \code{V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/Integrated data
#' tool Case Interviews/} and file \code{integrated_data_YYYY-MM-DD.csv}.) Only
#' change this if you're doing some custom analysis; by default,
#' \code{\link{load_integrated_data}} will look here for data to import.
#'
#' To use \code{download_integrated_data} you'll need an API access key for the
#' associated REDcap project. Please contact
#' \email{jesse.smith@@shelbycountytn.gov} or
#' \email{karim.gilani@@shelbycountytn.gov} for more information.
#'
#' Best practice is to save this API access token in a separate file; see
#' \link{env_vars} for details.
#'
#' @inherit download_interview_report params return
#'
#' @export
download_integrated_data <- function(
  date = Sys.Date(),
  api_token = Sys.getenv("redcap_NIT_token"),
  directory = paste0(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
     "Integrated data tool Case Interviews/data/"
  ),
  new_file = paste0("integrated_data_", date, ".csv"),
  force = FALSE
) {
  message("Starting Integrated Data download...\n")
  download_interview_report(
    date = date,
    api_token = api_token,
    report_id = "81",
    directory = directory,
    new_file = new_file,
    force = force
  )
  message("Finished Integrated Data download.")

  invisible(path_create(directory, new_file))
}
