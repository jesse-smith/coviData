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
#' \email{umar.kabir@@shelbycountytn.gov} for more information.
#'
#' Best practice is to save this API access token in a separate file; see
#' \link{using-renviron} for details.
#'
#' @inherit download_interview_report params return
#'
#' @export
download_integrated_data <- function(
  date = Sys.Date(),
  api_token = Sys.getenv("redcap_IDT_token"),
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
    report_id = "12314",
    directory = directory,
    new_file = new_file,
    force = force
  )
  message("Finished Integrated Data download.")

  invisible(path_create(directory, new_file))
}

#' Download AEL File from Serv-U SFTP Site
#'
#' \code{download_ael} finds and download the AEL data for the input date and
#' saves it to a folder of your choice. To make the best use of this function,
#' you should save your username and password for Serv-U in a ".Renviron" file;
#' see
#'
#' @param date The creation date of the Serv-U file
#'
#' @param usr The username for Serv-U; the default pulls from a ".Renviron" file
#'   and is the recommended setup
#'
#' @param pwd The password for Serv-U; the default pulls from a ".Renviron" file
#'   and is the recommended setup
#'
#' @param directory The directory in which the new file will be saved
#'
#' @export
download_ael <- function(
  date = Sys.Date(),
  usr = Sys.getenv("sftp_usr"),
  pwd = Sys.getenv("sftp_pwd"),
  directory = "V:/EPI DATA ANALYTICS TEAM/AEL Data/"
) {
  download_servu(
    date = date,
    usr = usr,
    pwd = pwd,
    remote_dir = "AEL",
    local_dir = directory,
    new_file  = NULL
  )
}
