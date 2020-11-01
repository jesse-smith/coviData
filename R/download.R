#' Download Data from the Integrated Data Tool REDcap Project
#'
#' \code{download_integrated_data} connects to the Integrated Data Tool REDcap
#' project via an API token and downloads the data currently available in the
#' \emph{case interviews} report.
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
#' \link{using_Renviron} for details.
#'
#' @param date The date with which to name \code{new_file}
#'
#' @param api_token The API access token for the Integrated Data Tool REDcap
#'   project. This should be stored in a \emph{.Renviron} file; see
#'   \link{using_Renviron} for details.
#'
#' @param directory A string specifying the directory in which to save the data;
#' this should usually be left alone
#'
#' @param new_file A string specifying the file name to save the data under;
#' this should always end in \emph{.csv} and should usually be left alone
#'
#' @param convert A logical indicating whether to open in Excel for conversion
#'   to \emph{.xlsx} format (or another file format)
#'
#' @param force A logical indicating whether to overwrite an existing file with
#'   the same date; the default is \code{FALSE} and should usually be left alone
#'
#' @return Invisibly returns \code{NULL}
#'
#' @export
download_integrated_data <- function(
  date = Sys.Date(),
  api_token = Sys.getenv("redcap_IDT_token"),
  directory = paste0(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
    "Integrated data tool Case Interviews/"
  ),
  new_file  = paste0("integrated_data_", date, ".csv"),
  convert   = FALSE,
  force     = FALSE
) {
  message("Starting Integrated Data download...\n")
  download_redcap_report(
    date = date,
    api_token = api_token,
    report_id = "12314",
    directory = directory,
    new_file = new_file,
    convert = convert,
    force = force
  )
  message("Finished Integrated Data download.")
}

#' Download Snapshot of NBS Data Posted on REDcap
#'
#' @export
download_nbs_snapshot <- function(
  date = Sys.Date(),
  api_token = Sys.getenv("redcap_DFR_token"),
  redcap_file = "nbs_daily_upload",
  directory = paste0(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
    "Sandbox data pull Final/"
  ),
  new_file = paste0(date, " Final Data Pull.csv"),
  convert = FALSE,
  force = FALSE
) {
  message("Starting NBS snapshot download...\n")
  download_redcap_file(
    date = date,
    api_token = api_token,
    redcap_file = redcap_file,
    directory = directory,
    new_file = new_file,
    convert = convert,
    force = force
  )
  message("\nFinished NBS snapshot download.")
}

#' Download Snapshot of NBS PCR Test Data Posted on REDcap
#'
#' @export
download_pcr_snapshot <- function(
  date = Sys.Date(),
  api_token = Sys.getenv("redcap_DFR_token"),
  redcap_file = "lab_pcr",
  directory = "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/MSR PCR/",
  new_file = paste0("MSR - All PCRs_", format(Sys.Date(), "%m%d%Y"), ".csv"),
  convert = FALSE,
  force = FALSE
) {
  message("Starting PCR snapshot download...\n")
  download_redcap_file(
    date = date,
    api_token = api_token,
    redcap_file = redcap_file,
    directory = directory,
    new_file = new_file,
    convert = convert,
    force = force
  )
  message("\nFinished PCR snapshot download.")

  invisible(NULL)
}

#' Download Snapshot of NBS PCR Test Data Posted on REDcap
#'
#' @export
download_antigen_snapshot <- function(
  date = Sys.Date(),
  api_token = Sys.getenv("redcap_DFR_token"),
  redcap_file = "lab_antigen",
  directory =
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/MSR ANTIGEN/"
  ,
  new_file = paste0(
    "MSR - All Antigens_",
    format(Sys.Date(), "%m%d%Y"),
    ".csv"
  ),
  convert = FALSE,
  force = FALSE
) {
  message("Starting antigen snapshot download...\n")
  download_redcap_file(
    date = date,
    api_token = api_token,
    redcap_file = redcap_file,
    directory = directory,
    new_file = new_file,
    convert = convert,
    force = force
  )
  message("\nFinished antigen snapshot download.")
}

#' Download Snapshot of NBS PCR Test Data Posted on REDcap
#'
#' @export
download_serology_snapshot <- function(
  date = Sys.Date(),
  api_token = Sys.getenv("redcap_DFR_token"),
  redcap_file = "lab_serology",
  directory =
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/MSR SEROLOGY/"
  ,
  new_file = paste0(
    "MSR - All Serologies_",
    format(Sys.Date(), "%m%d%Y"),
    ".csv"
  ),
  convert = FALSE,
  force = FALSE
) {
  message("Starting serology snapshot download...\n")
  download_redcap_file(
    date = date,
    api_token = api_token,
    redcap_file = redcap_file,
    directory = directory,
    new_file = new_file,
    convert = convert,
    force = force
  )
  message("\nFinished serology snapshot download.")
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
