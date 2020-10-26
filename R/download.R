#' Download Snapshot of NBS Data Posted on REDcap
#'
#' @export
download_nbs_snapshot <- function(
  date = Sys.Date(),
  api_token = Sys.getenv("redcap_DFR_token"),
  redcap_file = "nbs_daily_upload",
  directory = "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/Sandbox data pull Final/",
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
}

#' Download AEL File from Serv-U SFTP Site
#'
#' \code{download_ael} finds and download the AEL data for the input date and
#' saves it to a folder of your choice. To make the best use of this function,
#' you should save your username and password for Serv-U in a ".Renviron" file;
#' see
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

#' Download AEL File from Serv-U SFTP Site
#'
#' @param pattern A regular expression matching the desired filename in the AEL
#'   folder. The default is todate's date, but a more specific pattern may be
#'   necessary if multiple files are found.
#'
#' @param usr Username for the Serv-U site. The default is to look for the
#'   system environment variable "sftp_usr", as storing credentials in a script
#'   is not recommended for security purposes.
#'
#' @param pwd Password for the Serv-U site. The default is to look for the
#'   system environment variable "sftp_pwd", as storing credentials in a script
#'   is not recommended for security purposes.
#'
#' @param tofolder Folder/directory to save the file to
#'
#' @return NULL
#'
#' @importFrom magrittr `%>%`
#'
#' @export

download_and_replace_ael <- function(
  pattern = Sys.Date(),
  usr = Sys.getenv("sftp_usr"),
  pwd = Sys.getenv("sftp_pwd"),
  tofolder = "V:/EPI DATA ANALYTICS TEAM/AEL Data/",
  fix_names = TRUE
) {

  # Create SFTP connection details
  sftp_con <- sftp::sftp_connect(
    server = "xfer.shelbycountytn.gov",
    folder = "AEL",
    username = usr,
    password = pwd
  )

  # Get files matching date
  sftp::sftp_listfiles(sftp_connection = sftp_con) %>%
    dplyr::select(name) %>%
    dplyr::filter(
      stringr::str_detect(name, pattern = as.character(pattern))
    ) %>%
    .[[1]] ->
    filename

  # Check that exactly one matching file was found and take action
  if (length(filename) == 0L) {
    stop("No matching files were found.")
  } else if (length(filename) > 1L) {
    stop(
      paste0(
        "Multiple matching files were found. ",
        "Please specify a unique 'pattern' from the filenames below:\n",
        stringr::str_flatten(filename, collapse = "\n")
      )
    )
  } else {
    sftp::sftp_download(
      file = filename,
      tofolder = tofolder,
      sftp_connection = sftp_con
    )

    if (fix_names & lubridate::is.Date(pattern)) {
      message("Standardizing name columns...")
      replace_ael(date = pattern, directory = tofolder, date_file = date_file)
      message("Done!")
    } else if (fix_names) {
      wrn <- paste0(
        "Name columns were not repaired b/c 'pattern' must be a date to fix ",
        "names automatically. ",
        "Please call 'replace_ael' separately with the date of interest."
      )
      warning(wrn)
    }

    # Return NULL
    invisible(NULL)
  }
}
