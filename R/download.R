#' Download REDcap File
download_redcap_file <- function(
  date = Sys.Date(),
  api_token,
  redcap_file,
  directory,
  new_file,
  convert = FALSE,
  force = FALSE
) {

  # Generic parameters - don't want these to be function parameters, but they
  # could change state-side
  region <- "MSR"
  date_updated <- "date_updated"

  # Step 1 - Check directory to make sure file isn't already there

  # Get matching files from directory
  n_existing <- find_file(
    date = date,
    pattern = new_file,
    directory = directory,
    rtn_error = FALSE
  ) %>% length()

  # Don't run if any are found


  assertthat::assert_that(
    n_existing == 0 | force,
    msg = paste(
      "An existing file matches this date; download will not continue.",
      "To download anyway, set 'force == TRUE'."
    )
  )

  # Step 2 - Make sure REDcap's data matches the date requested

  # URL base for API
  api_uri <- "https://redcap.health.tn.gov/redcap/api/"

  # Create request params for REDcap update date
  api_date_params <- list(
    token        = api_token,
    content      = "record",
    format       = "json",
    type         = "flat",
    records      = region,
    fields       = date_updated,
    returnFormat = "json"
  )

  # Get date_updated
  httr::POST(api_uri, body = api_date_params) %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON() %>%
    purrr::as_vector() %>%
    lubridate::as_date() ->
    date_updated

  # Make sure that date_updated is at least as current as input date
  assertthat::assert_that(
    date_updated >= date,
    msg = paste0(
      "REDcap does not yet have data for ",
      Sys.Date(), ". Please check back later."
    )
  )

  # Make sure that date_updated equals input date; otherwise it's older
  assertthat::assert_that(
    date_updated == date,
    msg = paste0(
      "REDcap's update date is more recent than the date specified in 'date'. ",
      "To download REDcap's most recent data, please re-run with",
      "'date == as.Date(", date_updated, ")'."
    )
  )

  # Step 3 - Download
  message("Downloading REDcap file...")

  # Create params to get
  api_nbs_params <- list(
    token        = api_token,
    content      = "file",
    action       = "export",
    record       = region,
    field        = redcap_file,
    returnFormat = "json"
  )

  # Create temp folder and file names
  dir_temp <- fs::path_temp() %>% paste0("/redcap_temp")
  zip_temp <- paste0(dir_temp, "/redcap_file.zip")

  # Delete existing temp folder if it exists and create new one
  if (fs::dir_exists(dir_temp)) fs::dir_delete(dir_temp)
  fs::dir_create(dir_temp)

  # Make sure that things are cleaned up when this function exits, whether
  # normally or as a result of an error
  on.exit(fs::dir_delete(dir_temp))

  # Download file
  httr::POST(
    api_uri,
    body = api_nbs_params,
    httr::write_disk(zip_temp),
    httr::progress()
  )

  message("\nDone.")

  # Step 4 - Unzip, Move, and Rename

  # Unzip new file in temporary directory
  message("Unzipping folder...", appendLF = FALSE)
  utils::unzip(zip_temp, exdir = dir_temp)
  fs::file_delete(zip_temp)
  message("Done.")

  # Move to specified directory and rename
  message(
    "Moving file and cleaning up; this may take a while...",
    appendLF = FALSE
  )

  # Find the result of unzipping
  file_temp <- fs::dir_ls(dir_temp)

  # Make sure there's only one file
  assertthat::assert_that(
    length(file_temp) == 1,
    msg = paste0(
      "Expected 1 file when unzipping the response from REDcap, but got ",
      length(files_temp), "."
    )
  )

  file_new <- directory %>%
    fs::path_split() %>%
    .[[1]] %>%
    append(new_file) %>%
    fs::path_join()

  # Move the file to the chosen directory with the chosen file name
  fs::file_move(
    path = file_temp,
    new_path = file_new
  )
  message("Done.")

  if (convert) {
    # Optional Step 5 - Convert to xlsx
    message("Opening in Excel for conversion...", appendLF = FALSE)
    shell.exec(file_new)
    message("Done.")
  }
}

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

download_servu <- function(
  date = Sys.Date(),
  usr = Sys.getenv("sftp_usr"),
  pwd = Sys.getenv("sftp_pwd"),
  servu_dir,
  local_dir = "V:/EPI DATA ANALYTICS TEAM/AEL Data/",
  new_file  = NULL
) {

  # Create SFTP connection details
  sftp_con <- sftp::sftp_connect(
    server = "xfer.shelbycountytn.gov",
    folder = servu_dir,
    username = usr,
    password = pwd
  )

  # Get files matching date
  sftp:::sftp_listfiles(sftp_connection = sftp_con) %>%
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

download_ael <- function(
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
  sftp:::sftp_listfiles(sftp_connection = sftp_con) %>%
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

download_ael_new <- function(
  date = Sys.Date(),
  usr = Sys.getenv("sftp_usr"),
  pwd = Sys.getenv("sftp_pwd"),
  directory = "V:/EPI DATA ANALYTICS TEAM/AEL Data/",
  new_file  = NULL
) {
  download_servu(
    date = date,
    usr = usr,
    pwd = pwd,
    directory = directory,
    new_file  = new_file
  )
}
