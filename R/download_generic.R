#' Download a File from the Data for Regions REDcap Project
#'
#' `download_data_for_regions()` is the workhorse behind the
#' `download_*_snapshot()` functions.
#'
#' @param date A \code{Date} indicating the date of the file to download
#'
#' @param api_token The API token for accessing the Data for Regions REDcap
#'   project. This should be stored in an \emph{.Renviron} file; see
#'   \link{using-renviron} for details.
#'
#' @param redcap_file A string indicating the name of the file to download from
#'   REDcap
#'
#' @param directory A string specifying the save directory; this should usually
#'   be left alone
#'
#' @param new_file A string specifying the save file name; this should always
#'   end in \emph{.csv} and should usually be left alone
#'
#' @param force A logical indicating whether to ignore any existing files
#'   matching `date` and `directory`
#'
#' @return Invisibly returns the path to the new data file
#'
#' @keywords internal
#'
#' @export
download_data_for_regions <- function(
  date = Sys.Date(),
  api_token,
  redcap_file,
  directory,
  new_file,
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
  on.exit(options(warn = options("warn")[[1]]), add = TRUE)

  options(warn = 2)
  check_date_updated()
  options(warn = options("warn")[[1]])

  # Step 3 - Download

  # URL base for API
  api_uri <- "https://redcap.health.tn.gov/redcap/api/"

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
  directory %<>% create_path()
  dir_temp <- fs::file_temp(".temp_redcap_", tmp_dir = directory)
  zip_temp <- create_path(dir_temp, "redcap_file", ".zip")

  # Delete existing temp folder if it exists and create new one
  if (fs::dir_exists(dir_temp)) fs::dir_delete(dir_temp)
  fs::dir_create(dir_temp)

  # Make sure that things are cleaned up when this function exits, whether
  # normally or as a result of an error
  on.exit(fs::dir_delete(dir_temp))

  # Download file
  httr::RETRY(
    "POST",
    url = api_uri,
    body = api_nbs_params,
    httr::write_disk(zip_temp),
    httr::progress(),
    times = 12L,
    pause_cap = 300L
  ) %>%
    httr::stop_for_status()

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
      length(file_temp), "."
    )
  )

  file_new <- create_path(directory, new_file)

  # Move the file to the chosen directory with the chosen file name
  fs::file_move(
    path = file_temp,
    new_path = file_new
  )
  message("Done.")

  invisible(path_create(directory, new_file))
}

#' Download a Report from the Integrated Data Tool REDcap Project
#'
#' `download_interview_report()` downloads a report from the Integrated Data
#' Tool project. You'll need API access to the project (and an API token for it)
#' to use this function.
#'
#' @inherit download_data_for_regions params return
#'
#' @param report_id The ID of the report to download
#'
#' @keywords internal
download_interview_report <- function(
  date = Sys.Date(),
  api_token,
  report_id,
  directory,
  new_file,
  force = FALSE
) {

  # Step 1 - Check directory to make sure file isn't already there

  # Get matching files from directory
  n_existing <- find_file(
    date = date,
    pattern = new_file,
    directory = directory,
    rtn_error = FALSE
  ) %>%
    length()

  # Don't run if any are found
  assertthat::assert_that(
    n_existing == 0 | force,
    msg = paste(
      "An existing file matches this date; download will not continue.",
      "To download anyway, set 'force == TRUE'."
    )
  )

  # Step 2 - Download
  message("Downloading REDcap report...")

  # URL base for API
  api_uri <- "https://redcap.health.tn.gov/redcap/api/"

  # Create params to get
  api_nbs_params <- list(
    token               = api_token,
    content             = "report",
    format              = "csv",
    report_id           = report_id,
    rawOrLabel          = "label",
    rawOrLabelHeaders   = "raw",
    exportCheckboxLabel = "true",
    returnFormat        = "json"
  )

  # Create temp folder and file names
  directory %<>% create_path()
  dir_temp <- fs::file_temp(".temp_redcap_", tmp_dir = directory)
  zip_temp <- create_path(dir_temp, "redcap_file.csv")

  # Delete existing temp folder if it exists
  if (fs::dir_exists(dir_temp)) fs::dir_delete(dir_temp)

  # Make sure that things are cleaned up when this function exits, whether
  # normally or as a result of an error
  on.exit(fs::dir_delete(dir_temp))

  # Create temp directory
  fs::dir_create(dir_temp)

  # Download file
  httr::RETRY(
    "POST",
    api_uri,
    body = api_nbs_params,
    httr::write_disk(zip_temp),
    httr::progress(),
    times = 12L,
    pause_cap = 300L
  ) %>%
    httr::stop_for_status()

  message("\nDone.")

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
      length(file_temp), "."
    )
  )

  file_new <- create_path(directory, new_file)

  # Move the file to the chosen directory with the chosen file name
  fs::file_move(
    path = file_temp,
    new_path = file_new
  )
  message("Done.")

  invisible(path_create(directory, new_file))
}

#' Download Lab File from Serv-U
#'
#' @param date A \code{date} indicating the date of the file to download
#'
#' @param usr The username to pass to the SFTP site. Ideally, this should be
#'   stored in an Renviron file and not in a script.
#'
#' @param pwd The password to pass to the SFTP site. Ideally, this should be
#'   stored in an Renviron file and not in a script.
#'
#' @param remote_dir A string indicating the directory on Serv-U that should
#'   contain the file
#'
#' @param local_dir A string indicating the directory on the local system in
#'   which to save the file
#'
#' @param new_file A string indicating the new file name to save the data under.
#'   Currently not used.
download_servu <- function(
  date = Sys.Date(),
  usr = Sys.getenv("sftp_usr"),
  pwd = Sys.getenv("sftp_pwd"),
  remote_dir,
  local_dir = "V:/EPI DATA ANALYTICS TEAM/AEL Data/",
  new_file = NULL
) {

  # Standardize paths
  remote_dir %<>% fs::path_norm() %>% fs::path_tidy()
  local_dir %<>% fs::path_expand() %>% fs::path_norm() %>% fs::path_tidy()

  # Create SFTP connection details
  sftp_con <- sftp_connect(
    server = "xfer.shelbycountytn.gov",
    folder = remote_dir,
    username = usr,
    password = pwd
  )

  # Get files matching date
  sftp_listfiles(sftp_connection = sftp_con) %>%
    dplyr::select(name) %>%
    dplyr::filter(
      stringr::str_detect(name, pattern = as.character(date))
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
    sftp_download(
      file = filename,
      tofolder = local_dir,
      sftp_connection = sftp_con
    )

    # Return NULL
    invisible(NULL)
  }
}

#' Check Update Date in Data for Regions REDcap Project
#'
#' `check_date_updated()` checks whether the supplied `date`
#' (today, by default) matches the `date_updated` record for the specified
#' region in the Data for Regions REDcap project.
#'
#' @inheritParams download_data_for_regions
#'
#' @param date The date to check against; defaults to `Sys.Date()`, which is
#'   probably what you want
#'
#' @param region The region to check for; exists to aid internal implementation
#'   and should probably not be changed from default
#'
#' @param date_updated The name of the record containing the date updated in
#'   REDcap; this exists to aid internal implementation and should probably not
#'   be changed from default
#'
#' @param quiet By default, warnings are issued if date does not match, and a
#'   message is issued if it does; should these be silenced?
#'
#' @return `TRUE` if dates match, `FALSE` otherwise
#'
#' @seealso \code{\link{data-for-regions-snapshots}}
#'
#' @export
check_date_updated <- function(
  date = Sys.Date(),
  api_token = Sys.getenv("redcap_DFR_token"),
  region = "MSR",
  date_updated = "date_updated",
  quiet = FALSE
) {
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
  httr::RETRY(
    "POST",
    url = api_uri,
    body = api_date_params,
    times = 12L,
    pause_cap = 300L
  ) %>%
    httr::stop_for_status() %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON() %>%
    purrr::as_vector() %>%
    lubridate::as_date() ->
    date_updated

  # Make sure that date_updated is at least as current as input date

  not_yet_warning <- paste0(
    "REDcap does not yet have data for ",
    date, ". Please check back later."
  )

  too_late_warning <- paste0(
    "REDcap's update date is more recent than the date specified in 'date'. ",
    "To download REDcap's most recent data, please re-run with",
    "'date == as.Date(", date_updated, ")'."
  )
  if (!quiet) {
    purrr::when(
      date_updated,
      . < date ~ rlang::warn(not_yet_warning, class = "warning_not_yet"),
      . > date ~ rlang::warn(too_late_warning, class = "warning_too_late"),
      ~ rlang::inform(paste0("Data available for ", date))
    )
  }

  if (date_updated == date) TRUE else FALSE
}
