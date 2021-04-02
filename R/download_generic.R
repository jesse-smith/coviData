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
#' @param headers Should column headers be raw variable names or labels?
#'
#' @keywords internal
download_interview_report <- function(
  date = Sys.Date(),
  api_token,
  report_id,
  headers = c("raw", "label"),
  directory,
  new_file,
  force = FALSE
) {

  headers <- rlang::arg_match(headers)[[1L]]

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
  assert_any(
    n_existing == 0, force,
    message = paste(
      "An existing file matches this date; download will not continue.",
      "To download anyway, set 'force == TRUE'."
    )
  )

  # Step 2 - Download
  message("Downloading REDcap report...")

  # URL base for API
  api_uri <- "https://redcap.shelbycountytn.gov/api/"

  # Create params to get
  api_nbs_params <- list(
    token               = api_token,
    content             = "report",
    format              = "csv",
    report_id           = report_id,
    rawOrLabel          = "label",
    rawOrLabelHeaders   = headers,
    exportCheckboxLabel = "true",
    returnFormat        = "json"
  )

  # Create temp folder and file names
  directory %<>% create_path()
  dir_temp <- fs::file_temp(".temp_redcap_", tmp_dir = directory)
  file_temp_dl <- create_path(dir_temp, "redcap_file.csv")

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
    httr::write_disk(file_temp_dl),
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
  assert(
    length(file_temp) == 1,
    message = paste0(
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
  remote_dir <- remote_dir %>% fs::path_norm() %>% fs::path_tidy()
  local_dir <- path_create(local_dir)

  # Create SFTP connection details
  sftp_con <- sftp_connect(
    server = "xfer.shelbycountytn.gov",
    folder = remote_dir,
    username = usr,
    password = pwd
  )

  # Get files matching date
  filename <- sftp_listfiles(sftp_connection = sftp_con) %>%
    dplyr::select("name") %>%
    dplyr::filter(
      stringr::str_detect(.data[["name"]], as.character({{ date }}))
    ) %>%
    dplyr::pull(1L)

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
#' @param vac Should vaccination data be checked (`TRUE`), or NBS data
#'   (`FALSE`, the default)?
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
  quiet = FALSE,
  vac = FALSE
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
  if (vac) {
    date_updated <- vac_snapshot_date(api_token)
  } else {
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
  }

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
