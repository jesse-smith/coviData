#' Download REDcap File from TN REDcap
#'
#' @param date A \code{Date} indicating the date of the file to download
#'
#' @param api_token The API token for the REDcap project to access. Ideally,
#'   this should be stored in Renviron and not in a script.
#'
#' @param redcap_file A string indicating the name of the file to download from
#'   REDcap
#'
#' @param directory A string indicating the directory to save the file in
#'
#' @param new_file A string indicating the name to save the file under
#'
#' @param convert A logical indicating whether the user would like to convert
#'   the file to another format. If so, the file will open in Excel after
#'   downloading.
#'
#' @param force A logical indicating whether to ignore existing files with
#'   the given date already in the directory
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
      length(file_temp), "."
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

download_redcap_report <- function(
  date = Sys.Date(),
  api_token,
  report_id,
  directory,
  new_file,
  convert = FALSE,
  force = FALSE
) {

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
    rawOrLabelHeaders   = "label",
    exportCheckboxLabel = "true",
    returnFormat        = "json"
  )

  # Create temp folder and file names
  dir_temp <- fs::path_temp() %>% paste0("/redcap_temp")
  zip_temp <- paste0(dir_temp, "/redcap_file.csv")

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

  # Step 3 - Unzip, Move, and Rename
  # Unzip new file in temporary directory
  # message("Unzipping folder...", appendLF = FALSE)
  # utils::unzip(zip_temp, exdir = dir_temp)
  # fs::file_delete(zip_temp)
  # message("Done.")

  # Move to specified directory and rename
  message(
    "Moving file and cleaning up; this may take a while...",
    appendLF = FALSE
  )

  # Find the result of unzipping
  file_temp <- fs::dir_ls(dir_temp)
  print(file_temp)
  # Make sure there's only one file
  assertthat::assert_that(
    length(file_temp) == 1,
    msg = paste0(
      "Expected 1 file when unzipping the response from REDcap, but got ",
      length(file_temp), "."
    )
  )

  file_new <- directory %>%
    fs::path_real() %>%
    fs::path_split() %>%
    .[[1]] %>%
    append(new_file) %>%
    fs::path_join()
  print(file_new)
  # Move the file to the chosen directory with the chosen file name
  fs::file_move(
    path = file_temp,
    new_path = file_new
  )
  message("Done.")

  if (convert) {
    # Optional Step 4 - Convert to xlsx
    message("Opening in Excel for conversion...", appendLF = FALSE)
    shell.exec(file_new)
    message("Done.")
  }
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
  remote_dir %<>% fs::path_tidy()
  local_dir %<>% fs::path_real() %>% fs::path_tidy()

  # Create SFTP connection details
  sftp_con <- sftp::sftp_connect(
    server = "xfer.shelbycountytn.gov",
    folder = remote_dir,
    username = usr,
    password = pwd
  )

  # Get files matching date
  sftp::sftp_listfiles(sftp_connection = sftp_con) %>%
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
    sftp::sftp_download(
      file = filename,
      tofolder = local_dir,
      sftp_connection = sftp_con
    )

    # Return NULL
    invisible(NULL)
  }
}
