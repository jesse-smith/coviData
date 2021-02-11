#' Download a File from the Data for Regions REDcap Project
#'
#' @description
#' These functions downloads a file posted on the Data for Regions
#' project. You'll need API access to the project (and an API token for it) to
#' use this function.
#'
#' `download_nbs_snapshot()` downloads the current snapshot of NBS
#' investigation data (if posted).
#'
#' `download_pcr_snapshot()` downloads the current snapshot of PCR testing data
#' (if posted).
#'
#' `download_antigen_snapshot()` downloads the current snapshot of antigen
#' testing data (if posted).
#'
#' `download_serology_snapshot()` downloads the current snapshot of serology
#' testing data (if posted).
#'
#' `download_vaccine_snapshot()` downloads the current snapshot of vaccination
#' data (if posted). Note that this is only posted on Tues/Fri, unlike other
#' snapshot files.
#'
#' @inherit download_data_for_regions params return
#'
#' @aliases download_nbs_snapshot download_pcr_snapshot
#'   download_antigen_snapshot download_serology_snapshot
#'
#' @md
#'
#' @name data-for-regions-snapshots
#'
#' @aliases download_nbs_snapshot download_pcr_snapshot
#'   download_antigen_snapshot download_serology_snapshot
#'   download_vaccine_snapshot
NULL

#' @rdname data-for-regions-snapshots
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
  force = FALSE
) {
  message("Starting NBS snapshot download...\n")
  download_data_for_regions(
    date = date,
    api_token = api_token,
    redcap_file = redcap_file,
    directory = directory,
    new_file = new_file,
    force = force
  )
  message("\nFinished NBS snapshot download.")

  invisible(path_create(directory, new_file))
}

#' @rdname data-for-regions-snapshots
#'
#' @export
download_pcr_snapshot <- function(
  date = Sys.Date(),
  api_token = Sys.getenv("redcap_DFR_token"),
  redcap_file = "lab_pcr",
  directory = "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/MSR PCR/",
  new_file = paste0("MSR - All PCRs_", format(Sys.Date(), "%m%d%Y"), ".csv"),
  force = FALSE
) {
  message("Starting PCR snapshot download...\n")
  download_data_for_regions(
    date = date,
    api_token = api_token,
    redcap_file = redcap_file,
    directory = directory,
    new_file = new_file,
    force = force
  )
  message("\nFinished PCR snapshot download.")

  invisible(path_create(directory, new_file))
}


#' @rdname data-for-regions-snapshots
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
  force = FALSE
) {
  message("Starting antigen snapshot download...\n")
  download_data_for_regions(
    date = date,
    api_token = api_token,
    redcap_file = redcap_file,
    directory = directory,
    new_file = new_file,
    force = force
  )
  message("\nFinished antigen snapshot download.")

  invisible(path_create(directory, new_file))
}

#' @rdname data-for-regions-snapshots
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
  force = FALSE
) {
  message("Starting serology snapshot download...\n")
  download_data_for_regions(
    date = date,
    api_token = api_token,
    redcap_file = redcap_file,
    directory = directory,
    new_file = new_file,
    force = force
  )
  message("\nFinished serology snapshot download.")

  invisible(path_create(directory, new_file))
}

#' @rdname data-for-regions-snapshots
#'
#' @export
download_vaccine_snapshot <- function(
  date = Sys.Date(),
  api_token = Sys.getenv("redcap_DFR_token"),
  redcap_file = "covid_vaccine_data",
  directory = path_create(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
    "COVID-19 Vaccine Reporting/data/COVID-19 Vaccine data pull/"
  ),
  new_file = paste0("MSR_VACC_", format(date, "%Y%m%d"), ".csv"),
  force = FALSE
) {

  directory <- path_create(directory)
  new_file <- path_create(new_file)

  updated <- check_vac_date_updated(
    date,
    api_token = api_token,
    redcap_file = redcap_file
  )

  if (!updated && !force) {
    rlang::abort(
      paste0(
        "Vaccine data was last updated on\n",
        vacc_date, "\n",
        "but the specified download date is\n",
        date,
        "\n\n",
        "To download anyway (and potentially overwrite existing files), ",
        "set `force = TRUE`."
      ),
      class = "vaccine_data_not_updated"
    )
  }

  message("Starting vaccine snapshot download...")
  download_data_for_regions(
    date = date,
    api_token = api_token,
    redcap_file = redcap_file,
    directory = directory,
    new_file = new_file,
    force = force
  )
  message("\nFinished vaccine snapshot download.")
}

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
  check_directory_for_existing_file(
    date = date,
    file = new_file,
    directory = directory,
    force = force
  )

  # Step 2 - Make sure REDcap's data matches the date requested
  original_warn <- options("warn")[[1L]]
  on.exit(options(warn = original_warn), add = TRUE)

  options(warn = 2L)
  check_date_updated(date = date)
  options(warn = original_warn)

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

  # Determine file name for download
  file_dl <- name_data_for_regions_file(
    redcap_file,
    api_token = api_token
  )

  # Create temp folder and file names
  directory <- path_create(directory)
  dir_temp <- fs::file_temp(".temp_redcap_", tmp_dir = directory)
  file_temp_dl <- path_create(dir_temp, file_dl)

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
    httr::write_disk(file_temp_dl),
    httr::progress(),
    times = 12L,
    pause_cap = 300L
  ) %>%
    httr::stop_for_status()

  message("\nDone.")

  # Step 4 - Unzip, Move, and Rename

  # Unzip new file in temporary directory, if necessary

  if (fs::path_ext(file_temp_dl) == "zip") {
    message("Unzipping folder...", appendLF = FALSE)
    utils::unzip(file_temp_dl, exdir = dir_temp)
    fs::file_delete(file_temp_dl)
    message("Done.")
  }

  # Move to specified directory and rename
  message(
    "Moving file and cleaning up; this may take a while...",
    appendLF = FALSE
  )

  # Find the result of downloading/unzipping
  file_temp <- fs::dir_ls(dir_temp)

  # Make sure there's only one file
  assert(
    length(file_temp) == 1,
    message = paste0(
      "Expected 1 file when unzipping the response from REDcap, but got ",
      length(file_temp), "."
    )
  )

  file_new <- path_create(directory, new_file)

  # Move the file to the chosen directory with the chosen file name
  fs::file_move(
    path = file_temp,
    new_path = file_new
  )
  message("Done.")

  invisible(path_create(directory, new_file))
}

#' Check a Directory for an Existing File
#'
#' `check_directory_for_existing_file()` is used internally to avoid overwriting
#' existing files. It throws an error when a file is found matching the given
#' `file`; to override this behavior, set `force = TRUE`.
#'
#' @param date The date included in the file name; provided here for error
#'   messages
#'
#' @param file The file to check for
#'
#' @param directory The directory to check in
#'
#' @param force Should the function ignore existing files?
#'
#' @return A logical indicating whether existing files were found, invisibly
#'
#' @noRd
check_directory_for_existing_file <- function(
  date,
  file,
  directory,
  force
) {
  # Get matching files from directory
  n_existing <- find_file(
    date = date,
    pattern = file,
    directory = directory,
    rtn_error = FALSE
  ) %>% length()

  # Don't run if any are found
  assert_any(
    n_existing == 0, force,
    message = paste(
      "An existing file matches this date; download will not continue.",
      "To download anyway, set 'force == TRUE'."
    )
  )

  invisible(as.logical(n_existing))
}

#' Request the Name of a Data for Regions File
#'
#' `name_data_for_regions_file()` gets the name of a file stored on the Data for
#' Regions REDcap server.
#'
#' @param redcap_file The name of the record on REDcap
#'
#' @param api_token An API key for the Data for Regions project
#'
#' @return A character vector containing the name of the file
#'
#' @noRd
name_data_for_regions_file <- function(
  redcap_file,
  api_token = Sys.getenv("redcap_DFR_token")
) {

  api_uri <- "https://redcap.health.tn.gov/redcap/api/"
  region <- "MSR"

  api_params <- list(
    token   = api_token,
    content = "record",
    format  = "json",
    records = region,
    fields  = redcap_file
  )

  httr::RETRY(
    "POST",
    url = api_uri,
    body = api_params,
    times = 12L,
    pause_cap = 300L
  ) %>%
    httr::stop_for_status() %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON() %>%
    extract2(1L)
}


#' Check Date Vaccination Data Was Last Updated
#'
#' `check_vac_date_updated()` checks the last update date for vaccination data
#' on the TN REDcap.
#'
#' @param date The expected update date
#'
#' @param api_token The API key for the TN Data for Regions project
#'
#' @param redcap_file The name of the vaccine data file on the REDcap project
#'
#' @return A boolean (`TRUE` or `FALSE`)
#'
#' @export
check_vac_date_updated <- function(
  date = Sys.Date(),
  api_token = Sys.getenv("redcap_DFR_token"),
  redcap_file = "covid_vaccine_data"
) {
  update_date <- get_vaccine_snapshot_date(api_token, redcap_file = redcap_file)

  lubridate::as_date(date) == lubridate::as_date(update_date)
}

#' Determine Update Date for Vaccine Snapshot Data
#'
#' `get_vaccine_snapshot_date()` determines the last date that the vaccine
#' snapshot on the Data for Regions project was updated.
#'
#' @param api_token An API key for the Data for Regions project
#'
#' @return A `Date` indicating the last update date
#'
#' @noRd
get_vaccine_snapshot_date <- function(
  api_token = Sys.getenv("redcap_DFR_token"),
  redcap_file = "covid_vaccine_data"
) {

  tmp_file <- download_vaccine_file(api_token, redcap_file = redcap_file)
  on.exit(fs::dir_delete(fs::path_dir(tmp_file)), add = TRUE)

  if (fs::path_ext(tmp_file) != "zip") {
    rlang::abort("Vaccine file is not a ZIP archive")
  }

  tmp_file %>%
    zip::zip_list() %>%
    dplyr::pull("timestamp") %>%
    lubridate::as_date()
}

download_vaccine_file <- function(
  api_token = Sys.getenv("redcap_DFR_token"),
  redcap_file = "covid_vaccine_data",
  dir = fs::dir_create(fs::file_temp("vacc_")),
  file = name_data_for_regions_file(redcap_file, api_token = api_token)
) {

  file <- path_create(dir, file)

  api_uri <- "https://redcap.health.tn.gov/redcap/api/"

  # Create params to get
  api_params <- list(
    token        = api_token,
    content      = "file",
    action       = "export",
    record       = "MSR",
    field        = redcap_file,
    returnFormat = "json"
  )

  httr::RETRY(
    "POST",
    url = api_uri,
    body = api_params,
    httr::write_disk(file),
    httr::progress(),
    times = 12L,
    pause_cap = 300L
  ) %>%
    httr::stop_for_status()

  file
}
