#' Download NBS Data for Regions Reports
#' @importFrom magrittr `%>%`
download_nbs <- function(
  day = Sys.Date(),
  api_token = Sys.getenv("redcap_DFR_token"),
  dir_path = "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/Sandbox data pull Final/",
  fname = paste0(day, " Final Data Pull.csv"),
  force = FALSE
) {

  # Step 1 - Check dir_path to make sure file isn't already there

  # Check to see if file is already in directory
  existing_file <- find_file(
    day = day,
    pattern = fname,
    dir_path = dir_path,
    rtn_error = FALSE
  )

  # Don't run if file is already there
  if (length(existing_file) != 0 & !force) {
    error_exists <- paste(
      "An existing file matches this date; download will not continue.",
      "To download anyway, set 'force == TRUE'."
    )
    stop(error_exists)
  }

  # Step 2 - Make sure REDcap's data matches the day requested

  # Create URL base for API
  api_uri <- "https://redcap.health.tn.gov/redcap/api/"

  # Create request params to check for new REDcap file
  api_date_params <- list(
    token        = api_token,
    content      = "record",
    format       = "json",
    type         = "flat",
    records      = "MSR",
    fields       = "date_updated",
    returnFormat = "json"
  )

  # Check date updated
  httr::POST(api_uri, body = api_date_params) %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON() %>%
    purrr::as_vector() %>%
    lubridate::as_date() ->
  date_updated

  # If not updated yet
  if (date_updated < day) {
    error_future <- paste0(
      "REDcap does not yet have data for ",
      Sys.Date(), ". Please check back later."
    )
    stop(error_future)
  }

  # If date updated is later than input date
  if (date_updated > day) {
    error_past <- paste0(
      "REDcap's update date is more recent than the date specified in 'day'. ",
      "To download REDcap's most recent data, please re-run with",
      "'day == as.Date(", date_updated, ")'."
    )
    stop(error_past)
  }

  # Step 3 - Download NBS file
  message("Downloading NBS file...")

  # Create params to get NBS form
  api_nbs_params <- list(
    token        = api_token,
    content      = "file",
    action       = "export",
    record       = "MSR",
    field        = "nbs_daily_upload",
    returnFormat = "json"
  )

  # Create temporary directory for new files
  if (!dir.exists("temp")) {
    dir.create("temp")
  } else {
    unlink("temp", recursive = TRUE)
    dir.create("temp")
  }

  # Downloading most recent investigations file
  httr::POST(
    api_uri,
    body = api_nbs_params,
    httr::write_disk("temp/nbs.zip"),
    httr::progress()
  )

  message("\nDone.")

  # Unzip new file
  message("Unzipping folder...", appendLF = FALSE)
  unzip("temp/nbs.zip", exdir = "temp")
  message(" Done.")

  # Move to specified directory and rename
  message("Moving file and cleaning up...", appendLF = FALSE)
  nbs_file <- paste0(dir_path, fname)
  file.rename(
    from = "temp/MSR INVS.csv",
    to = nbs_file
  )

  # Delete temp directory before exit
  unlink("temp", recursive = TRUE)
  message(" Done!")
}

get_nbs <- function(
  day = Sys.Date(),
  dir_path = "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/Sandbox data pull Final/",
  file_name = NULL,
  encoding = TRUE,
  clean = TRUE,
  min_completion_rate = 0.5,
  clean_quietly = TRUE
) {

  # We want to remind the user what date is being used
  message(paste0("\nDate used: ", day, "\n"))

  # 'find_file' needs to match filenames like '...YYY-MM-DD...csv'
  pattern <- paste0(".*", day, ".*", "csv")

  # We're ready to find and read the NBS data
  coviData::find_file(
    day = day,
    dir_path = dir_path,
    pattern = pattern,
    file_name = file_name
  ) %>%
    coviData::read_nbs() ->
  data

  # Type, format, and value cleaning is optional, but "on" by default; either
  # return clean data or raw data (with the latter in character format)
  if (clean) {
    coviData::clean_nbs(
      data,
      min_completion_rate = min_completion_rate,
      quiet = clean_quietly
    )
  } else {
    data
  }
}

read_nbs <- function(
  path,
  nThread = 4
) {

  # Reading takes a while; we want to let the user know something is happening
  message("Reading NBS data file...")

  # The 'data.table::fread' function is designed to read large delimited files.
  # In this case, read_csv needs too much memory to store intermediate values;
  # 'fread' avoids this.
  data.table::fread(
    path,
    header = TRUE,
    colClasses = "character",
    blank.lines.skip = TRUE,
    nThread = nThread
  ) %>%
  as_tibble() %>%
  readr::type_convert()
}

clean_nbs <- function(
  .nbs_data,
  min_completion_rate = 0.5,
  quiet = TRUE
) {

  message("\nCleaning variables and data types...", appendLF = FALSE)
  cleaned_data <- coviData::clean_generic(.nbs_data)
  message("Done.\n")

  message("Factoring data for further wrangling...", appendLF = FALSE)
  cleaned_data %>%
    dplyr::mutate(
      dplyr::across(where(coviData::not_factor_date), factor)
    ) ->
  factor_data
  message("Done.\n")

  message("Finding informative variables in:")

  message("all data...")
  factor_data %>%
    coviData::cols_to_keep(min_completion_rate = min_completion_rate) ->
  all_cols

  message("cases...")
  factor_data %>%
    dplyr::filter(inv_case_status == "C" | inv_case_status == "P") %>%
    coviData::cols_to_keep(min_completion_rate = min_completion_rate) ->
  case_cols

  message("hospitalizations...")
  factor_data %>%
    dplyr::filter(hsptlizd_ind == "Y") %>%
    coviData::cols_to_keep(min_completion_rate = min_completion_rate) ->
  hosp_cols

  message("deaths...")
  factor_data %>%
    dplyr::filter(die_from_illness_ind == "Y") %>%
    coviData::cols_to_keep(min_completion_rate = min_completion_rate) ->
  death_cols
  message("Done.\n")

  message(
    "Removing uninformative variables and observations...",
    appendLF = FALSE
  )
  all_cols$info %>%
    union(case_cols$info) %>%
    union(hosp_cols$info) %>%
    union(death_cols$info) %>%
    union("inv_local_id") ->
  keep_cols_info

  all_cols$missing %>%
    union(case_cols$missing) %>%
    union(hosp_cols$missing) %>%
    union(death_cols$missing) %>%
    union("inv_local_id") ->
  keep_cols_missing

  keep_cols <- intersect(keep_cols_info, keep_cols_missing)

  cleaned_data %>%
    dplyr::select(tidyselect::matches(keep_cols)) %>%
    janitor::remove_empty(which = "rows") %>%
    tibble::as_tibble() %>%
    (function(.nbs_data) {message("Done!"); return(.nbs_data)})
}

find_dupes <- function(
  data,
  col_names = c(
    "patient_first_name",
    "patient_last_name",
    "patient_dob",
    "patient_current_sex",
    "patient_street_addr_1",
    "patient_zip",
    "inv_start_dt",
    "event_date"
  )
) {
  data %>%
    dplyr::select(tidyselect::matches(col_names)) %>%
    data.table::as.data.table() %>%
    duplicated(na.rm = TRUE) %>%
    sum()
}

clean_nbs_id <- function(.nbs_id) {
  .nbs_id %>%
    # Coerce to character
    as.character() %>%
    # Remove whitespace
    stringr::str_squish() %>%
    # Remove leading and trailing patters
    stringr::str_remove_all(pattern = "PSN1") %>%
    stringr::str_remove_all(pattern = "PSN2") %>%
    stringr::str_remove_all(pattern = "TN01") %>%
    # Truncate from left if > 7 characters
    stringr::str_trunc(width = 7, side = "left", ellipsis = "") %>%
    # Pad from left if < 7 characters
    stringr::str_pad(width = 7, side = "left", pad = "0")
}

clean_deaths <- function(
  dir_path = "V:/EPI DATA ANALYTICS TEAM/MORTALITY DATA/",
  r_file = "Working Copy Death  Epi.xlsx",
  w_file = paste0("cleaned_copies/cleaned_deaths_", Sys.Date(), ".xlsx")
) {
  readxl::read_excel(
    path = paste0(dir_path, r_file),
    trim_ws = TRUE,
    guess_max = .Machine$integer.max %/% 100L,
    progress = TRUE
  ) %>%
    mutate(NBS = clean_nbs_id(NBS)) %>%
    openxlsx::write.xlsx(
      file = paste0(dir_path, w_file)
    )
}
