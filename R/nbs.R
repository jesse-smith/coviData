#' Download NBS Data for Regions Reports
download_nbs() {

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
  # 'fread' avoids this by reading
  data.table::fread(
    path,
    header = TRUE,
    # colClasses = "character",
    blank.lines.skip = TRUE,
    nThread = nThread
  )
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
