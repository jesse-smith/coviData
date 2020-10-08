#' @export
load_and_process_nbs <- function(
  date = Sys.Date(),
  directory = "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/Sandbox data pull Final/",
  file_name = NULL,
  file_type = c("csv", "xlsx"),
  clean = TRUE,
  min_completion_rate = 0.5
) {

  # We want to remind the user what date is being used
  message(paste0("\nDate used: ", date, "\n"))

  # 'find_file' needs to match filenames like '...YYY-MM-DD...csv'
  pattern <- paste0(".*", date, ".*", file_type[[1]])

  # We're ready to find and read the NBS data
  find_file(
    date = date,
    directory = directory,
    pattern = pattern,
    file_name = file_name
  ) %>%
    read_file() ->
  data

  # Type, format, and value cleaning is optional, but "on" by default; either
  # return clean data or raw data (with the latter in character format)
  if (clean) {
    clean_nbs(
      data,
      min_completion_rate = min_completion_rate
    )
  } else {
    data
  }
}

#' @importFrom magrittr `%>%`
#'
#' @importFrom magrittr `%T>%`
clean_nbs <- function(
  .nbs_data,
  min_completion_rate = 0.5,
  string_to_factor = FALSE
) {

  message("\nCleaning patient names...")
  .nbs_data %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::matches(c("_name", "_nm")),
        .fns = standardize_names
      )
    ) %T>%
    {message("Done.")} %T>%
    {message("\nCleaning variables and data types...", appendLF = FALSE)} %>%
    clean_generic(string_to_factor = string_to_factor) %T>%
    {message("Done.\n")} %T>%
    {message("Factoring data for further wrangling...", appendLF = FALSE)} %>%
    dplyr::mutate(
      dplyr::across(where(not_factor_date), factor)
    ) %T>%
    {message("Done.\n")} ->
  cleaner_data

  message("Finding informative variables in:")

  message("all data...")
  cleaner_data %>%
    cols_to_keep(min_completion_rate = min_completion_rate) ->
  all_cols

  message("cases...")
  cleaner_data %>%
    dplyr::filter(inv_case_status == "C" | inv_case_status == "P") %>%
    cols_to_keep(min_completion_rate = min_completion_rate) ->
  case_cols

  message("hospitalizations...")
  cleaner_data %>%
    dplyr::filter(hsptlizd_ind == "Y") %>%
    cols_to_keep(min_completion_rate = min_completion_rate) ->
  hosp_cols

  message("deaths...")
  cleaner_data %>%
    dplyr::filter(die_from_illness_ind == "Y") %>%
    cols_to_keep(min_completion_rate = min_completion_rate) ->
  death_cols

  message("Done.\n")

  message(
    "Removing uninformative variables and observations...",
    appendLF = FALSE
  )

  all_cols$info %>%
    generics::union(case_cols$info) %>%
    generics::union(hosp_cols$info) %>%
    generics::union(death_cols$info) %>%
    generics::union("inv_local_id") ->
  keep_cols_info

  all_cols$missing %>%
    generics::union(case_cols$missing) %>%
    generics::union(hosp_cols$missing) %>%
    generics::union(death_cols$missing) %>%
    generics::union("inv_local_id") ->
  keep_cols_missing

  keep_cols <- generics::intersect(keep_cols_info, keep_cols_missing)

  cleaner_data %>%
    dplyr::select(tidyselect::matches(keep_cols)) %>%
    janitor::remove_empty(which = "rows") %>%
    tibble::as_tibble() %T>%
    {message("Done!\n")}
}

#' @importFrom magrittr `%>%`
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

#' @importFrom magrittr `%>%`
standardize_nbs_id <- function(id) {
  id %>%
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

#' @importFrom magrittr `%>%`
#'
#' @export
clean_deaths <- function(
  directory = "V:/EPI DATA ANALYTICS TEAM/MORTALITY DATA/",
  r_file = "Working Copy Death  Epi.xlsx",
  w_file = paste0("cleaned_copies/cleaned_deaths_", Sys.Date(), ".xlsx")
) {
  readxl::read_excel(
    path = paste0(directory, r_file),
    trim_ws = TRUE,
    guess_max = .Machine$integer.max %/% 100L,
    progress = TRUE
  ) %>%
    dplyr::mutate(NBS = clean_nbs_id(NBS)) %>%
    openxlsx::write.xlsx(
      file = paste0(directory, w_file)
    )
}
