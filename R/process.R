#' Run the Cases Workflow for Daily Report
#'
#' The `process_*` functions mimic data transformations in the SAS program for
#' positive/negative people/tests.
#'
#' @param data A `tibble`. Input data- either investigations for
#'   `process_*_people()` functions or PCR tests for `process_*_tests()`
#'   functions
#'
#' @param date The download date of the data file to use if no data is supplied;
#'   defaults to most recent file
#'
#' @return A `tibble` containing the information from the NBS investigations/PCR
#'   files on positive/negative people/tests
#'
#' @name process_nbs
NULL

#' @rdname process_nbs
#'
#' @export
process_positive_people <- function(
  data = read_file_delim(path_inv(date)),
  date = NULL
) {

  missings <- c(NA_character_, "Na", "", " ")

  data %>%
    janitor::clean_names() %>%
    # Filter to only our jurisdiction and county
    # Jurisdiction should get dropped as a constant during pre-processing, so
    # handle it flexibly
    tidylog::filter(
      dplyr::across(
        dplyr::contains("jurisdiction_nm"),
        ~ stringr::str_detect(.x, "(?i).*Memphis.*Shelby.*County.*")
      ),
      .data[["alt_county"]] %in% c("Shelby County", {{ missings }})
    ) %>%
    # Filter to patients in our state
    # Note: This gets rid of records missing `alt_county` AND `patient_state`
    tidylog::filter(
      alt_county == "Shelby County" |
        as.numeric(.data[["patient_state"]]) %in% c(47, NA)
    ) %>%
    dplyr::mutate(
      patient_last_name = .data[["patient_last_name"]] %>%
        str_to_ascii() %>%
        stringr::str_to_upper() %>%
        stringr::str_remove_all(pattern = "\\s+"),
      patient_first_name = .data[["patient_first_name"]] %>%
        str_to_ascii() %>%
        stringr::str_to_upper() %>%
        stringr::str_remove_all(pattern = "\\s+")
    ) %>%
    dplyr::mutate(
      patient_street_addr = .data[["patient_street_addr_1"]] %>%
        str_to_ascii() %>%
        stringr::str_to_upper() %>%
        stringr::str_remove_all(pattern = "\\s+"),
      patient_zip = .data[["patient_zip"]] %>%
        str_to_ascii() %>%
        stringr::str_trunc(width = 5, ellipsis = "")
    ) %>%
    # Filter to confirmed and probable cases
    # Check that inv_local_id is unique - matches SAS on 2020-11-15
    tidylog::filter(.data[["inv_case_status"]] %in% c("C", "P")) %>%
    dplyr::arrange(.data[["inv_local_id"]]) %>%
    tidylog::distinct(.data[["inv_local_id"]], .keep_all = TRUE) %>%
    # Filter any non-distinct patient_local_ids - matches SAS on 2020-11-15
    dplyr::arrange(.data[["patient_local_id"]], .data[["inv_case_status"]]) %>%
    tidylog::distinct(.data[["patient_local_id"]], .keep_all = TRUE) %>%
    # Filter non-distinct people based on name and DOB - matches SAS on 2020-11-15
    # SAS code standardizes case and removes whitespace
    # TODO: Switch to `standardize_string()`
    dplyr::arrange(
      .data[["patient_last_name"]],
      .data[["patient_first_name"]],
      .data[["patient_dob"]],
      .data[["inv_case_status"]]
    ) %>%
    tidylog::distinct(
      .data[["patient_last_name"]],
      .data[["patient_first_name"]],
      .data[["patient_dob"]],
      .keep_all = TRUE
    )
}

#' @rdname process_nbs
#'
#' @export
process_negative_people <- function(
  data = read_file_delim(path_inv(date)),
  date = NULL
) {

  missings <- c(NA_character_, "Na", "", " ")

  data %>%
    janitor::clean_names() %>%
    # Filter to only our jurisdiction and county
    # Jurisdiction should get dropped as a constant during pre-processing, so
    # handle it flexibly
    tidylog::filter(
      dplyr::across(
        dplyr::contains("jurisdiction_nm"),
        ~ stringr::str_detect(.x, "(?i).*Memphis.*Shelby.*County.*")
      ),
      .data[["alt_county"]] %in% c("Shelby County", {{ missings }})
    ) %>%
    # Filter to patients in our state
    # Note: This gets rid of records missing `alt_county` AND `patient_state`
    tidylog::filter(
      .data[["alt_county"]] == "Shelby County" |
        as.numeric(.data[["patient_state"]]) %in% c(47, NA)
    ) %>%
    dplyr::mutate(
      .obs_tmp_ = stringr::str_starts(.data[["inv_local_id"]], "(?i)OBS"),
      .not_pcr_tmp_ = .data[["negative_ig_a"]] %in% "1"
      | .data[["negative_ig_a"]] %in% "1"
      | .data[["negative_ig_g"]] %in% "1"
      | .data[["negative_ig_m"]] %in% "1"
      | .data[["negative_antigen"]] %in% "1"
    ) %>%
    tidylog::filter(!(.data[[".obs_tmp_"]] & .data[[".not_pcr_tmp_"]])) %>%
    dplyr::select(-c(".obs_tmp_", ".not_pcr_tmp_")) %>%
    tidylog::filter(.data[["inv_case_status"]] == "N") %>%
    dplyr::arrange(.data[["patient_local_id"]]) %>%
    tidylog::distinct(.data[["patient_local_id"]], .keep_all = TRUE) %>%
    dplyr::mutate(
      patient_last_name = .data[["patient_last_name"]] %>%
        str_to_ascii() %>%
        stringr::str_to_upper() %>%
        stringr::str_remove_all(pattern = "\\s+"),
      patient_first_name = .data[["patient_first_name"]] %>%
        str_to_ascii() %>%
        stringr::str_to_upper() %>%
        stringr::str_remove_all(pattern = "\\s+"),
      patient_dob = std_dates(
        .data[["patient_dob"]],
        orders = "ymdT",
        force = "dt",
        train = FALSE
      )
    ) %>%
    dplyr::mutate(
      patient_street_addr = .data[["patient_street_addr_1"]] %>%
        str_to_ascii() %>%
        stringr::str_to_upper() %>%
        stringr::str_remove_all(pattern = "\\s+"),
      patient_zip = .data[["patient_zip"]] %>%
        str_to_ascii() %>%
        stringr::str_trunc(width = 5, ellipsis = "")
    ) %>%
    dplyr::arrange(
      .data[["patient_last_name"]],
      .data[["patient_first_name"]],
      .data[["patient_dob"]],
      .data[["patient_street_addr"]],
      .data[["patient_zip"]]
    ) %>%
    tidylog::distinct(
      .data[["patient_last_name"]],
      .data[["patient_first_name"]],
      .data[["patient_dob"]],
      .data[["patient_street_addr"]],
      .data[["patient_zip"]],
      .keep_all = TRUE
    )
}

#' @rdname process_nbs
#'
#' @export
process_positive_tests <- function(
  data = read_file_delim(path_pcr(date)),
  date = NULL
) {
  data %>%
    janitor::clean_names() %>%
    tidylog::inner_join(
      process_positive_people(date = date),
      by = "inv_local_id",
      suffix = c("", "_inv_")
    ) %>%
    dplyr::select(-dplyr::ends_with("_inv_")) %>%
    tidylog::filter(
      stringr::str_detect(.data[["lab_result"]], "(?i)positive|presumpt")
    )
}

#' @rdname process_nbs
#'
#' @export
process_negative_tests <- function(
  data = read_file_delim(path_pcr(date)),
  date = NULL
) {
  data %>%
    janitor::clean_names() %>%
    tidylog::inner_join(
      process_negative_people(date = date),
      by = "inv_local_id",
      suffix = c("", "_inv_")
    ) %>%
    dplyr::select(-dplyr::ends_with("_inv_")) %>%
    tidylog::filter(
      stringr::str_detect(.data[["lab_result"]], "(?i)negative")
    )
}
