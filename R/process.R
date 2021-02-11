#' Run the Cases Workflow for Daily Report
#'
#' `process_positive_people()` runs the positive people (cases) workflow from
#' the SAS program.
#'
#' @param date The date of the files to use
#'
#' @return A `tibble` containing the information from the NBS investigations
#'   file on all cases
#'
#' @export
process_positive_people <- function(date = NULL) {

  missings <- c(NA_character_, "Na", "", " ")

  path_inv(date) %>%
    read_file_delim() %>%
    janitor::clean_names() %>%
    # Filter to only our jurisdiction and county
    # Jurisdiction should get dropped as a constant during pre-processing, so
    # handle it flexibly
    tidylog::filter(
      dplyr::across(
        dplyr::contains("jurisdiction_nm"),
        ~ stringr::str_detect(.x, "(?i).*Memphis.*Shelby.*County.*")
      ),
      alt_county %in% c("Shelby County", missings)
    ) %>%
    # Filter to patients in our state
    # Note: This gets rid of records missing `alt_county` AND `patient_state`
    tidylog::filter(
      alt_county == "Shelby County" |
        as.numeric(patient_state) %in% c(47, NA)
    ) %>%
    dplyr::mutate(
      patient_last_name = patient_last_name %>%
        str_to_ascii() %>%
        stringr::str_to_upper() %>%
        stringr::str_remove_all(pattern = "\\s+"),
      patient_first_name = patient_first_name %>%
        str_to_ascii() %>%
        stringr::str_to_upper() %>%
        stringr::str_remove_all(pattern = "\\s+")
    ) %>%
    dplyr::mutate(
      patient_street_addr = patient_street_addr_1 %>%
        str_to_ascii() %>%
        stringr::str_to_upper() %>%
        stringr::str_remove_all(pattern = "\\s+"),
      patient_zip = patient_zip %>%
        str_to_ascii() %>%
        stringr::str_trunc(width = 5, ellipsis = "")
    ) %>%
    # Filter to confirmed and probable cases
    tidylog::filter(inv_case_status %in% c("C", "P")) %>% # Check that inv_local_id is unique - matches SAS on 2020-11-15
    dplyr::arrange(inv_local_id) %>%
    tidylog::distinct(inv_local_id, .keep_all = TRUE) %>%
    # Filter any non-distinct patient_local_ids - matches SAS on 2020-11-15
    dplyr::arrange(patient_local_id, inv_case_status) %>%
    tidylog::distinct(patient_local_id, .keep_all = TRUE) %>%
    # Filter non-distinct people based on name and DOB - matches SAS on 2020-11-15
    # SAS code standardizes case and removes whitespace
    # TODO: Switch to `standardize_string()`
    dplyr::arrange(
      patient_last_name,
      patient_first_name,
      patient_dob,
      inv_case_status
    ) %>%
    tidylog::distinct(
      patient_last_name,
      patient_first_name,
      patient_dob,
      .keep_all = TRUE
    )
}
