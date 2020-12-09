#' @export
process_positive_people <- function(date = Sys.Date()) {

  missings <- c(NA_character_, "Na", "", " ")

  load_nbs(date = date) %>%
    preprocess() %>%
    # Filter to only our jurisdiction and county
    # Jurisdiction should get dropped as a constant during pre-processing, so
    # handle it flexibly
    tidylog::filter(
      dplyr::across(
        dplyr::contains("jurisdiction_nm"),
        ~ .x == "Memphis Shelby County"
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
        stringr::str_to_upper() %>%
        stringr::str_remove_all(pattern = "[ \t\n\r]+"),
      patient_first_name = patient_first_name %>%
        stringr::str_to_upper() %>%
        stringr::str_remove_all(pattern = "[ \t\n\r]+")
    ) %>%
    dplyr::mutate(
      patient_street_addr = patient_street_addr_1 %>%
        stringr::str_to_upper() %>%
        stringr::str_remove_all(pattern = "[ \t\n\r]+"),
      patient_zip = patient_zip %>% stringr::str_trunc(width = 5, ellipsis = "")
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