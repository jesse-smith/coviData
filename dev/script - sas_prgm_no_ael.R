# Load Data ####################################################################
nbs           <- load_nbs() %>% preprocess()
pcr           <- load_pcr() %>% preprocess()

# Pre-process AEL ##############################################################
missings <- c(NA_character_, "Na", "", " ")

nbs %>%
  # Filter to only our jurisdiction and county
  # Jurisdiction should get dropped as a constant during pre-processing, so
  # handle it flexibly
  # Matches SAS on 2020-11-15
  dplyr::filter(
    dplyr::across(
      dplyr::contains("jurisdiction_nm"),
      ~ .x == "Memphis Shelby County"
    ),
    alt_county %in% c("Shelby County", missings)
  ) ->
nbs_shelby

# Check case status - matches SAS on 2020-11-15
nbs_shelby %>%
  janitor::tabyl(inv_case_status) %>%
  janitor::adorn_totals() %>%
  dplyr::as_tibble()

# Check probable deaths - matches SAS on 2020-11-15
nbs_shelby %>%
  janitor::tabyl(inv_case_status, die_from_illness_ind) %>%
  janitor::adorn_totals(where = c("row", "col")) %>%
  dplyr::as_tibble() %>%
  dplyr::select(inv_case_status, Deaths = Y, Total) %>%
  dplyr::filter(inv_case_status == "P")

# Filter to patients in our state - matches SAS on 2020-11-15
# Note: This gets rid of records missing `alt_county` AND `patient_state`
nbs_shelby %>%
  dplyr::filter(
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
  ) ->
nbs_shelby_tn

# Filter to confirmed and probable cases - matches SAS on 2020-11-15
nbs_shelby_tn %>%
  dplyr::filter(inv_case_status %in% c("C", "P")) ->
nbs_positive

# Check confirmed and probable - matches SAS on 2020-11-15
nbs_positive %>%
  janitor::tabyl(inv_case_status) %>%
  janitor::adorn_totals() %>%
  dplyr::as_tibble()

nbs_positive %>%
  # Check that inv_local_id is unique - matches SAS on 2020-11-15
  dplyr::arrange(inv_local_id) %>%
  dplyr::distinct(inv_local_id, .keep_all = TRUE) %>%
  # Filter any non-distinct patient_local_ids - matches SAS on 2020-11-15
  dplyr::arrange(patient_local_id, inv_case_status) %>%
  dplyr::distinct(patient_local_id, .keep_all = TRUE) ->
nbs_distinct

# Filter non-distinct people based on name and DOB - matches SAS on 2020-11-15
# SAS code standardizes case and removes whitespace
# TODO: Switch to `standardize_string()`
nbs_distinct %>%
  dplyr::arrange(patient_last_name, patient_first_name, patient_dob, inv_case_status) %>%
  dplyr::distinct(patient_last_name, patient_first_name, patient_dob, .keep_all = TRUE) ->
nbs_cases

# Check case status after dedup - matches SAS on 2020-11-15
nbs_cases %>%
  janitor::tabyl(inv_case_status) %>%
  janitor::adorn_totals() %>%
  dplyr::as_tibble()

# Get status breakdown - matches SAS on 2020-11-15
nbs_cases %>%
  janitor::tabyl(inv_case_status) %>%
  janitor::adorn_totals() %>%
  dplyr::as_tibble()

# Negatives ####################################################################

nbs_shelby_tn %>%
  # Filter to negatives - matches SAS on 2020-11-15
  dplyr::filter(inv_case_status == "N") %>%
  # Filter non-distinct patient_ids - matches SAS on 2020-11-15
  dplyr::arrange(patient_local_id) %>%
  dplyr::distinct(patient_local_id, .keep_all = TRUE) %>%
  # Filter non-distinct addresses and people - matches SAS on 2020-11-15
  dplyr::arrange(
    patient_last_name,
    patient_first_name,
    patient_dob,
    patient_street_addr,
    patient_zip
  ) %>%
  dplyr::distinct(
    patient_last_name,
    patient_first_name,
    patient_dob,
    patient_street_addr,
    patient_zip,
    .keep_all = TRUE
  ) ->
nbs_negative_ppl

# PCR Positives ################################################################

# Merge PCR with AEL+NBS - matches SAS on 2020-11-15
dplyr::inner_join(
  nbs_cases,
  pcr,
  by = "inv_local_id"
) %>%
  # Filter to positive - matches SAS on 2020-11-15
  dplyr::filter(lab_result %in% c("Positive", "Presumptive Positive")) ->
positive_pcr

# PCR Negatives ################################################################

# Merge PCR with AEL+NBS - matches SAS on 2020-11-15
dplyr::inner_join(
  nbs_negative_ppl,
  pcr,
  by = "inv_local_id"
) %>%
  # Filter to negative - matches SAS on 2020-11-15
  dplyr::filter(lab_result == "Negative") ->
negative_pcr

# That's a wrap!!!

# On to analysis - and the associated data munging
