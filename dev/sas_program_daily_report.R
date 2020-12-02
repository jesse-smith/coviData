# Variable Creation
cases %>%
  dplyr::mutate(
    illness_onset_dt = lubridate::as_date(illness_onset_dt),
    specimen_coll_dt = lubridate::as_date(specimen_coll_dt),
    inv_start_dt = lubridate::as_date(inv_start_dt),
    active_date = dplyr::coalesce(illness_onset_dt, specimen_coll_dt, inv_start_dt),
    active = dplyr::between(active_date, Sys.Date() - 14, Sys.Date())
  ) ->
cases_daily_rpt

# Active Cases - SAS program had an error, agrees after correction on 2020-11-15
cases_daily_rpt %>%
  dplyr::mutate(
    died = dplyr::if_else(die_from_illness_ind == "Y", "Deceased", NA_character_),
    active = dplyr::if_else(active == TRUE, "Active", NA_character_),
    active = dplyr::coalesce(died, active) %>% tidyr::replace_na("Inactive/Recovered")
  ) %>%
  janitor::tabyl(active) %T>%
  show() ->
active_status

# Closed Investigations
cases_daily_rpt %>%
  janitor::tabyl(investigation_status_cd) %>%
  janitor::adorn_totals() %>%
  dplyr::as_tibble() %T>%
  show() %>%
  dplyr::filter(investigation_status_cd == "C") %>%
  dplyr::pull(n) ->
closed_investigations


# Confirmed, probable, and total cases
# TODO: Merge AEL status with NBS status
cases_daily_rpt %>%
  dplyr::mutate(inv_case_status = tidyr::replace_na(inv_case_status, "P")) %>%
  janitor::tabyl(inv_case_status) %>%
  janitor::adorn_totals() %>%
  dplyr::as_tibble() %>%
  {set_names(dplyr::pull(., n), dplyr::pull(., inv_case_status))} %T>%
  show() ->
case_counts

# Confirmed, probable, and total deaths
cases_daily_rpt %>%
  dplyr::mutate(
    die_from_illness_ind = die_from_illness_ind %>%
      tidyr::replace_na("N") %>%
      stringr::str_replace_all(pattern = "UNK", replacement = "N")
  ) %>%
  janitor::tabyl(die_from_illness_ind) %T>%
  show() ->
death_counts

cases_daily_rpt %>%
  dplyr::mutate()

