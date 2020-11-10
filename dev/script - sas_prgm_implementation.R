# Load Data ####################################################################
ael_today     <- load_ael() %>% preprocess()
ael_yesterday <- load_ael(date = Sys.Date() - 1) %>% preprocess()
nbs           <- load_nbs() %>% preprocess()
pcr           <- load_pcr() %>% preprocess()

# Pre-process AEL ##############################################################

missings <- c(NA_character_, "Na", "", " ")

# Filter today's file
ael_today %>%
  preprocess() %>%
  # Filter by state - matches SAS on 2020-11-09
  dplyr::filter(
    standardize_string(pt_state) %in% c("47", "Tn", missings),
    dplyr::between(auth_date, Sys.Date() - 1, Sys.Date())
  ) ->
ael_today_processed

# Filter yesterday's file
ael_yesterday %>%
  preprocess() %>%
  # Filter by state - matches SAS on 2020-11-09
  dplyr::filter(
    standardize_string(pt_state) %in% c("47", "Tn", missings),
    dplyr::between(auth_date, Sys.Date() - 1, Sys.Date())
  ) ->
ael_yesterday_processed

# Merge and remove indeterminate
dplyr::anti_join(
  ael_today_processed,
  ael_yesterday_processed,
  by = "episode_no"
) %>%
  dplyr::filter(standardize_string(result) != "Indeterminate") ->
ael_merged

# TODO: Move antijoin to top, remove auth_date filter, apply remaining to both

# Pre-process NBS ##############################################################

nbs %>%
  dplyr::filter(
    standardize_string(jurisdiction_nm) == "Memphis Shelby County",
    standardize_string(alt_county) %in% c("Shelby County", missings)
  ) %>%
  dplyr::mutate(
    current_age = Sys.Date() - patient_dob,
    spcol_age = lubridate::duration()
  ) %>%
