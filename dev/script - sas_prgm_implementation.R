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
  dplyr::filter(standardize_string(result) != "Indeterminate") %>%
  dplyr::mutate(
    patient_last_name = standardize_string(patient_last_name),
    patient_first_name = standardize_string(patient_first_name),
    patient_middle_name = standardize_string(patient_middle_name)
  ) %>%
  dplyr::mutate(
    patient_zip = pt_zipcode %>%
      as.character() %>%
      stringr::str_trunc(width = 5, ellipsis = "") %>%
      factor(),
    patient_street_addr = pt_add_1 %>% snakecase::to_title_case(sep_out = " ")
  ) %>%
  dplyr::select(-pt_zipcode, -pt_add_1)
ael_merged

# TODO: Move antijoin to top, remove auth_date filter, apply remaining to both

# Pre-process NBS ##############################################################

nbs %>%
  # Filter to only our jurisdiction and county
  # Jurisdiction should get dropped as a constant, so handle it flexibly
  dplyr::filter(
    dplyr::across(
      dplyr::contains("jurisdiction_nm"),
      ~ standardize_string(.x) == "Memphis Shelby County"
    ),
    standardize_string(alt_county) %in% c("Shelby County", missings)
  ) %>%
  # We can approximate missing ages using a few different dates
  # TODO: Discuss changing order of preference and adding dates
  dplyr::mutate(
    # The lubridate package is the go-to for handling dates and times.
    # Generally, prefer `Interval` if possible, because it contains the most
    # information (start, end, and time between). Since we don't know the start
    # or end values for `age_in_years`, I'm transforming it to a `Duration`,
    # which is just a length of time measured in seconds.
    current_age = lubridate::interval(patient_dob, lubridate::today()),
    spcol_age = lubridate::interval(patient_dob, specimen_coll_dt),
    reported_age = lubridate::ddays(patient_age_reported),
    # inv_start_age = lubridate::interval(patient_dob, inv_start_dt),
    # onset_age = lubridate::interval(patient_dob, illness_onset_dt),
    # age_in_years = lubridate::dyears(age_in_years),

    # This step casts all of the above to the same type (a `Period` in years)
    # and sets negative values to missing. I like `Period` because it lets you
    # do math like a regular number in your unit of choice.
    dplyr::across(
      c(age_in_years, dplyr::ends_with("_age")),
      ~ .x %>%
        lubridate::as.period(unit = "year") %>%
        purrr::when(. < lubridate::years(0) ~ lubridate::years(NA), ~ .)
    ),
    # Combining columns in dplyr is done with `coalesce()`; it takes the first
    # non-missing value it finds for each observation
    age = dplyr::coalesce(age_in_years, spcol_age, current_age)
  ) %>%
  # coviData has a function for cleaning up names called `standardize_string()`
  # Type `?coviData::standardize_string()` to see more about it.
  dplyr::mutate(
    patient_last_name = standardize_string(patient_last_name),
    patient_first_name = standardize_string(patient_first_name),
    patient_middle_name = standardize_string(patient_middle_name)
  ) %>%
  # Just re-implementing SAS code for addresses; want to try using the
  # postmastr package for this
  dplyr::mutate(
    patient_zip = patient_zip %>%
      as.character() %>%
      stringr::str_trunc(width = 5, ellipsis = "") %>%
      factor(),
    patient_street_addr = patient_street_addr %>%
      snakecase::to_title_case(sep_out = " ")
  )

# Stopped on line 1174 of sas program - starting race recoding
