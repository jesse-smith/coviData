download_integrated_data()
data <- load_integrated_data(date = Sys.Date())

data %>%
  preprocess() %>%
  dplyr::filter(school_type == "College/Higher Education") %>%
  dplyr::select(school_name) %>%
  dplyr::mutate(
    school_name = standardize_string(school_name) %>%
      stringr::str_remove_all(pattern = "(A )|(An )|(The )") %>%
      stringr::str_remove(pattern = "School$")
  ) %>%
  unique() %>%
  tidytext::unnest_tokens(
    output = token,
    input = school_name,
    to_lower = FALSE,
    drop = FALSE
  ) %>%
  dplyr::mutate(
    suggested = hunspell::hunspell_check(token) %>%
      ifelse(yes = token, no = hunspell::hunspell_suggest(token)) %>%
      purrr::map_chr(~ unlist(.x)[[1]]),
    osa_dist =  stringdist::stringdist(token, suggested, method = "osa"),
    soundex_dist = stringdist::stringdist(token, suggested, method = "soundex"),
    replace  = osa_dist <= 3 | soundex_dist == 0,
    new_token = ifelse(replace, yes = suggested, no = token)
  ) %>%
  dplyr::group_by(school_name) %>%
  dplyr::summarize(new_name = paste0(new_token, collapse = " ")) ->
lookup

data %>%
  preprocess() %>%
  # dplyr::filter(school_type == "College/Higher Education") %>%
  dplyr::select(school_name) %>%
  dplyr::mutate(
    school_name = standardize_string(school_name) %>%
      stringr::str_remove_all(pattern = "(A )|(An )|(The )")
  ) ->
school_names

data %>%
  dplyr::mutate(
    school_name = lookup[[2]][match(school_names[[1]], lookup[[1]])] %>%
      gsub(pattern = "NA", replacement = NA_character_)
  ) ->
clean_schools

clean_schools %>%
  dplyr::filter(
    !is.na(school_name),
    how_school %in% c("Hybrid/Mix", NA, "In-person"),
    !is.na(infecstart), !is.na(infecend)
  ) %>%
  dplyr::mutate(
    infecstart = lubridate::as_date(infecstart),
    infecend   = lubridate::as_date(infecend)
  ) %>%
  dplyr::transmute(
    record_id,
    school_name,
    exposure_window = lubridate::interval(
      start = infecstart - 12,
      end = infecstart - 1
    ) %>% lubridate::int_standardize(),
    infection_window = lubridate::interval(
      start = infecstart,
      end = infecend
    )
  ) ->
intervals

intervals %>%
  dplyr::mutate(exposure_id = record_id, infection_id = record_id, exposure_school = school_name, infection_school = school_name) %>%
  tidyr::expand(nesting(infection_school, infection_id, infection_window), nesting(exposure_school, exposure_id, exposure_window)) %>%
  dplyr::filter(
    lubridate::int_overlaps(exposure_window, infection_window),
    exposure_school == infection_school
  ) %>%
  dplyr::arrange(infection_window, infection_id, exposure_window, exposure_id)

  denoise_factor(
    `Baptist College Of Health Sciences`,
    `Christian Brothers University`,
    `Dr Benjamin L Hooks Job Corps Center`,
    `Empire Beauty School`,
    `LeMoyne-Owen College`,
    `Memphis College of Art`,
    `Remington College`,
    `Rhodes College`,
    `Southwest Tennessee Community College`,
    `Southern College of Optometry`,
    `University Of Memphis`,
    `University Of Tennessee Health Sciences Center`,
    other_level = "Other"
  )
