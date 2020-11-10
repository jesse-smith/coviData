# Load Data ####################################################################
ael_today     <- load_ael()
ael_yesterday <- load_ael(date = Sys.Date() - 1)
nbs           <- load_nbs()
pcr           <- load_pcr()

# Pre-process AEL ##############################################################

ael_today %>%
  preprocess() %>%
  # Filter by state - matches SAS on 2020-11-09
  dplyr::filter(
    standardize_string(pt_state) %in% c("47", "Tn", "", NA_character_)
  )

ael_yesterday %>%
  preprocess() %>%
  # Filter by state - matches SAS on 2020-11-09
  dplyr::filter(
    standardize_string(pt_state) %in% c("47", "Tn", "", NA_character_)
  )
