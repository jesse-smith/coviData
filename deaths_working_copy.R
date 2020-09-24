deaths_surv <- readxl::read_excel(
  path = "V:/EPI DATA ANALYTICS TEAM/MORTALITY DATA/Working Copy Death  Epi.xlsx",
  trim_ws = TRUE,
  guess_max = .Machine$integer.max %/% 100L,
  progress = TRUE
)

clean_nbs_id <- function(.nbs_id) {
  .nbs_id %>%
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

death_file <- paste0(
  "V:/EPI DATA ANALYTICS TEAM/MORTALITY DATA/cleaned_copies/",
  "cleaned_deaths_",
  Sys.Date(),
  ".xlsx"
)

deaths_surv %>%
  dplyr::mutate(NBS = clean_nbs_id(NBS)) %>%
  openxlsx::write.xlsx(
    file = death_file
  )
