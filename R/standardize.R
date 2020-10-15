standardize_names <- function(.x) {

  esc_msg <- paste0(
    " substitute character(s) have been removed.",
    " See warnings for more detail on which characters could not be encoded."
  )

  .x %>%
    stringr::str_conv(encoding = "UTF-8") %T>%
    detect_and_replace(pattern = "[\ufffd\u001a]", msg = esc_msg) %>%
    stringr::str_remove_all(pattern = "['\"]") %>%
    stringr::str_replace_all(pattern = "[^a-zA-Z0-9 ]", replacement = " ") %>%
    stringr::str_squish() %>%
    stringr::str_to_title()
}

standardize_dates <- function(
  .data,
  suffix = c("dt", "dob", "date"),
  orders = c("dmy", "mdy", "ymd", "dmyT", "mdyT", "ymdT"),
  ...
) {
  .data %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::ends_with(suffix, ignore.case = TRUE),
        .fns = ~ .x %>%
          as.character() %>%
          lubridate::parse_date_time(orders = orders) %>%
          dttm_to_dt()
      )
    )
}

standardize_nbs_id <- function(id) {
  id %>%
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
