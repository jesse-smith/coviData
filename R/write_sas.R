#' Separate +/- Investigation/PCR Data Files for SAS
#' @export
write_sas_nbs <- function(
  pcr = process_pcr(inv = inv),
  inv = process_inv(),
  dir = "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/Data for SAS",
  date = attr(pcr, "date"),
  force = FALSE
) {
  write_sas_pos(pos(inv), type = "inv", dir = dir, date = date, force = force)
  write_sas_pos(pos(pcr), type = "pcr", dir = dir, date = date, force = force)
  write_sas_neg(neg(inv), type = "inv", dir = dir, date = date, force = force)
  write_sas_neg(neg(pcr), type = "pcr", dir = dir, date = date, force = force)
}

#' Write Positive Investigation and PCR Data to CSV for SAS
#' @export
write_sas_pos <- function(
  data = pos(process_inv()),
  type = c("inv", "pcr"),
  dir = "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/Data for SAS",
  date = attr(data, "date"),
  force = FALSE
) {
  type <- rlang::arg_match(type)[[1L]]
  date <- date_inv(date)
  assert_bool(force)

  path <- path_create(dir, paste0(type, "_pos_", date), ext = ".csv")

  if (fs::file_exists(path) && !force) {
    rlang::abort(
      "A file already exists at this location; to overwrite, use `force = TRUE`"
    )
  }

  data %>%
    sas_trans_demog() %>%
    sas_std_chr() %>%
    write_file_delim(path = path)
}

#' Write Negative Investigation and PCR Data to CSV for SAS
#' @export
write_sas_neg <- function(
  data = neg(process_pcr()),
  type = c("inv", "pcr"),
  dir  = "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/Data for SAS",
  date = attr(data, "date"),
  force = FALSE
) {
  type <- rlang::arg_match(type)[[1L]]
  date <- date_inv(date)
  assert_bool(force)

  path <- path_create(dir, paste0(type, "_pos_", date), ext = ".csv")

  if (fs::file_exists(path) && !force) {
    rlang::abort(
      "A file already exists at this location; to overwrite, use `force = TRUE`"
    )
  }

  data %>%
    dplyr::select("inv_local_id", "specimen_coll_dt") %>%
    write_file_delim(path = path)
}

sas_trans_demog <- function(data) {
  data %>%
    sas_trans_age(.before = 1L) %>%
    sas_trans_sex(.before = 2L) %>%
    sas_trans_race(.before = 3L) %>%
    sas_trans_ethnicity(.before = 4L)
}

sas_trans_age <- function(data, .before = NULL) {
  data %>%
    dplyr::mutate(
      inv_start_dt_tmp_ = std_dates(
        .data[["inv_start_dt"]],
        orders = "ymdT",
        force = "dt"
      ),
      age_in_years_tmp_ = as.double(.data[["age_in_years"]]),
      age_test_tmp_ = (.data[["specimen_coll_dt"]] - .data[["patient_dob"]]) %>%
        lubridate::as.duration() %>%
        divide_by(lubridate::dyears(1L)),
      age_start_dt_tmp_ = (.data[["inv_start_dt_tmp_"]] - .data[["patient_dob"]]) %>%
        lubridate::as.duration() %>%
        divide_by(lubridate::dyears(1L)),
      dplyr::across(dplyr::matches("age_.*_tmp_"), std_age),
      age_calc = dplyr::coalesce(
        .data[["age_in_years_tmp_"]],
        .data[["age_test_tmp_"]],
        .data[["age_start_dt_tmp_"]]
      ) %>%
        as.integer(),
      .before = .before
    ) %>%
    dplyr::select(-dplyr::ends_with("_tmp_"))
}

sas_trans_sex <- function(data, .before = NULL) {
  dplyr::mutate(
    data,
    sex_calc = .data[["patient_current_sex"]] %>%
      stringr::str_to_upper() %>%
      stringr::str_remove_all("[^FMALE]") %>%
      stringr::str_extract("^[MF]"),
    sex_calc = dplyr::case_when(
      .data[["sex_calc"]] == "F" ~ "Female",
      .data[["sex_calc"]] == "M" ~ "Male"
    ),
    .before = .before
  )
}

sas_trans_race <- function(data, .before = NULL) {
  dplyr::mutate(
    data,
    race_calc = demog_race_grp_(.data[["patient_race_calc"]]),
    .before = .before
  )
}

sas_trans_ethnicity <- function(data, .before = NULL) {
  dplyr::mutate(
    data,
    ethnicity_calc = dplyr::case_when(
      .data[["patient_ethnicity"]] == "2135-2" ~ "Hispanic/Latino",
      .data[["patient_ethnicity"]] == "2186-5" ~ "Not Hispanic/Latino",
      TRUE ~ NA_character_
    ),
    .before = .before
  )
}

demog_race_grp_ <- function(chr) {
  chr <- chr %>%
    stringr::str_to_upper() %>%
    stringr::str_squish() %>%
    stringr::str_extract("[A-Z ]+()")

  baa <- "Black/African American"
  w <- "White"
  # api <- "Asian/Pacific Islander"
  # aian <- "American Indian/Alaskan Native"

  dplyr::case_when(
    stringr::str_detect(chr, "(BLACK)|(AFRICAN)") ~ baa,
    stringr::str_detect(chr, "(WHITE)|(CAUCASIAN)") ~ w,
    stringr::str_detect(chr, "(INDIAN)|(NATIVE)") ~ "Other",
    stringr::str_detect(chr, "(ASIAN)|(PACIFIC)") ~ "Other",
    stringr::str_detect(chr, "OTHER") ~ "Other",
    TRUE ~ NA_character_
  )
}

sas_std_chr <- function(data) {
  dplyr::mutate(
    data,
    dplyr::across(where(is.character) | where(is.factor), ~ to_chr_ascii(.x))
  )
}

#' Convert Atomic Vectors to ASCII
#'
#' `to_chr_ascii()` converts atomic vectors to ASCII representation. It powers
#' `as_chr_ascii()`.
#'
#' @param x An atomic vector
#'
#' @return `x` as an ASCII encoded character vector
#'
#' @noRd
to_chr_ascii <- function(x) {
  x %>%
    as.character() %>%
    stringi::stri_trans_general("Any-Latin;Latin-ASCII") %>%
    stringi::stri_enc_toascii()
}


std_age <- function(x) {
  x_dbl <- as.double(x)
  dplyr::if_else(0 <= x_dbl & x_dbl < 115, x_dbl, NA_real_)
}
