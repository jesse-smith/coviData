#' Prepare Vaccine Data for Analysis
#'
#' @description
#' `vac_prep()` prepares vaccination data for analysis. It performs the
#' following steps:
#' \itemize{
#'   \item{Standardize variable names}
#'   \item{Extract ZIP codes; convert non-Shelby to `"Other"`, invalid to `NA`}
#'   \item{Determine Shelby County residency based on ZIP}
#'   \item{Convert `dose_count` to `integer`}
#' }
#'
#' @param .data Vaccination data, as loaded by
#'   \code{\link[coviData:vac_load]{vac_load()}}
#'
#' @param distinct Should data be de-duplicated to distinct individuals?
#'
#' @return A `tibble`
#'
#' @export
vac_prep <- function(.data, distinct = FALSE) {
  .data %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      address_zip = vac_parse_zip(.data[["address_zip"]]),
      resident = .data[["address_zip"]] != "Other",
      dose_count = as.integer(.data[["dose_count"]])
    ) %>%
    dplyr::filter(.data[["dose_count"]] <= 2L) %>%
    dplyr::mutate(.row_id_tmp_ = dplyr::row_number()) %>%
    dplyr::arrange(dplyr::desc(.data[["dose_count"]])) %>%
    purrr::when(
      distinct ~ coviData::coalesce_dupes(., .data[["asiis_pat_id_ptr"]]),
      ~ .
    ) %>%
    dplyr::arrange(.data[[".row_id_tmp_"]]) %>%
    dplyr::select(-".row_id_tmp_")
}

#' Parse Zip Codes from Vaccination Data
#'
#' `vac_parse_zip()` parses ZIP codes from the `address_zip` field in
#' vaccination data. It extracts the first 5-digit sequence in the field and
#' compares with a list of valid Shelby County ZIP codes.
#'
#' @param string A `character` vector of ZIP codes
#'
#' @return A `character` vector of 5-digit ZIP codes. Invalid values are
#'   converted to missing; valid ZIP codes outside Shelby County are converted
#'   to `"Other"`.
#'
#' @export
vac_parse_zip <- function(string) {

  valid_zip <- c(
    '38002', '38016', '38017', '38018', '38028', '38053', '38103', '38104',
    '38105', '38106', '38107', '38108', '38109', '38111', '38112', '38114',
    '38115', '38116', '38117', '38118', '38119', '38120', '38122', '38125',
    '38126', '38127', '38128', '38133', '38134', '38135', '38138', '38139',
    '38141'
  ) %>%
    paste0(collapse = "|")

  string %>%
    stringr::str_extract("[0-9]{5}") %>%
    {dplyr::if_else(stringr::str_detect(., valid_zip), ., "Other")}
}
