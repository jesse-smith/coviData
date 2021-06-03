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
vac_prep <- function(
  data = vac_load(),
  distinct = FALSE,
  filter_doses = TRUE,
  filter_residents = TRUE
) {
  data %>%
    janitor::clean_names() %>%
    vac_mutate() %>%
    purrr::when(filter_residents ~ vac_filter_residents(.), ~ .) %>%
    purrr::when(filter_doses ~ vac_filter_doses(.), ~ .) %>%
    purrr::when(distinct ~ vac_distinct(.), ~ .)
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

  valid_zip <- paste0(coviData::shelby_zip, collapse = "|")

  string %>%
    stringr::str_extract("[0-9]{5}") %>%
    {dplyr::if_else(stringr::str_detect(., valid_zip), ., "Other")}
}

#' Create and Transform Variables in Vaccination Data
#'
#' @param data Vaccination data from TennIIS
#'
#' @return The data with parsed `address_zip`, `integer` `dose_count`, and
#'   added columns `resident` (`logical`), `max_doses` (`integer`),
#'   `recip_fully_vacc` (`logical`)
#'
#' @keywords internal
vac_mutate <- function(data) {
  dplyr::mutate(
    data,
    address_zip = vac_parse_zip(.data[["address_zip"]]),
    resident = .data[["address_zip"]] != "Other",
    dose_count = as.integer(.data[["dose_count"]]),
    max_doses = dplyr::case_when(
      .data[["cvx_code"]] %in% c("210", "212") ~ 1L,
      .data[["cvx_code"]] %in% c("207", "208") ~ 2L,
      TRUE ~ NA_integer_
    ),
    recip_fully_vacc = .data[["dose_count"]] == .data[["max_doses"]]
  )
}

#' Filter to Valid Doses in Vaccination Data
#'
#' @param data Vaccination data with `numeric` `dose_count` and `max_doses`
#'   columns
vac_filter_doses <- function(data) {
  dplyr::filter(
    data,
    .data[["dose_count"]] <= 2L,
    .data[["dose_count"]] <= .data[["max_doses"]] | is.na(.data[["max_doses"]])
  )
}

#' Filter to Possible Shelby Residents in Vaccination Data
#'
#' @param data Vaccination data with `logical` `resident` column
#'
#' @return `data` filtered to potential residents
#'
#' @keywords internal
vac_filter_residents <- function(data) {
  dplyr::filter(data, .data[["resident"]] | is.na(.data[["resident"]]))
}

#' Deduplicate Vaccination Data
#'
#' Create distinct vaccination data. Sorts by `dose_count` prior to calling
#' \code{\link[dplyr:distinct]{distinct()}}.
#'
#' @param data Vaccination data
#'
#' @param cols \code{\link[dplyr:dplyr_data_masking]{<data masking>}}
#'   `character` vector of variables to use when determining uniqueness. If
#'   there are multiple rows for a given combination of inputs, only the first
#'   row will be preserved. The default de-duplicates by person.
#'
#' @param desc Should `dose_count` be
#'   \code{\link[dplyr:arrange]{arrange}}`d` in descending order?
#'
#' @return An object of the same type as `data`. The output has the following
#'   properties: \itemize{
#'     \item Rows are a subset of the input but appear in the same order
#'     \item Columns are not modified
#'     \item Groups are not modified
#'     \item Data frame attributes are preserved
#'   }
#'
#' @keywords internal
vac_distinct <- function(
  data,
  cols = "asiis_pat_id_ptr",
  desc = TRUE
) {

  assert_bool(desc)

  cols <- assert_cols(data, {{ cols }})
  cols <- rlang::syms(cols)

  data %>%
    dplyr::mutate(.row_id_tmp_ = dplyr::row_number()) %>%
    dplyr::arrange(
      purrr::when(.data[["dose_count"]], desc ~ dplyr::desc(.), ~ .)
    ) %>%
    dplyr::distinct(., !!!cols) %>%
    dplyr::arrange(.data[[".row_id_tmp_"]]) %>%
    dplyr::select(-".row_id_tmp_")
}
