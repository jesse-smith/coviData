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
#' @param data Vaccination data, as loaded by
#'   \code{\link[coviData:read_vac]{read_vac()}}
#'
#' @param distinct Should data be de-duplicated to distinct individuals? The
#'   latest valid dose will be kept.
#'
#' @param filter_doses Should invalid doses be removed?
#'
#' @param filter_residents Should records for individuals outside of Shelby Co
#'   be removed? This retains records whose residence is unknown.
#'
#' @return A `tibble`
#'
#' @export
vac_prep <- function(
  data = read_vac(date = date),
  distinct = FALSE,
  filter_doses = FALSE,
  filter_residents = FALSE,
  date = NULL
) {

  date <- coviData::date_vac(date)


  vacs <- data %>%
    janitor::clean_names() %>%
    vac_mutate()



  vacs$status <- dplyr::case_when(
    vacs$cvx_code %in% c("300", "229", "301", "230", "302") ~ "Bivalent Booster",
    is.na(vacs$recip_fully_vacc) ~ "Initiated",
    vacs$recip_fully_vacc == FALSE ~ "Initiated",
    vacs$recip_fully_vacc == TRUE ~ "Completed/Monovalent Booster"
  )


  #some people have a bivalent dose and a different dose after that.
  #we want to delineate based on if they have a bivalent dose or not.
  #may a dummy variable to note a higher dose count for those who have a bivalent dose, so we can keep them in the distinct filtering instead of their non-bivalent dose
  vacs$dose_count_real <- vacs$dose_count

  vacs$dose_count <- dplyr::case_when(
    vacs$status == "Bivalent Booster" ~ as.integer(100 + vacs$dose_count_real),
    TRUE ~ vacs$dose_count_real
  )


  #if their bivalent dose is their second or later dose, count as fully vaccinated.  If it is their first dose, count at initiated
  vacs$recip_fully_vacc <- dplyr::case_when(
    vacs$status == "Bivalent Booster" & vacs$dose_count_real > 1 ~ TRUE,
    vacs$status == "Bivalent Booster" & vacs$dose_count_real == 1 ~ FALSE,
    TRUE ~ vacs$recip_fully_vacc
  )


  vacs <- vacs %>%
    purrr::when(distinct ~ vac_distinct(.), ~ .)

  vacs$dose_count <- vacs$dose_count_real

  vacs <- vacs %>% dplyr::select(-dose_count_real)

  return(vacs)

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
      .data[["cvx_code"]] %in% c("219") ~ 3L, #this is pfizer's 3-dose vaccine for those under 5
      .data[["cvx_code"]] %in% c("212") ~ 1L,
      .data[["cvx_code"]] %in% c("207", "208", "217", "218", "210", "221", "228", "211") ~ 2L,
      TRUE ~ NA_integer_
    ),
    recip_fully_vacc = .data[["dose_count"]] >= .data[["max_doses"]]
  )
}




# vac_mutate <- function(data) {
#   dplyr::mutate(
#     data,
#     address_zip = vac_parse_zip(.data[["address_zip"]]),
#     resident = .data[["address_zip"]] != "Other",
#     dose_count = as.integer(.data[["dose_count"]]),
#     max_doses = dplyr::case_when(
#       .data[["cvx_code"]] %in% c("210", "212") ~ 1L,
#       .data[["cvx_code"]] %in% c("207", "208", "217", "218") ~ 2L,
#       TRUE ~ NA_integer_
#     ),
#     recip_fully_vacc = .data[["dose_count"]] == .data[["max_doses"]]
#   )
# }


#' Filter to Valid Doses in Vaccination Data
#'
#' @param data Vaccination data with `numeric` `dose_count` and `max_doses`
#'   columns
#'
#' @return `data` filtered to valid doses
#'
#' @export
#'
#' @keywords internal
vac_filter_doses <- function(data) {
  dplyr::filter(
    data,
    .data[["dose_count"]] <= 2L
   # .data[["dose_count"]] <= .data[["max_doses"]] | is.na(.data[["max_doses"]])
  )
}

#' Filter to Possible Shelby Residents in Vaccination Data
#'
#' @param data Vaccination data with `logical` `resident` column
#'
#' @return `data` filtered to potential residents
#'
#' @export
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
#' @export
#'
#' @keywords internal
vac_distinct <- function(
  data,
  cols = "asiis_pat_id_ptr",
  desc = TRUE
) {

  assert_bool(desc)



  cols <- select_colnames(data, !!!as.list(cols))
  cols <- rlang::syms(cols)

  data %>%
    dplyr::mutate(.row_id_tmp_ = dplyr::row_number()) %>%
    dplyr::arrange(
      purrr::when(.data[["dose_count"]], desc ~ dplyr::desc(.), ~ .)
    ) %>%
    dplyr::distinct(., !!!cols, .keep_all = TRUE) %>%
    dplyr::arrange(.data[[".row_id_tmp_"]]) %>%
    dplyr::select(-".row_id_tmp_")
}
