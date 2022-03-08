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
  data = read_vac(),
  distinct = FALSE,
  filter_doses = TRUE,
  filter_residents = TRUE
) {
  vac_third_dose <- coviData:::vac_prep_dose3()%>%
    janitor::clean_names() %>%
    vac_mutate()


  vac_dose1_2 <- data %>%
    janitor::clean_names() %>%
    vac_mutate() %>%
    purrr::when(filter_doses ~ vac_filter_doses(.), ~ .)

  vacs <- dplyr::bind_rows(vac_dose1_2, vac_third_dose)%>%
    purrr::when(filter_residents ~ vac_filter_residents(.), ~ .)

  fully <- dplyr::select(vacs, asiis_pat_id_ptr, dose_count, recip_fully_vacc)%>%
    dplyr::arrange(asiis_pat_id_ptr, desc(dose_count))%>%
    dplyr::distinct(asiis_pat_id_ptr, .keep_all = TRUE)

  dose1 <- subset(vacs, dose_count == "1")%>%
    dplyr::rename(vacc_date1 = vacc_date, cvx_code1 = cvx_code)%>%
    dplyr::select(asiis_pat_id_ptr, vacc_date1, cvx_code1)

  dose2 <- subset(vacs, dose_count == "2")%>%
    dplyr::rename(vacc_date2 = vacc_date, cvx_code2 = cvx_code)%>%
    dplyr::select(asiis_pat_id_ptr, vacc_date2, cvx_code2)

  dose3 <- subset(vacs, dose_count == "3")%>%
    dplyr::rename(vacc_date3 = vacc_date, cvx_code3 = cvx_code)%>%
    dplyr::select(asiis_pat_id_ptr, vacc_date3, cvx_code3)

  vac_ind <- dplyr::left_join(dose1, dose2)%>%
    dplyr::left_join(dose3)%>%
    dplyr::left_join(fully)%>%
    dplyr::rename(dose_count_last = dose_count, recip_fully_vacc_last = recip_fully_vacc)


  #if fully vac, when was the last dose in their series?
  vac_ind$cvx_code1 <- dplyr::case_when(vac_ind$cvx_code1 == "213" & (!is.na(vac_ind$cvx_code2) & vac_ind$cvx_code2 != "213") ~ vac_ind$cvx_code2,
                                        vac_ind$cvx_code1 == "213" & (!is.na(vac_ind$cvx_code3) & vac_ind$cvx_code3 != "213") ~ vac_ind$cvx_code3,
                                        TRUE~vac_ind$cvx_code1)


  #were they a two dose series or not?
  vac_ind$two_dose_series <- dplyr::case_when(
    vac_ind$cvx_code1 %in% c("212") ~ "No",
    vac_ind$cvx_code1 %in% c("207", "208", "217", "218", "210") ~ "Yes"
  )

  #what is their boost date?
  vac_ind$boost_date <- dplyr::case_when(
    vac_ind$two_dose_series == "Yes" ~ vac_ind$vacc_date3,
    vac_ind$two_dose_series == "No" ~ vac_ind$vacc_date2
  )

  vac_ind$boost_date <- lubridate::mdy(vac_ind$boost_date)

  vac_ind$boost_date <- lubridate::as_date(ifelse(vac_ind$boost_date < lubridate::as_date("2021-08-13"), NA, vac_ind$boost_date))

  vac_ind$status <- dplyr::case_when(
    !is.na(vac_ind$boost_date) ~ "Up to date",
    vac_ind$recip_fully_vacc_last == FALSE ~ "Not up to date",
    vac_ind$two_dose_series == "Yes" & lubridate::mdy(vac_ind$vacc_date2) < lubridate::add_with_rollback(Sys.Date(), months(-5)) ~ "Not up to date",
    vac_ind$two_dose_series == "Yes" & lubridate::mdy(vac_ind$vacc_date2) >= lubridate::add_with_rollback(Sys.Date(), months(-5)) ~ "Up to date",
    vac_ind$two_dose_series == "No" & lubridate::mdy(vac_ind$vacc_date1) < lubridate::add_with_rollback(Sys.Date(), months(-2)) ~ "Not up to date",
    vac_ind$two_dose_series == "No" & lubridate::mdy(vac_ind$vacc_date1) >= lubridate::add_with_rollback(Sys.Date(), months(-2)) ~ "Up to date"
  )

  #if they are up to date, do they have a booster?

  vac_ind$up_to_date_with_booster <- dplyr::case_when(
    vac_ind$status == "Up to date" & !is.na(vac_ind$boost_date) ~ "Yes",
    vac_ind$status == "Up to date" & is.na(vac_ind$boost_date) ~ "No",
  )


  vac_ind$status <- ifelse(
    is.na(vac_ind$status), "Undetermined", vac_ind$status
  )

  vacs_all <- dplyr::left_join(vacs, vac_ind, by = "asiis_pat_id_ptr")

  vacs_all %>%
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
      .data[["cvx_code"]] %in% c("212") ~ 1L,
      .data[["cvx_code"]] %in% c("207", "208", "217", "218", "210") ~ 2L,
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
