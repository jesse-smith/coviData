#' Prepare 3rd Dose Vaccine Data for Analysis
#'
#' @description
#' `vac_prep_dose3()` prepares vaccination data for third doses for analysis. It performs the
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
#'
#' @param filter_residents Should records for individuals outside of Shelby Co
#'   be removed? This retains records whose residence is unknown.
#'
#' @return A `tibble`
#'
#' @export
vac_prep_dose3 <- function(
  data = read_vac(),
  distinct = FALSE,
  filter_residents = TRUE
) {
 try1 <- data %>%
    janitor::clean_names() %>%
    vac_mutate() %>%
    purrr::when(filter_residents ~ vac_filter_residents(.), ~ .) %>%
  #  purrr::when(filter_doses ~ vac_filter_doses(.), ~ .) %>%
    purrr::when(distinct ~ vac_distinct(.), ~ .)

 try1 %>% subset(lubridate::mdy(vacc_date) >= lubridate::ymd("2021-08-13")) %>%
 #restrict to Moderna (207) and Pfizer (208) and other/unspecified (213) only
 subset(cvx_code %in% c("207", "208", "213")) %>%
 #restrict to dose = 3
 subset(dose_count %in% "3")

}


#' Prepare Vaccine Data including legit 1st, 2nd, and 3rd doses for Analysis
#'
#' @description
#' `vac_prep_all()` prepares vaccination data including valid third doses for
#' analysis. It performs the following steps:
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
#' @return A `tibble`
#'
#' @export
vac_prep_all <- function(data = read_vac()) {

  #EXAMPLE of how to run this for 5 days ago
  #vac_prep_all(data = read_vac(Sys.Date()-5))

dose12 <- vac_prep(data = data)
dose3 <- vac_prep_dose3(data = data)

dplyr::full_join(dose12, dose3)
}





