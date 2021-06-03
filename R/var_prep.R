#' Parse Variant Data from NIT Project
#'
#' `var_parse()` deduplicates variant data from the NIT REDcap project and
#' parses key variables into a standard format. Currently parsed variables are:
#' \itemize{
#'   \item{`nbs`}
#'   \item{`firstname`}
#'   \item{`lastname`}
#'   \item{`dob`}
#'   \item{`specimendate`}
#' }
#'
#' @param data NIT variant data
#'
#' @return A `tibble` of parsed data
#'
#' @export
var_parse <- function(data = var_load()) {
  data %>%
    dplyr::mutate(
      nbs = parse_nbs(.data[["nbs"]]),
      firstname = std_names(.data[["firstname"]]),
      lastname = std_names(.data[["lastname"]]),
      dob = std_dates(
        .data[["dob"]],
        force = "dt",
        train = FALSE,
        orders = c("ymd", "ymdT", "ymdR")
      ),
      specimendate = std_dates(
        .data[["specimendate"]],
        train = FALSE,
        orders = c("ymd", "ymdT", "ymdR")
      )
    ) %>%
    dplyr::arrange(.data[["specimendate"]]) %>%
    dplyr::distinct(
      dplyr::across(c("nbs", "firstname", "lastname", "dob")),
      .keep_all = TRUE
    ) %>%
    dplyr::arrange(as.numeric(.data[["record_id"]]))
}

parse_nbs <- function(string) {
  string %>%
    stringr::str_extract("[0-9]{8}") %>%
    {paste0("CAS", ., "TN01")}
}
