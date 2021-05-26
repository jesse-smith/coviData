#' Get Dates for Data files
#'
#' `date_inv()` returns the date of an NBS investigation file (if it exists)
#'
#' `date_pcr()` returns the date of an NBS PCR file (if it exists)
#'
#' `date_vac()` returns the date of a TennIIS vaccination file (if it exists)
#'
#' @param date A date (as a `Date` or `"%Y-%m-%d"` string)
#'
#' @return A `Date` (or errors if data does not exist)
#'
#' @keywords internal
#'
#' @name date-data
NULL

#' @rdname date-data
#'
#' @export
date_inv <- function(date = NULL) {
  path_inv(date) %>%
    fs::path_file() %>%
    fs::path_ext_remove() %>%
    stringr::str_extract("[0-9]{1,4}.?[0-9]{1,2}.?[0-9]{1,4}") %>%
    lubridate::ymd()
}

#' @rdname date-data
#'
#' @export
date_pcr <- function(date = NULL) {
  path_pcr(date) %>%
    fs::path_file() %>%
    fs::path_ext_remove() %>%
    stringr::str_extract("[0-9]{1,4}.?[0-9]{1,2}.?[0-9]{1,4}") %>%
    lubridate::mdy()
}


#' @rdname date-data
#'
#' @export
date_vac <- function(date = NULL) {
  path_vac(date) %>%
    fs::path_file() %>%
    fs::path_ext_remove() %>%
    stringr::str_extract("[0-9]{1,4}.?[0-9]{1,2}.?[0-9]{1,4}") %>%
    lubridate::ymd()
}
