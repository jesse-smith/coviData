#' Parse Dates to Standard Format
#'
#' `std_dates` standardizes a date vector and returns a vector in `Date` or
#' `POSIXct` format, depending on whether there is sub-daily information
#' available in the data.
#'
#' @param x A vector of dates in a `Date`- or `POSIXct`-like format, or as
#'   character strings
#'
#' @param orders The orders to use when parse character strings with
#'   \code{\link[lubridate:parse_date_time]{parse_date_time()}}
#'
#' @return A `Date` or `POSIXct` vector
#'
#' @export
std_dates <- function(
  .x,
  orders = c("dmy", "mdy", "ymd", "dmyT", "mdyT", "ymdT")
) {
  .x %>%
    janitor::convert_to_datetime(
      character_fun = chr_to_dttm,
      string_conversion_failure = "warning"
    ) %>%
    dttm_to_dt()
}

#' Coerce Datetimes to Dates if No Information is Lost
#'
#' `dttm_to_dt()` converts `POSIX` objects to `Date` objects when there is no
#' additional information contained in the `POSIX` format (i.e. there is no
#' sub-daily information).
#'
#' Specifically, `dttm_to_dt` checks whether all sub-daily information is the
#' same for each value in the datetime vector. If so, no additional information
#' is gained by using a `POSIX` format over the simpler `Date` format, and
#' the data is coerced.
#'
#' @param x A `POSIXct` or `POSIXlt` (i.e. a datetime) vector
#'
#' @return Either a `POSIXct` vector or a `Date` vector
#'
#' @noRd
dttm_to_dt <- function(.x) {
  # If .x is already Date type, return as-is
  if (lubridate::is.Date(.x)) return(.x)

  # Otherwise, check for any additional information in the variable
  t <- (
    lubridate::hour(.x) +
      lubridate::minute(.x) / 60 +
      lubridate::second(.x) / 3600
  )

  if (all(t == stats::median(t, na.rm = TRUE) | is.na(t))) {
    lubridate::as_date(.x)
  } else if (lubridate::is.POSIXlt(.x)) {
    lubridate::as_datetime(.x)
  } else {
    .x
  }
}

#' Parse Dates in Character Format to Datetime Format
#'
#' `chr_to_dttm` standardizes a datetime vector in character format and returns
#' a vector in `POSIXct` format.
#'
#' @param x A vector of dates in a `Date`- or `POSIXct`-like character format
#'
#' @param tz timezone
#'
#' @param orders The orders to use when parse character strings with
#'   \code{\link[lubridate:parse_date_time]{parse_date_time()}}
#'
#' @return A `POSIXct` vector
#'
#' @noRd
chr_to_dttm <- function(
  .x,
  tz = "UTC",
  orders = c("mdy", "dmy", "ymd", "mdyT", "dmyT", "ymdT", "Tmdy", "Tdmy", "Tymd")
) {
  .x %>%
    stringr::str_replace(pattern = "^$", replacement = NA_character_) %>%
    lubridate::parse_date_time(orders = orders) %>%
    lubridate::as_datetime()
}
