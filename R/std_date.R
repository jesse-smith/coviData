#' Parse Dates to Standard Format
#'
#' `std_dates` standardizes a date vector and returns a vector in `Date` or
#' `POSIXct` format, depending on whether there is sub-daily information
#' available in the data.
#'
#' @inheritParams chr_to_dttm
#'
#' @inheritParams dttm_to_dt
#'
#' @param ... Additional arguments to pass to
#'   \code{\link[janitor:convert_to_date]{convert_to_datetime()}}. These
#'   will, in turn, be passed to further methods, including
#'   \code{\link[janitor:excel_numeric_to_date]{excel_numeric_to_date()}},
#'   \code{\link[lubridate:parse_date_time]{parse_date_time()}}, and
#'   \code{\link[base:as.POSIXct]{as.POSIXct()}}.
#'
#' @return A `Date` or `POSIXct` vector
#'
#' @export
std_dates <- function(
  x,
  force = c("none", "dt", "dttm"),
  orders = c("Omdy", "dOmy", "yOmd",
             "OmdyT", "dOmyT", "yOmdT", "TOmdy", "TdOmy", "TyOmd",
             "OmdyR", "dOmyR", "yOmdR", "Omdyr", "dOmyr", "yOmdr",
             "OmdyTz", "dOmyTz", "yOmdTz", "TOmdyz", "TdOmyz", "TyOmdz",
             "OmdyRz", "dOmyRz", "yOmdRz", "Omdyrz", "dOmyrz", "yOmdrz"),
  ...
) {
  x %>%
    janitor::convert_to_datetime(
      orders = orders,
      ...,
      character_fun = chr_to_dttm,
      string_conversion_failure = "warning"
    ) %>%
    dttm_to_dt(force = force)
}

#' Coerce Datetimes to Dates if No Information is Lost
#'
#' `dttm_to_dt()` converts `POSIXt` objects to `Date` objects when there is no
#' additional information contained in the `POSIXt` format (i.e. there is no
#' sub-daily information).
#'
#' Specifically, `dttm_to_dt` checks whether all sub-daily information is the
#' same for each value in the datetime vector. If so, no additional information
#' is gained by using a `POSIXt` format over the simpler `Date` format, and
#' the data is coerced.
#'
#' If the input is scalar (i.e. has length `1L`), then no conversion is
#' attempted
#'
#' @param .x A `Date`, `POSIXct` or `POSIXlt` (i.e. a datetime) vector
#'
#' @param force Whether to force conversion to `Date` (`force = "dt"`) or
#'   `POSIXct` (`force = "dttm"`). The default is no forcing (`force = "none"`).
#'
#' @return Either a `POSIXct` vector or a `Date` vector
dttm_to_dt <- function(.x, force = c("none", "dt", "dttm")) {
  # If force is set, calculations aren't necessary
  force <- rlang::arg_match(force)[[1L]]
  if (force == "dt") {
    return(lubridate::as_date(.x))
  } else if (force == "dttm") {
    return(lubridate::as_datetime(.x))
  }

  # If `.x` is already `Date` or is scalar `POSIXct`, return as-is
  # If `.x` is scalar `POSIXlt`, convert to `POSIXct`
  is_scalar <- vec_size(.x) == 1L
  if (lubridate::is.Date(.x) || (lubridate::is.POSIXct(.x) && is_scalar)) {
    return(.x)
  } else if (lubridate::is.POSIXlt(.x) && is_scalar) {
    return(lubridate::as_datetime(.x))
  }

  # Otherwise, check for any additional information in the variable
  t <- decimal_time(.x)
  t1 <- if (!all(is.na(t))) stats::na.omit(t)[[1L]] else NA_real_

  if (all(t == t1 | is.na(t))) {
    lubridate::as_date(.x)
  } else if (lubridate::is.POSIXlt(.x)) {
    lubridate::as_datetime(.x)
  } else {
    .x
  }
}

#' Extract Time as a Decimal of Hours
#'
#' @param x A vector with hour, minute, and/or second information that can be
#'   extracted by lubridate's `hour()`, `minute()`, and `second()` functions
#'   (respectively).
#'
#' @return A `double` vector of decimal hours
decimal_time <- function(x) {
  lubridate::hour(x) +
    lubridate::minute(x) / 60 +
    lubridate::second(x) / 3600
}

#' Parse Dates in Character Format to Datetime Format
#'
#' `chr_to_dttm` standardizes a datetime vector in character format and returns
#' a vector in `POSIXct` format.
#'
#' @param x A vector of character dates
#'
#' @param tz Optional timezone
#'
#' @param orders The orders to use when parsing character vector with
#'   \code{\link[lubridate:parse_date_time]{parse_date_time()}}
#'
#' @return A `POSIXct` vector
chr_to_dttm <- function(
  x,
  tz = "UTC",
  orders = c("Omdy", "dOmy", "yOmd",
             "OmdyT", "dOmyT", "yOmdT", "TOmdy", "TdOmy", "TyOmd",
             "OmdyR", "dOmyR", "yOmdR", "Omdyr", "dOmyr", "yOmdr",
             "OmdyTz", "dOmyTz", "yOmdTz", "TOmdyz", "TdOmyz", "TyOmdz",
             "OmdyRz", "dOmyRz", "yOmdRz", "Omdyrz", "dOmyrz", "yOmdrz")
) {
  x %>%
    stringr::str_replace(pattern = "^$", replacement = NA_character_) %>%
    lubridate::parse_date_time(orders = orders, tz = tz) %>%
    lubridate::as_datetime()
}
