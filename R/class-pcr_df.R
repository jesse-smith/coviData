#' Construct or Validate a New Object of Class `pcr_df`
#'
#' @description
#' `new_pcr_df()` creates a new `pcr_df` object as a subclass of `tbl_df`, `tbl`,
#' and `data.frame`. This function is optimized for performance, checks are
#' reduced to a minimum.
#'
#' `validate_pcr_df()` checks an `pcr_df` object for internal consistency.
#' Correct behavior can be guaranteed if and only if this function runs without
#' raising an error.
#'
#' @inheritParams tibble:new_tibble
new_pcr_df <- function(x, nrow) {
  tibble::new_tibble(x, date_stamp = date_stamp, nrow = nrow, class = "pcr_df")
}

#' @inherit new_pcr title description params
validate_pcr_df <- function(x) {

  assertthat::has_attr(x, "date_stamp")

  ds <- attr(x, "date_stamp")

  assertthat::assert_that(
    lubridate::is.Date(ds),
    msg = "The 'date_stamp' attribute of a `pcr_df` must be of class `Date`"
  )

  assertthat::assert_that(
    ds > as.Date("2020-01-01"),
    msg = paste0("The 'date_stamp' attribute of a `pcr_df` ",
                 "must not be earlier than 2020-01-01")
  )

  assertthat::assert_that(
    ds <= lubridate::today("US/Central"),
    msg = paste0("The 'date_stamp' attribute of a `pcr_df` ",
                 "must not be later than the current date in US CST time.")
  )

  tibble::validate_tibble(x)
}
