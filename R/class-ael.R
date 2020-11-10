#' Construct or Validate a New Object of Class `ael_df`
#'
#' @description
#' `new_ael_df()` creates a new `ael_df` object as a subclass of `tbl_df`, `tbl`,
#' and `data.frame`. This function is optimized for performance, checks are
#' reduced to a minimum.
#'
#' `validate_ael_df()` checks an `ael_df` object for internal consistency.
#' Correct behavior can be guaranteed if and only if this function runs without
#' raising an error.
#'
#' @inheritParams tibble:new_tibble
new_ael_df <- function(x, date_stamp, nrow) {
  tibble::new_tibble(x, date_stamp = date_stamp, nrow = nrow, class = "ael_df")
}

#' @inherit new_ael title description params
validate_ael_df <- function(x) {
  tibble::validate_tibble(x)
}
