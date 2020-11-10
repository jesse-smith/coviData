#' Construct or Validate a New Object of Class `nbs_df`
#'
#' @description
#' `new_nbs_df()` creates a new `nbs_df` object as a subclass of `tbl_df`, `tbl`,
#' and `data.frame`. This function is optimized for performance, checks are
#' reduced to a minimum.
#'
#' `validate_nbs_df()` checks an `nbs_df` object for internal consistency.
#' Correct behavior can be guaranteed if and only if this function runs without
#' raising an error.
#'
#' @inheritParams tibble:new_tibble
new_nbs_df <- function(x, date_stamp, nrow) {
  tibble::new_tibble(x, date_stamp = date_stamp, nrow = nrow, class = "nbs_df")
}

#' @inherit new_nbs title description params
validate_nbs_df <- function(x) {
  tibble::validate_tibble(x)
}
