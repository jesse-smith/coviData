detect_and_replace <- function(
  .x,
  pattern,
  replacement = "",
  msg = " patterns replaced"
) {
  .x %>%
    stringr::str_detect(pattern = pattern) %>%
    sum(na.rm = TRUE) ->
  n_patterns

  if (n_patterns != 0) {
    .x %>%
      stringr::str_replace_all(
        pattern = pattern,
        replacement = replacement
      ) %T>%
      {message(paste0(n_patterns, msg))}
  } else {
    .x
  }
}

cols_to_keep <- function(data, min_completion_rate = 1/3) {

  factor_data <- dplyr::mutate(
    data,
    dplyr::across(where(not_factor_date), factor)
  )

  factor_data %>%
    dplyr::summarize(dplyr::across(dplyr::everything(), skimr::n_unique)) %>%
    tidyr::pivot_longer(cols = dplyr::everything()) %>%
    dplyr::filter(value < NROW(factor_data)) %>%
    dplyr::select(name) %>%
    .[[1]] ->
    keep_cols_info

  factor_data %>%
    dplyr::summarize(dplyr::across(.fns = skimr::complete_rate)) %>%
    tidyr::pivot_longer(cols = dplyr::everything()) %>%
    dplyr::filter(value >= min_completion_rate) %>%
    dplyr::select(name) %>%
    .[[1]] ->
    keep_cols_missing

  list(info = keep_cols_info, missing = keep_cols_missing)
}

not_factor_date <- function(x) {
  !(is.factor(x) | lubridate::is.POSIXt(x) | lubridate::is.Date(x))
}

#' Assign Date Type to DateTime Variables
#'
#' \code{dttm_to_dt} is an opinionated formatter for dates and datetimes. It
#' prefers simple dates to datetimes, and checks any datetime variables for
#' additional information in the hour:minute:second portion of the variable. If
#' it finds none, it converts the variable to a standard date.
#'
dttm_to_dt <- function(.x) {
  # If .x is already Date type, return as-is
  if (lubridate::is.Date(.x)) return(.x)

  # Otherwise, check for any additional information in the variable
  t <- (
    lubridate::hour(.x) +
      lubridate::minute(.x) / 60 +
      lubridate::second(.x) / 3600
  )

  if (all(t == stats::na.omit(t)[[1]] | is.na(t))) {
    lubridate::as_date(.x)
  } else if (lubridate::is.POSIXlt(.x)) {
    lubridate::as_datetime(.x)
  } else {
    .x
  }
}

#' Coalesce Rows by Columns in Data Frame
#'
#' @references
#'   See Jon Harmon's solution to
#'   \href{https://stackoverflow.com/questions/45515218/combine-rows-in-data-frame-containing-na-to-make-complete-row}{this question}
#'   on Stack Overflow.
coalesce_by_column <- function(x) {
  if (length(x) > 1) {
    dplyr::coalesce(!!!as.list(x))
  } else {
    x
  }
}

#' Coalesce Information in Duplicate Rows
#'
#' \code{coalesce_dupes} sorts data, removes duplicates, and combines
#' information in duplicate rows.
#'
#' \code{coalesce_dupes} can be thought of as an enhanced version of
#' \code{\link[dplyr]{distinct}}. Like \code{distinct}, \code{coalesce_dupes}
#' removes duplicates from a dataset based on a provided set of variables.
#' Unlike distinct, it sorts the data on those variables by default using
#' \code{\link[dplyr]{arrange}}. It also tries to replace missing values with
#' the first non-missing values in any duplicate rows using a modification of
#' \code{\link[dplyr]{coalesce}}.
#'
#' @param data A data frame, data frame extension (e.g. a tibble), or a lazy
#'   data frame (e.g. from dbplyr or dtplyr)
#'
#' @param ... Variables to use for sorting and determining uniqueness. If there
#'   are multiple rows for a given combination of inputs, only the first row
#'   will be preserved. If omitted, simply calls \code{distinct} with
#'   \code{.keep_all = TRUE}.
#'
#' @param sort A logical indicating whether to sort using the input variables.
#'   If no input variables are given, no sorting is performed, and this
#'   parameter is ignored.
#'
#' @export
coalesce_dupes <- function(data, ..., pre_sort = TRUE, post_sort = FALSE) {

  # If no variables are provided, just call distinct() and exit
  if (rlang::dots_n(...) == 0) {
    message(
      paste0(
        "No variables provided; calling dplyr::distinct. ",
        "If your data are large, this may take a while..."
      ),
      appendLF = FALSE
    )
    data %>%
      dplyr::distinct(.keep_all = TRUE) %>%
      return()
    message("Done.")
  }

  # Prepare data by id-ing, arranging, grouping, and counting dupes
  data %>%
    dplyr::mutate(order_id = seq_len(NROW(data))) %>%
    {if (pre_sort) dplyr::arrange(., ...) else .} %>%
    dplyr::group_by(...) %>%
    dplyr::add_count() ->
  grouped_data

  # Handle groups with duplicates
  grouped_data %>%
    dplyr::filter(n > 1) %>%
    dplyr::select(-n) %>%
    dplyr::summarize(
      dplyr::across(.fns = coalesce_by_column),
      .groups = "drop"
    ) ->
  coalesced_dupes

  # Handle groups without duplicates
  grouped_data %>%
    dplyr::filter(n == 1) %>%
    dplyr::select(-n) %>%
    dplyr::ungroup() %>%
    # Add coalesced groups back to data
    tibble::add_row(coalesced_dupes) %>%
    # Either sort by the given variables or by .id, then drop .id
    {if (post_sort) dplyr::arrange(., ...) else dplyr::arrange(., order_id)} %>%
    dplyr::select(-order_id)
}


guess_filetype <- function(path) {
  switch(
    fs::path_ext(path),
    xlsx = "excel",
    xls  = "excel",
    csv  = "delimited",
    prn  = "delimited",
    tsv  = "delimited",
    txt  = "delimited",
    "unknown"
  )
}
