#' Standardize Variables, Handle Dates, and Remove Bad Columns
#'
#' \code{clean_generic} standardizes column names, removes empty or constant
#' columns, converts variables to the appropriate data types (including oddly
#' formatted dates/datetimes), removes empty or constant columns, and optionally converts
#' strings to factors.
#'
#' @importFrom magrittr `%>%`
clean_generic <- function(
  .data,
  string_to_factor = FALSE
) {
  suppressMessages(
    .data %>%
      janitor::clean_names(parsing_option = 1) %>%
      standardize_dates() %>%
      readr::type_convert() %>%
      janitor::remove_empty(which = "cols") %>%
      janitor::remove_constant(na.rm = TRUE) ->
    clean_data
  )
    if (string_to_factor) {
      clean_data %>%
        dplyr::mutate(dplyr::across(where(is.character), str_to_factor, encoding = TRUE))
    } else {
      clean_data %>%
        dplyr::mutate(
          dplyr::across(where(is.character), stringr::str_conv, encoding = "UTF-8")
        )
    }
}

#' @importFrom magrittr `%>%`
standardize_names <- function(.x) {

  esc_msg <- paste0(
    " substitute character(s) have been removed.",
    " See warnings for more detail on which characters could not be encoded."
  )

  .x %>%
    stringr::str_conv(encoding = "UTF-8") %T>%
    detect_and_replace(pattern = "[\ufffd\u001a]", msg = esc_msg) %>%
    stringr::str_remove_all(pattern = "['\"]") %>%
    stringr::str_replace_all(pattern = "[^a-zA-Z0-9 ]", replacement = " ") %>%
    stringr::str_squish() %>%
    stringr::str_to_title()
}

#' @importFrom magrittr `%>%`
#'
#' @importFrom magrittr `%T>%`
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

#' @importFrom magrittr `%>%`
standardize_dates <- function(
  .data,
  suffix = c("_dt", "_dob", "_date"),
  orders = c("dmy", "mdy", "ymd", "dmyT", "mdyT", "ymdT"),
  ...
) {
  .data %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::ends_with(suffix, ignore.case = TRUE),
        .fns = ~ .x %>%
          as.character() %>%
          lubridate::parse_date_time(orders = orders) %>%
          dttm_to_dt()
      )
    )
}

#' @importFrom magrittr `%>%`
cols_to_keep <- function(.data, min_completion_rate = 0.5) {

  .factor_data <- dplyr::mutate(
    .data,
    dplyr::across(where(not_factor_date), factor)
  )

  .factor_data %>%
    dplyr::summarize(dplyr::across(dplyr::everything(), skimr::n_unique)) %>%
    tidyr::pivot_longer(cols = dplyr::everything()) %>%
    dplyr::filter(value < NROW(.factor_data)) %>%
    dplyr::select(name) %>%
    .[[1]] ->
    keep_cols_info

  .factor_data %>%
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

#' @importFrom magrittr `%>%`
filter_geo <- function(
  .data,
  state_col = "patient_state",
  county_col = "alt_county",
  zip_col = "patient_zip",
  states = c("TN", "47"),
  zips = c("380..", "381.."),
  counties = "Shelby County",
  include_na = TRUE
) {

  state_exists <- any(state_col %in% colnames(.data))
  county_exists <- any(county_col %in% colnames(.data))
  zip_exists <- any(zip_col %in% colnames(.data))

  # Filter by state if column exists
  if (state_exists) {
    .data %>%
      dplyr::filter(
        get(state_col) %>%
          check_state(states = states, include_na = include_na)
      ) ->
      .data
  }

  # Filter by county if column exists
  if (county_exists) {
    .data %>%
      dplyr::filter(
        get(county_col) %>%
          check_county(counties = counties, include_na = include_na)
      ) ->
    .data
  }

  # Filter by ZIP if not NULL
  if (zip_exists) {
    .data %>%
      dplyr::filter(
        get(zip_col) %>%
          check_zip(zips = zips, include_na = include_na)
      ) ->
    .data
  }

  if (all(!c(state_exists, county_exists, zip_exists))) {
    warning("Returning input data; no columns were selected for filtering.")
  }

  .data
}

#' @importFrom magrittr `%>%`
check_state <- function(x, states, include_na) {

  # Make sure both are character vectors & upper case
  x <- as.character(x) %>% stringr::str_to_upper()
  states <- as.character(states) %>% stringr::str_to_upper()

  # State codes must be 1 or 2 characters
  # (If given by FIPS code, [01, ..., 09] will be converted to [1, ..., 9] by R)
  x[!(stringr::str_length(x) %in% c(1L,2L))] <- NA

  # Any improperly formatted entries in comparison vector are removed
  states[!(stringr::str_length(states) %in% c(1L,2L))] <- NULL

  # Handle NAs separately; shouldn't be in `states`
  states[is.na(states)] <- NULL

  # Initialize logical vector
  x_lgl <- vector(length = length(x))

  # Loop through `states` to compare with `x`
  # Inefficient, but vectorizing would take much of my time
  for (state in states) {
    x_lgl <- x_lgl + stringr::str_detect(x, pattern = state)
  }

  # Add NA if `include_na` == TRUE
  if (include_na) {
    x_lgl[is.na(x_lgl)] <- TRUE
  }

  # Return logical vector
  as.logical(x_lgl)
}

#' @importFrom magrittr `%>%`
check_county <- function(x, counties, include_na) {
  # Make sure both are character vectors & title case
  x <- as.character(x) %>% stringr::str_to_title()
  counties <- as.character(counties) %>% stringr::str_to_title()

  # Handle NAs separately; shouldn't be in `counties`
  counties[is.na(counties)] <- NULL

  # Initialize logical vector
  x_lgl <- vector(length = length(x))

  # Loop through `counties` to compare with `x`
  # Inefficient, but vectorizing would take much of my time
  for (county in counties) {
    x_lgl <- x_lgl + stringr::str_detect(x, pattern = county)
  }

  # Add NA if `include_na` == TRUE
  if (include_na) {
    x_lgl[is.na(x_lgl)] <- TRUE
  }

  # Return logical vector
  as.logical(x_lgl)
}

#' @importFrom magrittr `%>%`
check_zip <- function(x, zips, include_na) {
  # Make sure both are character vectors
  x <- as.character(x)
  zips <- as.character(zips)

  # ZIP codes must be 5 digits
  x[stringr::str_length(x) != 5] <- NA

  # Any improperly formatted entries in comparison vector are removed
  zips[stringr::str_length(zips) != 5] <- NULL

  # Handle NAs separately; shouldn't be in `zips`
  zips[is.na(zips)] <- NULL

  # Initialize logical vector
  x_lgl <- vector(length = length(x))

  # Loop through `zips` to compare with `x`
  # Inefficient, but vectorizing would take too long
  for (zipcode in zips) {
    x_lgl <- x_lgl + stringr::str_detect(x, pattern = zip)
  }

  # Add NA if `include_na` == TRUE
  if (include_na) {
    x_lgl[is.na(x_lgl)] <- TRUE
  }

  # Return logical vector
  as.logical(x_lgl)
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

  if (all(t == median(t, na.rm = TRUE) | is.na(t))) {
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
#' @param .data A data frame, data frame extension (e.g. a tibble), or a lazy
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
