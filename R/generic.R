#' Find a File with the Specified Date + Pattern in the File Name
#'
#' \code{find_file} looks for a file in the folder \code{dir_path}
#' with the date \code{day} in its name. It returns the path to that file; if
#' more than one file is found, it returns the path to the first one and issues
#' a warning.
#'
#' @param day A \code{Date} indicating the date in the filename
#'
#' @param pattern The pattern to match when searching for the filename. The
#'   default is to look for any file with \code{day} in the name.
#'
#' @param dir_path The directory in which to search for the file.
#'
#' @param file_name If the search does not return a file known to exist,
#'   \code{filename} can be used to specify the file directly
#'
#' @param day_flag A string used to print more informative messages in some
#'   functions
#'
#' @return A string containing the full path to the file that was found
find_file <- function(
  day = Sys.Date(),
  pattern = paste0(".*", day, ".*"),
  dir_path = NULL,
  file_name = NULL,
  day_flag = NULL
) {

  # 'file_name' should override 'pattern' so that the file can be found directly
  if (!is.null(file_name)) {
    pattern <- file_name
  }

  file_name <- list.files(dir_path, pattern = pattern)

  # The next section handles warnings (for multiple matches) and errors (for no
  # matches)

  # check_ael (and possibly other functions) use 'day_flag' to return more
  # informative messages; we display the 'day_flag' in the message if it's given
  if (!is.null(day_flag)) {
    # A warning indicates that multiple matches were found, 'find_file' is
    # returning the first one
    wrn1 <- paste0(
      "'find_file' found multiple files matching ", day_flag, "'s date ",
      "(", day, "):\n\n  ",
      file_name %>%
        stringr::str_flatten(collapse = ";") %>%
        stringr::str_replace_all(pattern = ";", replacement = "\n  "),
      "\n\n  ",
      "By default, the first file is used; to select another file, use the ",
      "'", day_flag, "_file' argument."
    )

    # An error indicates that no matches were found
    stp1 <- paste0(
      "\n  'find_file' did not find a file matching ", day_flag, "'s date ",
      "(", day, ") ",
      "in the specified directory:\n",
      dir_path
    )
    # If used elsewhere
  } else {
    wrn1 <- paste0(
      "'find_file' found multiple files matching this date ",
      "(", day, "):\n\n  ",
      file_name %>%
        stringr::str_flatten(collapse = ";") %>%
        stringr::str_replace_all(pattern = ";", replacement = "\n  "),
      "\n\n  ",
      "By default, the first file is used; to select another file, use the ",
      "'file_name' argument."
    )

    stp1 <- paste0(
      "'find_file' did not find a file matching this date ",
      "(", day, ") ",
      "in the specified directory:\n",
      dir_path
    )
  }

  # Check and respond with warning or error
  if (length(file_name) > 1) {
    warning(wrn1)
  } else if (length(file_name) == 0) {
    stop(stp1)
  }

  # Return full path
  paste0(dir_path, file_name[[1]])
}

#' Standardize Variables, Handle Dates, and Remove Bad Columns
#'
#' \code{clean_generic} standardizes column names, removes empty or constant
#' columns, converts variables to the appropriate data types (including oddly
#' formatted dates/datetimes), removes empty or constant columns, and optionally converts
#' strings to factors.
clean_generic <- function(
  .data,
  date_suffix = c("_dt", "_dob", "_date"),
  date_orders = c("dmy", "mdy", "ymd", "dmyT", "mdyT", "ymdT"),
  string_to_factor = FALSE
) {
  suppressMessages(
    .data %>%
      janitor::clean_names() %>%
      standardize_dates() %>%
      type_convert() %>%
      janitor::remove_empty(which = "cols") %>%
      janitor::remove_constant(na.rm = TRUE) ->
    clean_data
  )
    if (string_to_factor) {
      clean_data %>%
        mutate(across(where(is.character), str_to_factor, encoding = TRUE))
    } else {
      clean_data %>%
        mutate(
          across(where(is.character), stringr::str_conv(encoding = "UTF-8"))
        )
    }
}

standardize_dates <- function(
  .data,
  suffix = c("_dt", "_dob", "_date"),
  orders = c("dmy", "mdy", "ymd", "dmyT", "mdyT", "ymdT"),
  ...
) {
  .data %>%
    dplyr::select(tidyselect::ends_with(suffix)) ->
    date_data

  date_cols <- colnames(date_data)

  date_data %>%
    readr::type_convert() %>%
    dplyr::mutate(
      dplyr::across(
        where(is.character),
        lubridate::parse_date_time,
        orders = orders,
        ...
      )
    ) ->
    .data[date_cols]

  .data
}

cols_to_keep <- function(.data, min_completion_rate = 0.5) {

  .factor_data <- dplyr::mutate(
    .data,
    dplyr::across(where(not_factor_date), factor)
  )

  .factor_data %>%
    dplyr::summarize(dplyr::across(everything(), skimr::n_unique)) %>%
    pivot_longer(cols = everything()) %>%
    filter(value < NROW(.factor_data)) %>%
    select(name) %>%
    .[[1]] ->
    keep_cols_info

  .factor_data %>%
    summarize(across(.fns = skimr::complete_rate)) %>%
    pivot_longer(cols = everything()) %>%
    filter(value >= min_completion_rate) %>%
    select(name) %>%
    .[[1]] ->
    keep_cols_missing

  list(info = keep_cols_info, missing = keep_cols_missing)
}

not_factor_date <- function(x) {
  !(is.factor(x) | lubridate::is.POSIXt(x))
}

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
      filter(
        get(state_col) %>%
          check_state(states = states, include_na = include_na)
      ) ->
      .data
  }

  # Filter by county if column exists
  if (county_exists) {
    .data %>%
      filter(
        get(county_col) %>%
          check_county(counties = counties, include_na = include_na)
      ) ->
    .data
  }

  # Filter by ZIP if not NULL
  if (zip_exists) {
    .data %>%
      filter(
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
