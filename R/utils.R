# || By Verb || ################################################################
# | Preprocess #################################################################

cols_to_keep <- function(data, min_completion_rate) {

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

# | Standardize ################################################################

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
      {message(n_patterns, msg)}
  } else {
    .x
  }
}

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

# | Coalesce ###################################################################

#' Coalesce with \strong{tidyselect} Semantics
#'
#' Given a set of data frame columns (potentially using
#' \code{\link[dplyr:dplyr_tidy_select]{<tidy-select>}}), `coalesce_across()`
#' finds the first non-missing value at each position.
#'
#' `coalesce_across()` is a wrapper around
#' \code{\link[dplyr:coalesce]{coalesce()}} that allows specification of columns
#' using \code{<tidy-select>} semantics. It does this using
#' \code{\link[dplyr:across]{across()}}, which means it is only designed to work
#' inside \strong{dplyr} verbs. In other cases, just use `coalesce()`.
#'
#' @inheritParams dplyr::across
#'
#' @inherit dplyr::coalesce return
#'
#' @export
coalesce_across <- function(.cols) {
  .cols <- rlang::enquo(.cols)
  dplyr::coalesce(rlang::splice(dplyr::across(!!.cols)))
}

#' Coalesce Information in Duplicate Rows
#'
#' `coalesce_dupes()` sorts data, removes duplicates, and combines
#' information in duplicate rows.
#'
#' `coalesce_dupes()` can be thought of as an enhanced version of
#' \code{\link[dplyr]{distinct}}. Like `distinct()`, `coalesce_dupes()`
#' removes duplicates from a dataset based on a provided set of variables.
#' Unlike `distinct()`, it sorts the data on those variables (by default) using
#' \code{\link[dplyr]{arrange}}. It also tries to replace missing values with
#' the first non-missing values in any duplicate rows using a modification of
#' \code{\link[dplyr]{coalesce}}.
#'
#' @param data A data frame, data frame extension (e.g. a tibble), or a lazy
#'   data frame (e.g. from dbplyr or dtplyr)
#'
#' @param ... Variables to use for sorting and determining uniqueness. If there
#'   are multiple rows for a given combination of inputs, only the first row
#'   will be preserved. If omitted, simply calls `distinct()` with
#'   `.keep_all = TRUE`.
#'
#' @param pre_sort A logical indicating whether to sort using the input
#'   variables prior to coalescing. If no input variables are given, no sorting
#'   is performed, and this parameter is ignored.
#'
#' @param post_sort A logical indicating whether to sort using the input
#'   variables after coalescing. If no input variables are given, no sorting
#'   is performed, and this parameter is ignored.
#'
#' @export
coalesce_dupes <- function(data, ..., pre_sort = FALSE, post_sort = FALSE) {

  # If no variables are provided, just call distinct() and exit
  if (rlang::dots_n(...) == 0) {
    message(
      paste0(
        "No variables provided; calling `dplyr::distinct()`. ",
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
    purrr::when(rlang::is_true(pre_sort) ~ dplyr::arrange(., ...), ~ .) %>%
    dplyr::group_by(...) %>%
    dplyr::add_count() ->
    grouped_data

  remove(data)

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
    dplyr::add_row(coalesced_dupes) %>%
    # Either sort by the given variables or by .id, then drop .id
    purrr::when(
      rlang::is_true(post_sort) ~ dplyr::arrange(., ...),
      ~ dplyr::arrange(., order_id)
    ) %>%
    dplyr::select(-order_id)
}

#' Coalesce Rows by Columns in Data Frame
#'
#' `coalesce_by_column()` is the workhorse behind `coalesce_dupes()`; when
#' combined with \code{\link[dplyr:group_by]{group_by()}} and
#' \code{\link[dplyr:summarise]{summarize()}}, it allows coalescing of data
#' frame \emph{rows}.
#'
#' @param x A grouped data frame
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

# | Load #######################################################################

#' Guess the Filetype of a Data File
#'
#' `guess_filetype` attempts to guess the encoding of a data file from the
#' file's extension. If the extension is not recognized, it returns
#' `"unknown"`.
#'
#' @param path The path to the data file
#'
#' @return One of `"excel"`, `"delimited"`, `"unknown"`
#'
#' @keywords internal
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

# | Validate ###################################################################
#' Do One or More Columns Actually Exist?
#'
#' `cols_exist()` is a pipeline validation function that checks whether
#' specified columns exist in the input data. It is inspired by
#' `pointblank::col_exists()` but allows tidy selection of columns.
#'
#' @param .data A data frame or data frame extension (e.g. a
#'   \code{\link[tibble:tbl_df-class]{tibble}})
#'
#' @param ... \code{\link[dplyr:dplyr_tidy_select]{<tidy-select>}} One or more
#'   columns from the data, separated by commas. These can be specified as
#'   unquoted names or using tidy selection helpers; if the latter,
#'   `cols_exist()` will check that \emph{at least one} column matches each
#'   provided \code{<tidy-select>} expression.
#'
#' @param action The action to perform if one or more columns do not exist; one
#'   of \code{\link[rlang:abort]{abort()}} (the default),
#'   \code{\link[rlang:abort]{warn()}}, or \code{\link[rlang:abort]{inform()}}.
#'
#' @return The input data frame
#'
#' @export
cols_exist <- function(.data, ..., action = rlang::abort) {

  assertthat::assert_that(
    is.data.frame(.data),
    msg = "`.data` must be a data frame"
  )

  .ptype <- vctrs::vec_ptype(.data)

  rlang::enexprs(...) %>%
    purrr::map(
      ~ dplyr::select(.ptype, !!.x) %>%
        purrr::when(
          NCOL(.) > 0 ~ NULL,
          ~ rlang::expr_label(.x)
        )
    ) %>%
    purrr::flatten_chr() ->
    cols_missing

  if (vctrs::vec_size(cols_missing) > 0) {

    assertthat::assert_that(
      any(
        identical(rlang::abort, action),
        identical(rlang::warn, action),
        identical(rlang::inform, action)
      ),
      msg = paste0(
        "`action` must be one of `rlang::abort()`, `rlang::warn()`, ",
        "or `rlang::inform()`"
      )
    )

    msg <- paste0(
      "\n",
      "No columns were found that match the following selections:\n",
      rlang::format_error_bullets(cols_missing), "\n\n",
      "Did you misspecify a column selection, or were the data not what you ",
      "expected?\n"
    )
    action(message = msg, class = "cols_do_not_exist")

  } else {
    .data
  }
}

# || By Dataset || #############################################################

# | AEL ########################################################################

#' Add Name Columns to AEL Excel File
#'
#' `process_names` creates standardized `PatientLastName`,
#' `PatientFirstName`, `PatientMiddleName`, and
#' `PatientMidOthName` columns in AEL data, if they do not already exist.
#'
#' `process_names` first checks the input data for the above columns, as
#' well as any additional `PatientMidOthName` columns. If any of the
#' standardized columns do not exist, or non-standard \code{Patient...Name}
#' columns exist, the function deletes any previously created
#' \code{Patient...Name} columns and adds standardized ones using
#' \code{\link{split_names}}. If the data contains the standardized columns and
#' no additional `PatientMidOthName` columns, the data is returned as-is.
#'
#' @param data A data frame or tibble containing AEL data from one date
#'
#' @param force A logical; if `force = TRUE`, the data is (re-)processed
#'   regardless of whether it passes the above checks
#'
#' @return A tibble containing the input data plus standardized
#'   `PatientLastName`, `PatientFirstName`, `PatientMiddleName`,
#'   and `PatientMidOthName` columns
process_names <- function(data, force = FALSE) {

  # Get column names in data and set reference columns
  col_names <- colnames(data)
  ref_cols <- c(
    "PatientLastName",
    "PatientFirstName",
    "PatientMiddleName",
    "PatientMidOthName"
  )

  # Check that all standardized columns exist
  ifelse(ref_cols %in% col_names, yes = TRUE, no = FALSE) %>%
    all() ->
    std_exist

  # Check that no more than the standardized columns exist
  stringr::str_detect(col_names, pattern = "Patient.+Name.*") %>%
    sum() %>%
    (function(x) x == 4) ->
    only_std

  # If the data passes both checks, return as-is (unless force == TRUE)
  if (std_exist & only_std & !force) {
    attr(data, which = "modified") <- FALSE
    return(data)
  }

  # Find and delete any Patient...Name columns, then create standardized ones
  # and return the result
  data %>%
    dplyr::select(-dplyr::matches("Patient.+Name.*")) %>%
    split_names() ->
    new_data

  attr(new_data, which = "modified") <- TRUE

  new_data
}


#' Split Patient Names in AEL Data
#'
#' \code{split_names} creates columns for the last, first, first-middle, and
#' other-middle names of patients in AEL data
#'
#' @param .data A data frame or data frame extension (e.g. a
#'   \code{\link[tibble]{tibble}})
#'
#' @return The original data frame, tibble, etc. with character columns for
#'   `PatientLastName`, `PatientFirstName`, `PatientMiddleName`,
#'   `PatientMidOthName` inserted after `PatientName` (which is a
#'   \code{\link[base]{factor}})
split_names <- function(.data) {
  # Create character matrix holding last names and first + other names
  .data$PatientName %>%
    as.character() %>%
    stringr::str_split_fixed(pattern = ",", n = 2) ->
    pt_names

  # Create character matrix holding first, middle, and other middle names
  pt_names[, 2] %>%
    stringr::str_split_fixed(
      pattern = " ",
      n = 3
    ) ->
    pt_other_names

  # Create new columns
  .data %>%
    dplyr::transmute(
      last = pt_names[, 1] %>%
        as.vector() %>%
        stringr::str_squish() %>%
        gsub(pattern = "^$", replacement = NA),
      first = pt_other_names[, 1] %>%
        as.vector() %>%
        stringr::str_squish() %>%
        gsub(pattern = "^$", replacement = NA),
      middle = pt_other_names[, 2] %>%
        as.vector() %>%
        stringr::str_squish() %>%
        gsub(pattern = "^$", replacement = NA),
      other = pt_other_names[, 3] %>%
        as.vector() %>%
        stringr::str_squish() %>%
        gsub(pattern = "^$", replacement = NA)
    ) ->
    new_names

  # Add to existing data
  .data %>%
    dplyr::mutate(
      PatientLastName = new_names$last,
      PatientFirstName = new_names$first,
      PatientMiddleName = new_names$middle,
      PatientMidOthName = new_names$other,
      .after = "PatientName"
    )
}

#' Convert Character Strings to Factors
#'
#' `str_to_factor` converts a character (string) vector to a factor after
#' removing extra whitespace, converting to title case, and (optionally)
#' converting the encoding a specified standard.
#'
#' @param string A character vector to convert
#'
#' @param encoding A logical indicating whether to convert the character
#'  encoding, or a string indicating the encoding to convert to. If
#'  `encoding = TRUE`, then characters are converted to 'UTF-8' encoding.
#'
#' @return A factor
str_to_factor <- function(string, encoding = FALSE) {
  # Perform conversion without re-encoding
  if (encoding == FALSE) {
    string %>%
      stringr::str_to_upper() %>%
      stringr::str_squish() %>%
      factor()
    # Perform conversion with re-encoding
  } else {
    if (encoding == TRUE) encoding <- "UTF-8"
    string %>%
      stringr::str_conv(encoding = encoding) %>%
      stringr::str_to_upper() %>%
      stringr::str_squish() %>%
      factor()
  }
}

#' Filter Dataframe from an AEL File to Memphis Area
#'
#' `filter_region` takes a data frame from \code{\link{load_ael}} & removes
#' observations not in the region specified by `states` and `zips`.
#' The default is to keep Shelby County residents and missing values;
#' `incl_msr` ensures that the entire Memphis Statistical Region is
#' included (as well as any other regions specified). Note that `zips`
#' match to starting values of `PtZipcode`; if a 5-digit code is specified,
#' this is equivalent to matching an entire ZIP code.
#'
#' @param .data A data frame or data frame extension (e.g. a tibble)
#'
#' @param states Two-letter codes for the states to include in the output
#'
#' @param zips 1-to-5 digit codes matching the ZIP codes to include in the
#'   output
#'
#' @param incl_msr A logical indicating whether to ensure all states/ZIPs in
#'   the Memphis Statistical Region are included
#'
#' @param incl_na A logical indicating whether missing values are included or
#'   excluded
#'
#' @return A data frame, tibble, etc. containing observations from the specified
#'   region
filter_region <- function(
  .data,
  states = "TN",
  zips = c("380", "381"),
  incl_msr = FALSE,
  incl_na = TRUE
) {

  # Convert inputs to standardized format
  states <- stringr::str_to_upper(states)
  zips <- as.character(zips)

  # Make sure MSR is included, if desired
  if (incl_msr) {
    # Check states
    if (!("MS" %in% states)) states <- append(states, "MS")
    if (!("AR" %in% states)) states <- append(states, "AR")
    # Check zip codes
    if (!("386" %in% zips)) zips <- append(zips, "386")
    if (!("723" %in% zips)) zips <- append(zips, "723")
  }

  # Make sure NAs are included, if desired
  if (incl_na) {
    if (!any(is.na(states))) states <- append(states, NA)
    .data %>%
      dplyr::filter(
        PtState %in% states &
          (stringr::str_starts(PtZipcode, pattern = zips) | is.na(PtZipcode))
      )
    # Otherwise return matches to states and zips
  } else {
    .data %>%
      dplyr::filter(
        PtState %in% states &
          stringr::str_starts(PtZipcode, pattern = zips)
      )
  }
}

# | NBS ########################################################################
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

check_state <- function(x, states, include_na) {

  # Make sure both are character vectors & upper case
  x <- as.character(x) %>% stringr::str_to_upper()
  states <- as.character(states) %>% stringr::str_to_upper()

  # State codes must be 1 or 2 characters
  # (If given by FIPS code, [01, ..., 09] will be converted to [1, ..., 9] by R)
  x[!(stringr::str_length(x) %in% c(1L, 2L))] <- NA

  # Any improperly formatted entries in comparison vector are removed
  states[!(stringr::str_length(states) %in% c(1L, 2L))] <- NULL

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

# | PCR ########################################################################

relevel_pcr <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all(
      pattern = ".*indeterminate.*",
      replacement = "I"
    ) %>%
    stringr::str_replace_all(
      pattern = "negative",
      replacement = "N"
    ) %>%
    stringr::str_replace_all(
      pattern = ".*presumptive.*",
      replacement = "P"
    ) %>%
    stringr::str_replace_all(
      pattern = ".*positive.*",
      replacement = "C"
    ) %>%
    forcats::as_factor() %T>%
    {message("Levels collapsed to 'U' in relevel_labs:")} %T>%
    {
      levels(.) %>%
        stringr::str_subset(pattern = c("C", "P", "N", "I"), negate = TRUE) %>%
        paste0(collapse = "\t") %>%
        message()
    } %>%
    forcats::fct_expand("S") %>%
    forcats::fct_collapse(
      C = "C",
      P = "P",
      N = "N",
      I = "I",
      S = "S",
      other_level = "U"
    ) %>%
    purrr::when(
      (!"U" %in% levels(.)) ~ forcats::fct_expand(., "U"),
      ~ .
    ) %>%
    forcats::fct_relevel("C", "P", "N", "I", "S", "U")
}

# || By Data Type || ###########################################################

# | Timeseries #################################################################
fill_dates <- function(
  data,
  date_col,
  start = NULL,
  end = NULL,
  fill_na = NA
) {

  # Quote date_col for later evaluation
  date_col <- rlang::enquo(date_col)

  # Get start date if none supplied
  if (is.null(start)) {
    start <- data %>%
      dplyr::select(!!date_col) %>%
      .[[1]] %>%
      lubridate::as_date() %>%
      min(na.rm = TRUE)
  } else {
    start <- lubridate::as_date(start)
  }

  # Get end date if none supplied
  if (is.null(end)) {
    end <- data %>%
      dplyr::select(!!date_col) %>%
      .[[1]] %>%
      lubridate::as_date() %>%
      max(na.rm = TRUE)
  } else {
    end <- lubridate::as_date(end)
  }

  # Join
  dplyr::full_join(
    # Reference dates
    x = dplyr::tibble(join_date = seq(start, end, by = 1)),
    # Input data
    y = dplyr::mutate(data, join_date = !!date_col),
    by = "join_date"
  ) %>%
    dplyr::mutate(!!date_col := dplyr::coalesce(!!date_col, join_date)) %>%
    dplyr::select(-dplyr::matches("join_date"))
}

# | Factor #####################################################################
denoise_factor <- function(
  .f,
  ...,
  dist = 3,
  method = "osa",
  other_level = NULL,
  na_to_other = FALSE
) {
  # Levels to match
  lvls <- rlang::enexprs(...) %>% purrr::map_chr(rlang::as_string)

  # Create and clean character vector from .f
  chr <- standardize_string(.f)

  # Find closest match
  stringdist::amatch(
    chr,
    table = lvls,
    maxDist = dist,
    method = method,
    p = 0.1
  ) %>%
    purrr::map2_chr(chr, .f = ~ ifelse(is.na(.x), yes = .y, no = lvls[.x])) %>%
    factor() ->
    new_f

  new_lvls <- dplyr::intersect(lvls, levels(new_f))

  if (!is.null(other_level)) {
    forcats::fct_other(new_f, keep = new_lvls, other_level = other_level)
  } else {
    new_f
  }
}

# | Files ######################################################################
#' Find a File with the Specified Date + Pattern in the File Name
#'
#' \code{find_file} looks for a file in the folder \code{directory}
#' with the date \code{date} in its name. It returns the path to that file; if
#' more than one file is found, it returns the path to the first one and issues
#' a warning.
#'
#' @param date A `Date` indicating the date to look for in the filename (if any)
#'
#' @param pattern The pattern to match when searching for the filename. The
#'   default is to look for any file with `date` in the name.
#'
#' @param directory The directory in which to search for the file.
#'
#' @param file_name If the search does not return a file known to exist,
#'   `file_name` can be used to specify the file directly
#'
#' @param date_flag A string used to print more informative messages in some
#'   functions
#'
#' @return A string containing the full path to the file that was found
#'
#' @keywords internal
find_file <- function(
  date = Sys.Date(),
  pattern = paste0(".*", date, ".*"),
  directory = NULL,
  file_name = NULL,
  date_flag = NULL,
  rtn_error = TRUE
) {

  # 'file_name' should override 'pattern' so that the file can be found directly
  if (!is.null(file_name)) {
    pattern <- file_name
  }

  file_name <- list.files(directory, pattern = pattern)

  # The next section handles warnings (for multiple matches) and errors (for no
  # matches)

  # check_ael (and possibly other functions) use 'date_flag' to return more
  # informative messages; we display the 'date_flag' in the message if it's
  # given
  if (!is.null(date_flag)) {
    # A warning indicates that multiple matches were found, 'find_file' is
    # returning the first one
    wrn1 <- paste0(
      "'find_file' found multiple files matching ", date_flag, "'s date ",
      "(", date, "):\n\n  ",
      file_name %>%
        stringr::str_flatten(collapse = ";") %>%
        stringr::str_replace_all(pattern = ";", replacement = "\n  "),
      "\n\n  ",
      "By default, the first file is used; to select another file, use the ",
      "'", date_flag, "_file' argument."
    )

    # An error indicates that no matches were found
    stp1 <- paste0(
      "\n  'find_file' did not find a file matching ", date_flag, "'s date ",
      "(", date, ") ",
      "in the specified directory:\n",
      directory
    )
    # If used elsewhere
  } else {
    wrn1 <- paste0(
      "'find_file' found multiple files matching this date ",
      "(", date, "):\n\n  ",
      file_name %>%
        stringr::str_flatten(collapse = ";") %>%
        stringr::str_replace_all(pattern = ";", replacement = "\n  "),
      "\n\n  ",
      "By default, the first file is used; to select another file, use the ",
      "'file_name' argument."
    )

    stp1 <- paste0(
      "'find_file' did not find a file matching this date ",
      "(", date, ") ",
      "in the specified directory:\n",
      directory
    )
  }

  # Check and respond with warning or error
  if (length(file_name) > 1) {
    warning(wrn1)
  } else if (length(file_name) == 0 & rtn_error) {
    stop(stp1)
  } else if (length(file_name) == 0) {
    return(character())
  }

  # Return full path
  paste0(directory, file_name[[1]])
}

#' Test Whether a File is Open
#'
#' `is_open()` tests whether access to a file is currently being blocked by
#' another program.
#'
#' @param path The path to the file in question
#'
#' @return `TRUE` or `FALSE`
#'
#' @keywords internal
is_open <- function(path) {

  # Clean path
  path %<>% fs::path_expand() %>% fs::path_norm() %>% fs::path_tidy()

  # Check that file exists - if not, can't be open, so return FALSE
  if (!fs::file_exists(path)) return(FALSE)

  suppressWarnings(
    try(
      withr::with_connection(list(c = file(path, open = "a")), code = ""),
      silent = TRUE
    ) %>%
      class() %>%
      magrittr::equals("try-error") %>%
      any()
  )
}

#' Create a Tidy Path from Directory + File + Extension
#'
#' @description
#' \lifecycle{soft-deprecated}
#'
#' `create_path()` combines a directory, filename, and file extension into
#' an expanded and tidied path.
#'
#' @details
#' `create_path()` is now soft-deprecated in favor of
#' \code{\link[coviData:path_create]{path_create()}}, which is a re-implementation that
#' follows fs naming conventions, allows both \strong{User}- and \strong{R}-
#' based home directories, and relies on \code{\link[fs:path]{path()}} to
#' combine arbitrary path components given in `...`.
#'
#'
#' @param directory The path to a directory; optionally, the full path can be
#'   supplied here, in which case `create_path()` will simply expand and
#'   tidy the given file path.
#'
#' @param file_name The name of the file of interest; may be left `NULL` if
#'   this is included in `directory`
#'
#' @param ext The file extension to add to the path; may be left `NULL` if
#'   included in `file_name` or `directory`
#'
#' @return An `fs_path`, which is a character vector with additional attributes
#'
#' @export
create_path <- function(directory, file_name = NULL, ext = NULL) {
  directory %>%
    fs::path_expand() %>%
    fs::path_norm() %>%
    fs::path_tidy() %>%
    fs::path_split() %>%
    extract2(1) %>%
    append(file_name) %>%
    fs::path_join() ->
    new_path

  if (!rlang::is_empty(ext)) {
    ext <- stringr::str_remove(ext[[1]], pattern = "[.]")

    new_path %>%
      fs::path_ext_remove() %>%
      fs::path_ext_set(ext = ext) %>%
      fs::path_tidy()
  } else {
    new_path %>% fs::path_tidy()
  }
}

#' Clean a Path with the fs Package
#'
#' `path_clean()` is wrapper around \code{\link[fs:path_tidy]{path_tidy()}},
#' \code{\link[fs:path_expand_r]{path_expand_r()}}
#' (or \code{\link[fs:path_expand]{path_expand()}}), and
#' \code{\link[fs:path_norm]{path_norm()}}. It tidies, then expands, then
#' normalizes the given path(s).
#'
#' @inheritParams fs::path_tidy
#'
#' @param home The home directory definition to use when expanding a path. The
#'   default is to use \strong{R}'s definition, but the system definition of the
#'   user's home directory can also be used. These are equivalent to calling
#'   `path_expand_r()` or `path_expand()`, respectively.
#'
#' @return The cleaned path(s) in an `fs_path` object, which is a character
#'   vector that also has class `fs_path`
path_clean <- function(path, home = c("r", "user")) {

  home %<>%
    stringr::str_to_lower() %>%
    stringr::str_remove_all("[ \n\t\r]")

  home <- rlang::arg_match(home)[[1]]

  if (home == "r") {
    fs_path_expand <- fs::path_expand_r
  } else {
    fs_path_expand <- fs::path_expand
  }

  path %>%
    fs::path_tidy() %>%
    fs_path_expand() %>%
    fs::path_norm()

}

#' Create a Path from Components
#'
#' `path_create()` creates a cleaned path from the components specified in `...`
#' and an optional file extension (`ext`). The path is automatically expanded
#' using either the \strong{R} or system definition of the home directory.
#'
#' @inheritParams fs::path
#'
#' @inheritParams path_clean
#'
#' @return The combined and cleaned path in an `fs_path` object, which is a
#'   character vector that also has class `fs_path`
path_create <- function(..., ext = NULL, home = c("r", "user")) {

  if (rlang::is_null(ext)) {
    ext <- ""
  } else {
    ext %<>%
      stringr::str_remove_all("[ \n\t\r]") %>%
      stringr::str_remove("^[.]")
  }

  fs::path(..., ext = ext) %>% path_clean(home = home)

}

#' Remove Files from Directory Based on Date Created
#'
#' `trim_backups()` deletes old backup files from a directory. More
#' specifically, it identifies files in a `directory` with a common naming
#' `pattern` and it keeps the most recent of those files. To be deleted, a file
#' not be one of the `min_backups` newest files, \emph{and} it must be older
#' than `min_date`. This ensures that at least `min_backups` backups
#' are always retained while keeping any additional backups created on or after
#' `min_date`. The defaults are chosen so that at least 7 backups over at least
#' the past 7 days are kept.
#'
#' @param directory The path to the backup directory
#'
#' @param pattern The naming pattern of the backup files, as a regular
#'   expression. This allows maintenance of backups when other files are also
#'   present in the `directory` (though this is not recommended).
#'
#' @param min_backups The minimum number of backups to keep
#'
#' @param min_date The date after which to keep all backups. The default is set
#'   to keep files for `min_backups` days.
#'
#' @return The deleted paths (invisibly)
#'
#' @keywords internal
#'
#' @export
trim_backups <- function(
  directory,
  pattern = NULL,
  min_backups = 7,
  min_date = Sys.Date() - min_backups + 1
) {

  # Expand and clean directory path
  directory <- create_path(directory)

  # Coerce min_backups to integer
  min_backups <- as.integer(min_backups)

  # Coerce min_date to Date
  min_date <- lubridate::as_date(min_date)

  # Check that min_backups is > 0
  assertthat::assert_that(
    min_backups > 0,
    msg = paste0(
      "Setting `min_backups` < 1 could delete all identified files, ",
      "which is not the purpose of this function. ",
      "Please choose min_backups >= 1."
    )
  )

  # Check that min_date is <= Sys.Date()
  assertthat::validate_that(
    min_date <= Sys.Date(),
    msg = rlang::warn(
      message = paste0(
        "Setting `min_date` greater than today's date does not make sense.\n",
        "The effect is identical to `min_date = Sys.Date()`; ",
        "please consider revising your code for clarity."
      )
    )
  )

  # Get file info
  fs::dir_info(directory, regexp = pattern) %>%
    dplyr::arrange(dplyr::desc(birth_time)) ->
  backups

  # No files will be deleted if n_recent >= the number of returned files, so
  # no point in carrying out the operation.
  if (min_backups >= NROW(backups)) {
    character() %>%
      fs::path_tidy() %>%
      invisible()
  } else {
    backups %>%
      dplyr::filter(
        birth_time < birth_time[[min_backups]],
        lubridate::as_date(birth_time) < min_date
      ) %>%
      dplyr::pull(path) %>%
      fs::file_delete()
  }
}

# | Functions ##################################################################
assert_dots_used <- function(
  env = rlang::caller_fn(),
  fn = NULL,
  action = rlang::abort
) {

  objs <- rlang::env_get_list(
    env = env,
    nms = rlang::env_names(env),
    default = rlang::missing_arg()
  )

  # dots <- rlang::list2()
  #
  # dots_names <- rlang::names2(dots)
  #
  # dots_used <- dots_names %in% rlang::fn_fmls_names(arg_fn)
  #
  # if (any(!dots_used)) {
  #
  #   fn_name <- rlang::call_fn(arg_fn) %>% rlang::expr_label()
  #
  #   msg = paste0(
  #     sum(!dots_used), " components of `...` were not used.",
  #     "\n\n",
  #     "We detected these problematic arguments:",
  #     "\n",
  #     paste0("* `", dots_names[!dots_used], "`", collapse = "\n"),
  #     "\n\n",
  #     "Did you misspecify an argument?"
  #   )
  #
  #   .action(message = msg)
  # }
}

get_fns <- function(fn, string = "x") {

  name_regex <- "([A-Za-z.]+[A-Za-z0-9._]*)"

  fun_regex <- paste0(name_regex, "[(].*[)]")

  inner_regex <- paste0("[(](?!", name_regex, "[(].*[)])[A-Za-z0-9._]*[)]")

  rlang::fn_body(fn) %>%
    rlang::expr_text() %>%
    stringr::str_extract_all(pattern = fun_regex) %>%
    purrr::flatten_chr()
    # stringr::str_remove_all(pattern = "[(].*[)]") %>%
    # unique()
}

check_suggested <- function(..., msg_end = NULL) {
  pkgs <- rlang::exprs(...) %>% purrr::map_chr(~ rlang::expr_name(.x))

  installed <- purrr::map_lgl(pkgs, ~ rlang::is_installed(.x))

  if (all(installed)) return(installed)

  is_are <- if (NROW(pkgs[!installed]) <= 1) "is" else "are"

  not_installed <- paste0(pkgs[!installed],  collapse = "', '")

  error_msg <- paste0(
    "Suggested package(s) '", not_installed, "' ", is_are,
    " required for this function but not installed.",
    " Please install these packages to use this function.",
    if (rlang::is_empty(msg_end)) "" else paste0("\n\n", msg_end)
  )

  rlang::abort(error_msg)

}

#' Check Whether \strong{R} is Running in Windows
#'
#' `is_windows()` checks whether \strong{R} is running on Windows. There are
#' several functions in coviData that will only work on Windows.
#'
#' @keywords internal
is_windows <- function() {
  osVersion %>%
    stringr::str_to_lower() %>%
    stringr::str_detect("windows|microsoft")
}
