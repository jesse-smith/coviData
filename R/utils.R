# || By Verb || ######################################################################
# | Preprocess #################################################################

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
      {message(paste0(n_patterns, msg))}
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

# | Join #######################################################################
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
    dplyr::add_row(coalesced_dupes) %>%
    # Either sort by the given variables or by .id, then drop .id
    {if (post_sort) dplyr::arrange(., ...) else dplyr::arrange(., order_id)} %>%
    dplyr::select(-order_id)
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

# || By Dataset || #############################################################

# | AEL ########################################################################

#' Add Name Columns to AEL Excel File
#'
#' \code{process_names} creates standardized \code{PatientLastName},
#' \code{PatientFirstName}, \code{PatientMiddleName}, and
#' \code{PatientMidOthName} columns in AEL data, if they do not already exist.
#'
#' \code{process_names} first checks the input data for the above columns, as
#' well as any additional \code{PatientMidOthName} columns. If any of the
#' standardized columns do not exist, or non-standard \code{Patient...Name}
#' columns exist, the function deletes any previously created
#' \code{Patient...Name} columns and adds standardized ones using
#' \code{\link{split_names}}. If the data contains the standardized columns and
#' no additional \code{PatientMidOthName} columns, the data is returned as-is.
#'
#' @param data A data frame or tibble containing AEL data from one date
#'
#' @param force A logical; if \code{force == TRUE}, the data is (re-)processed
#'   regardless of whether it passes the above checks
#'
#' @return A tibble containing the input data plus standardized
#'   \code{PatientLastName}, \code{PatientFirstName}, \code{PatientMiddleName},
#'   and \code{PatientMidOthName} columns
#'
#' @importFrom magrittr `%>%`
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
#'   \code{PatientLastName}, \code{PatientFirstName}, \code{PatientMiddleName},
#'   \code{PatientMidOthName} inserted after \code{PatientName} (which is a
#'   \code{\link[base]{factor}})
split_names <- function(.data) {
  # Create character matrix holding last names and first + other names
  .data$PatientName %>%
    as.character() %>%
    stringr::str_split_fixed(pattern = ",", n = 2) ->
    pt_names

  # Create character matrix holding first, middle, and other middle names
  pt_names[,2] %>%
    stringr::str_split_fixed(
      pattern = " ",
      n = 3
    ) ->
    pt_other_names

  # Create new columns
  .data %>%
    dplyr::transmute(
      last = pt_names[,1] %>%
        as.vector() %>%
        stringr::str_squish() %>%
        gsub(pattern = "^$", replacement = NA),
      first = pt_other_names[,1] %>%
        as.vector() %>%
        stringr::str_squish() %>%
        gsub(pattern = "^$", replacement = NA),
      middle = pt_other_names[,2] %>%
        as.vector() %>%
        stringr::str_squish() %>%
        gsub(pattern = "^$", replacement = NA),
      other = pt_other_names[,3] %>%
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
#' \code{str_to_factor} converts a character (string) vector to a factor after
#' removing extra whitespace, converting to title case, and (optionally)
#' converting the encoding a specified standard.
#'
#' @param string A character vector to convert
#'
#' @param encoding A logical indicating whether to convert the character
#'  encoding, or a string indicating the encoding to convert to. If
#'  \code{encoding == TRUE}, then characters are converted to 'UTF-8' encoding.
#'
#' @return A factor
#'
#' @export
str_to_factor <- function(string, encoding = FALSE) {
  # Perform conversion without re-encoding
  if (encoding == FALSE) {
    string %>%
      stringr::str_to_title() %>%
      stringr::str_squish() %>%
      factor()
    # Perform conversion with re-encoding
  } else {
    if (encoding == TRUE) encoding <- "UTF-8"
    string %>%
      stringr::str_conv(encoding = encoding) %>%
      stringr::str_to_title() %>%
      stringr::str_squish() %>%
      factor()
  }
}

#' Filter Dataframe from an AEL File to Memphis Area
#'
#' \code{filter_region} takes a data frame from \code{link{read_ael}} & removes
#' observations not in the region specified by \code{states} and \code{zips}.
#' The default is to keep Shelby County residents and missing values;
#' \code{incl_msr} ensures that the entire Memphis Statistical Region is
#' included (as well as any other regions specified). Note that \code{zips}
#' match to starting values of \code{PtZipcode}; if a 5-digit code is specified,
#' this is equivalent to matching an entire ZIP code.
#'
#' @param .data A data frame or data frame extention (e.g. a tibble)
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
#'
#' @importFrom magrittr `%>%`
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
        {.[!. %in% c("C", "P", "N", "I")]} %>%
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
    {if (!"U" %in% levels(.)) forcats::fct_expand(., "U") else .} %>%
    forcats::fct_relevel("C", "P", "N", "I", "S", "U")
}

# || By Data Type || ###########################################################

# | Timeseries #################################################################
#' @export
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
