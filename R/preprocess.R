#' Standardize Variables, Handle Dates, and Remove Bad Columns
#'
#' \code{preprocess} standardizes column names, converts variables to the
#' appropriate data types (including oddly formatted dates/datetimes), and
#' removes empty or constant columns.
#'
#' @param data A dataframe(-like) object
#'
#' @param na_constant Should \code{NA} values be considered when deciding
#'   whether a column is constant? The default is \code{TRUE}, which will
#'   keep columns that only have 1 unique non-missing value.
#'
#' @return A processed tibble
#'
#' @export
preprocess <- function(data, na_constant = TRUE) {
  suppressMessages(
    data %>%
      # Convert variable names to snake case
      janitor::clean_names(parsing_option = 1) %>%
      # Convert dates to Date or datetime, depending on what info is there
      standardize_dates() %>%
      # Automatically convert the rest
      readr::type_convert() %>%
      # Get rid of empty columns
      janitor::remove_empty(which = "cols") %>%
      # Get rid of constant columns
      janitor::remove_constant(na.rm = !na_constant) %>%
      # Convert characters to UTF-8
      dplyr::mutate(
        dplyr::across(
          where(is.character),
          .fns = stringr::str_conv,
          encoding = "UTF-8"
        )
      ) %>%
      # Make sure output is tibble
      dplyr::as_tibble()
  )
}
