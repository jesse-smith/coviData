#' Standardize Variables, Handle Dates, and Remove Bad Columns
#'
#' \code{preprocess} standardizes column names, converts variables to the
#' appropriate data types (including oddly formatted dates/datetimes), and
#' removes empty or constant columns.
#'
#' @param data A dataframe(-like) object
#'
#' @return A processed tibble
#'
#' @export
preprocess <- function(data) {
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
      janitor::remove_constant(na.rm = FALSE) %>%
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
