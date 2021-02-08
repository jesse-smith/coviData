#' Standardize String Formatting
#'
#' \code{standardize_string} ensures that the input string (or character vector)
#' follows a consistent structure. It coerces the input to type
#' \code{character}; converts characters to \code{UTF-8} encoding; replaces
#' non-alphabetical symbols with a space; removes excess whitespace; and
#' converts to the desired case (title, by default). It also informs the user if
#' any characters could not be converted to \code{UTF-8}.
#'
#' This function is primarily intended to \emph{standardize} proper nouns (such
#' as names of persons) into a common format; it doesn't necessarily
#' \emph{correct} the strings. For more general string standardization of this
#' type, see the \href{https://tazinho.github.io/snakecase/}{snakecase} package.
#'
#' @param string The string or character vector to standardize
#'
#' @param case_fn A function for converting to the desired case. Note that
#'   \code{case_fn} is simply the last transformation applied to \code{string}
#'   and may technically perform operations other than case conversion.
#'
#' @param ... Additional parameters to pass to \code{case_fn}
#'
#' @return A character vector of the same length as \code{string}
#'
#' @export
standardize_string <- function(string, case_fn = stringr::str_to_title, ...) {

  esc_msg <- paste0(
    " substitute character(s) have been removed; ",
    "these are characters that could not be converted to UTF-8. ",
    "See warnings for more detail on which characters could not be encoded."
  )

  string %>%
    as.character() %>%
    stringr::str_conv(encoding = "UTF-8") %>%
    detect_and_replace(pattern = "[\ufffd\u001a]", msg = esc_msg) %>%
    stringr::str_remove_all(pattern = "['\"]") %>%
    stringr::str_replace_all(pattern = "[^a-zA-Z ]", replacement = " ") %>%
    stringr::str_squish() %>%
    case_fn(...)
}

standardize_dates <- function(
  .data,
  suffix = c("dt", "dob", "date"),
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

standardize_nbs_id <- function(id) {
  id %>%
    # Coerce to character
    as.character() %>%
    # Remove whitespace
    stringr::str_squish() %>%
    # Remove leading and trailing patters
    stringr::str_remove_all(pattern = "PSN1") %>%
    stringr::str_remove_all(pattern = "PSN2") %>%
    stringr::str_remove_all(pattern = "TN01") %>%
    # Truncate from left if > 7 characters
    stringr::str_trunc(width = 7, side = "left", ellipsis = "") %>%
    # Pad from left if < 7 characters
    stringr::str_pad(width = 7, side = "left", pad = "0")
}
