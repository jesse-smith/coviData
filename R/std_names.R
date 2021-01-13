#' Parse Investigator Names and Remove (Most) Non-Alphabetical Characters
#'
#' `std_names()` is designed to parse names of individuals into a standard
#' format. It:
#' \enumerate{
#'   \item Replaces square brackets and curly braces with parentheses
#'   \item Removes parenthetic expressions
#'   \item Removes ordinal and numeric expressions
#'   \item Converts `NA`s to text (for the next function)
#'   \item Passes the intermediate to
#'     \code{\link[janitor:make_clean_names]{
#'       make_clean_names(
#'         case = "title",
#'         use_make_names = FALSE,
#'         transliterations = c("Any-Latin", "Latin-ASCII"),
#'         sep_out = " "
#'       )
#'     }}
#'   \item Removes leading and trailing dashes
#'   \item Removes extraneous whitespace
#'   \item Inverts converted `NA`s (back to `NA_character_`)
#' }
#'
#' @param string A character vector containing investigator names
#'
#' @return The input `string` with cleaned names
#'
#' @export
std_names <- function(string) {
  string %>%
    str_replace_brackets() %>%
    str_replace_braces() %>%
    str_remove_parenthetic() %>%
    str_remove_ordinal() %>%
    str_remove_numeric() %>%
    stringr::str_replace_na() %>%
    janitor::make_clean_names(
      case = "title",
      use_make_names = FALSE,
      transliterations = c("Any-Latin", "Latin-ASCII"),
      sep_out = " "
    ) %>%
    str_trim_dashes() %>%
    stringr::str_squish() %>%
    str_invert_na()
}

#' Replace Square Brackets and Curly Braces with Parentheses
#'
#' `str_replace_brackets()` and `str_replace_braces()` are convenience functions
#' for replacing square brackets and curly braces with parentheses.
#'
#' @param string A character vector
#'
#' @return The `string`, with square brackets or curly braces replaced by
#'   parentheses
#'
#' @aliases str_replace_braces
#'
#' @keywords internal
#'
#' @export
str_replace_brackets <- function(string) {
  string %>%
    stringr::str_replace_all(
      pattern = stringr::coll("["),
      replacement = "("
    ) %>%
    stringr::str_replace_all(
      pattern = stringr::coll("]"),
      replacement = ")"
    )
}

#' @rdname str_replace_brackets
#'
#' @keywords internal
#'
#' @export
str_replace_braces <- function(string) {
  string %>%
    stringr::str_replace_all(
      pattern = stringr::coll("{"),
      replacement = "("
    ) %>%
    stringr::str_replace_all(
      pattern = stringr::coll("}"),
      replacement = ")"
    )
}

#' Remove Text Surrounded by Parentheses
#'
#' `str_remove_parenthetic()` removes parenthetic expressions from strings
#' (i.e. "Hi, Alice (and Bob)!" becomes "Hi, Alice !").
#'
#' @param string A character vector
#'
#' @return The input `string`, with parenthetic expressions removed
#'
#' @keywords internal
#'
#' @export
str_remove_parenthetic <- function(string) {
  stringr::str_remove_all(string, "[(].*[)]")
}

#' Remove Mixed Ordinal Numbers with Text Representation
#'
#' Removes mixed text/numeric ordinal numbers (i.e. "1st", "2nd", "23rd"). This
#' function was inspired by the `replace_ordinal()` function in the
#' textclean package, and re-uses pieces of its code.
#'
#' Currently only implemented for ordinal values 1 through 100.
#'
#' @param string A character vector
#'
#' @param num_paste A logical. If `TRUE`, the elements of larger numbers are
#'   separated with spaces. If `FALSE`, the elements will be joined without
#'   spaces.
#'
#' @param A logical. If `TRUE`, ordinal numbers are removed from `string`
#'
#' @param ... Ignored.
#'
#' @keywords internal
#'
#' @export
str_remove_ordinal <- function(string) {

  ordinals <- c(
    "1st",
    "2nd",
    "3rd",
    paste0(4:19, "th"),
    paste0(20:100, c("th", "st", "nd", "rd", rep("th", 6)))
  )

  ordinals_regex <- paste0(
    "(\\b", ordinals, "\\b)",
    collapse = "|"
  )

  stringr::str_remove_all(string, pattern = ordinals_regex)
}

#' Remove Numeric Symbols from String
#'
#' `str_remove_numeric()` removes numeric symbols from a string. It also removes
#' decimals and commas within numeric strings.
#'
#' @param string A character vector
#'
#' @return The input `string` with numeric symbols removed
#'
#' @keywords internal
#'
#' @export
str_remove_numeric <- function(string) {
  stringr::str_remove_all(string, pattern = "[0-9]([.,][0-9])*")
}

#' Remove Leading and Trailing Dashes from String
#'
#' `str_trim_dashes()` removes leading and trailing dashes from a string. This
#' is most useful to clean up the results of previous operations that may have
#' left such dashes.
#'
#' @param string A character vector
#'
#' @return The input `string`, with leading and trailing dashes removed
#'
#' @keywords internal
#'
#' @export
str_trim_dashes <- function(string) {
  string %>%
    stringr::str_remove_all(pattern = "^-+") %>%
    stringr::str_remove_all(pattern = "-+$")
}

#' Parse Weekdays to Match Full Weekday Names
#'
#' `parse_weekday()` takes a vector of weekday abbreviations and returns the
#' full name of each weekday. If the abbreviation is one letter, it matches the
#' standard one-letter weekday abbreviations (U/M/T/W/R/F/S). If more than one
#' letter, it matches the first two letters to the full day name.
#'
#' @param day A character vector of day names or abbreviations
#'
#' @return A character vector of full day names
#'
#' @keywords internal
#'
#' @export
parse_weekday <- function(day) {

  vctrs::vec_assert(day, ptype = character())

  day <- std_names(day)

  day_len_1 <- stringr::str_length(day) == 1L

  dplyr::case_when(
    day_len_1 & day == "U" ~ "Sunday",
    day_len_1 & day == "M" ~ "Monday",
    day_len_1 & day == "T" ~ "Tuesday",
    day_len_1 & day == "W" ~ "Wednesday",
    day_len_1 & day == "R" ~ "Thursday",
    day_len_1 & day == "F" ~ "Friday",
    day_len_1 & day == "S" ~ "Saturday",
    stringr::str_starts(day, "Su") ~ "Sunday",
    stringr::str_starts(day, "Mo") ~ "Monday",
    stringr::str_starts(day, "Tu") ~ "Tuesday",
    stringr::str_starts(day, "We") ~ "Wednesday",
    stringr::str_starts(day, "Th") ~ "Thursday",
    stringr::str_starts(day, "Fr") ~ "Friday",
    stringr::str_starts(day, "Sa") ~ "Saturday"
  )
}

#' Invert `NA` String Replacement
#'
#' `str_inv_replace_na()` performs the inverse operation of
#' \code{\link[stringr:str_replace_na]{str_replace_na()}}; that is, it converts
#' replaced `NA` values back to `NA_character_`. This only works if `NA` is
#' replaced with `"NA"` or a case-wise variant thereof (i.e. `"Na"`, `"na"`, or
#' `"nA"`). It is designed to invert the addition of "`_[0-9]`" suffixes as
#' well.
#'
#' @param string A character vector
#'
#' @return The input `string` with `NA` replacements converted back to
#'   `NA_character_`
#'
#' @keywords internal
#'
#' @export
str_invert_na <- function(string) {
  stringr::str_replace_all(
    string,
    pattern = "^(N|n)(A|a)(_[0-9]+)?$",
    replacement = NA_character_
  )
}
