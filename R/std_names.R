#' Parse Investigator Names and Remove (Most) Non-Alphabetical Characters
#'
#' `std_names()` is designed to parse names of individuals into a standard
#' format. It:
#' \enumerate{
#'   \item Replaces square brackets and curly braces with parentheses
#'   \item Removes parenthetic expressions
#'   \item Removes ordinal and numeric expressions
#'   \item Removes symbols (replaces all except apostrophes with a space)
#'   \item Removes extraneous whitespace
#'   \item Converts to the desired case
#' }
#'
#' @param string A character vector containing investigator names
#'
#' @param case Case of output
#'
#' @return The input `string` with cleaned names
#'
#' @export
std_names <- function(string, case = c("upper", "lower", "title", "sentence")) {

  case <- rlang::arg_match(case)[[1L]]

  string %>%
    str_to_ascii() %>%
    str_replace_brackets() %>%
    str_remove_parenthetic() %>%
    str_remove_ordinal() %>%
    str_remove_numeric() %>%
    str_remove_symbols() %>%
    stringr::str_squish() %>%
    purrr::when(
      case == "lower" ~ stringr::str_to_lower(.),
      case == "upper" ~ stringr::str_to_upper(.),
      case == "title" ~ stringr::str_to_title(.),
      case == "sentence" ~ stringr::str_to_sentence(.)
    )
}

#' Convert a String to ASCII Representation
#'
#' `str_to_ascii()` converts a string to ASCII characters only. When
#' `trans = TRUE`, non-ASCII characters are transliterated; otherwise, non-ASCII
#' characters are replaced with the ASCII SUBSTITUTE CHARACTER (`0x1A`).
#'
#' @param string A character vector
#'
#' @param trans Should non-ASCII characters be transliterated?
#'
#' @return An ASCII-encoded character vector
#'
#' @export
str_to_ascii <- function(string, trans = TRUE) {
  if (trans) {
    string %>%
      stringi::stri_trans_general("Any-Latin;Latin-ASCII") %>%
      stringi::stri_enc_toascii()
  } else {
    stringi::stri_enc_toascii(string)
  }
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
      pattern = "\\[|\\{",
      replacement = "("
    ) %>%
    stringr::str_replace_all(
      pattern = "\\]|\\}",
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

#' Remove Symbols from String
#'
#' `str_remove_symbols()` removes all non-alphanumeric characters from a string.
#' It removes apostrophes entirely but replaces other symbols with a space.
#'
#' @param string A character vector
#'
#' @return The input `string`, with symbols removed
#'
#' @keywords internal
#'
#' @export
str_remove_symbols <- function(string) {
  string %>%
    stringr::str_remove_all("[']+") %>%
    stringr::str_replace_all("[^A-Za-z0-9 ]", replacement = " ") %>%
    stringr::str_squish()
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
