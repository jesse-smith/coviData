#' Standardize String Formatting
#'
#' \code{standardize_string} ensures that the input string (or character vector)
#' follows a consistent structure. It coerces the input to type
#' \code{character}; converts characters to \code{UTF-8} encoding; replaces
#' non-alphabetical symbols with a space; removes excess whitespace; and
#' converts to the desired case (title, by default). It also informs the user if
#' any characters could not be converted to \code{UTF-8}.
#'
#' This functions is primarily intended to standardize proper nouns (such as
#' names of persons) into a common format; it doesn't necessarily \emph{correct}
#' the strings. For more general string standardization of this type, see
#' the \href{https://tazinho.github.io/snakecase/}{snakecase} package.
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
#' @examples
#'
#' # Convert a messy name
#' standardize_string(" Jesse_smITh!!")
#'
#' # Spacing is respected
#' standardize_string("j esse smith")
#'
#' # Numbers are replaced with a space
#' standardize_string("j3sse sm1th")
#'
#' # So are special characters
#' standardize_string("'Jesse.Smith?'")
standardize_string <- function(string, case_fn = stringr::str_to_title, ...) {

  esc_msg <- paste0(
    " substitute character(s) have been removed; ",
    "these are characters that could not be converted to UTF-8. ",
    "See warnings for more detail on which characters could not be encoded."
  )

  string %>%
    as.character() %>%
    stringr::str_conv(encoding = "UTF-8") %T>%
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

standardize_cities <- function(x) {
  city_dict <- hunspell::dictionary(
    add_words = c(
      "Collierville",
      "Cordova",
      "Germantown",
      "Millington",
      "Lakeland"
    )
  )

  cities <- c(
    "Arlington",
    "Bartlett",
    "Cordova",
    "Collierville",
    "Germantown",
    "Lakeland",
    "Memphis",
    "Millington"
  )

  x %>%
    dplyr::as_tibble() %>%
    dplyr::transmute(city = x) %>%
    mutate(
      city = city %>%
        standardize_string() %>%
        stringr::str_remove_all(pattern = "( [Tt][Nn])|([0-9]{5})") %>%
        stringr::str_replace(pattern = "^Arl$", replacement = "Arlington") %>%
        stringr::str_replace(pattern = "^Bart?$", replacement = "Bartlett") %>%
        stringr::str_replace(
          pattern = "^Coll?$",
          replacement = "Collierville"
        ) %>%
        stringr::str_replace(
          pattern = "^Germ?$",
          replacement = "Germantown"
        ) %>%
        stringr::str_replace(pattern = "^Lake?$", replacement = "Lakeland") %>%
        stringr::str_replace(pattern = "^Mem$", replacement = "Memphis") %>%
        stringr::str_replace(pattern = "^Mill?$", replacement = "Millington")
    ) %>%
    unique() %>%
    tidytext::unnest_tokens(
      output = token,
      input  = city,
      to_lower = FALSE,
      drop = FALSE
    ) %>%
    mutate(
      suggested = token %>%
        hunspell::hunspell_check(dict = city_dict) %>%
        ifelse(
          yes = token,
          no = hunspell::hunspell_suggest(token, dict = city_dict)
        ) %>%
        purrr::map_chr(~ unlist(.x)[[1]]),
      osa_dist = stringdist::stringdist(token, suggested, method = "osa"),
      soundex_dist = stringdist::stringdist(
        token,
        suggested,
        method = "soundex"
      ),
      in_cities = suggested %in% cities,
      suggested_start = stringr::str_trunc(
        suggested,
        width = 1,
        side = "right",
        ellipsis = ""
      ),
      tokens_start = stringr::str_trunc(
        token,
        width = 1,
        side = "right",
        ellipsis = ""
      ),
      replace = in_cities & suggested_start == tokens_start,
      new_token = ifelse(replace, yes = suggested, no = token)
    ) %>%
    dplyr::group_by(city) %>%
    dplyr::summarize(new_city = paste0(new_token, collapse = " ")) %>%
    dplyr::mutate(
      new_city = new_city %>%
        denoise_factor(
          Arlington,
          Bartlett,
          Covington,
          Collierville,
          Germantown,
          Lakeland,
          Memphis,
          Millington,
          dist = 3,
          other_level = "Other/Unincorporated Shelby"
        ) %>%
        fct_other(
          drop = "Covington",
          other_level = "Other/Unincorporated Shelby"
        ) %>%
        addNA() %>%
        fct_recode(c(Missing = NA_character_))
    ) %>%
    ungroup() ->
  lookup

  lookup[[2]][match(x, lookup[[1]])]
}
