#' Assert that an Object Is a Data Frame
#'
#' `assert_dataframe()` checks whether the input object is a data frame. If so,
#' it returns the object; if not, it throws an error.
#'
#' @param x An object to check
#'
#' @param arg The name of the object; will be set to the name of the input if
#'   left `NULL`. This is useful for creating more informative errors.
#'
#' @return The input, `x`
#'
#' @keywords internal
#'
#' @export
assert_dataframe <- function(x, arg = NULL) {

  if (is.null(arg)) {
    arg <- rlang::ensym(x) %>% rlang::as_label()
  }

  if (is.data.frame(x)) {
    x
  } else {
    rlang::abort(
      paste(arg, "must be a data frame or data frame extension"),
      class = "error_assert_dataframe"
    )
  }
}

#' Assert That a Data Frame Selection Matches the Specified Type and Length
#'
#' `assert_cols()` checks whether columns selected using `...` match the given
#' prototype and/or number of columns. If so, it returns the data frame;
#' otherwise, it throws an error.
#'
#' @param .data A data frame or data frame extension (e.g. a `tibble`)
#'
#' @param ... `<tidy-select>` One or more selection specifications for a set of
#'   columns in the dataframe
#'
#' @param ptype The prototype that the selected columns should match
#'
#' @param n The number of columns that should be selected
#'
#' @return The input object, `.data`
#'
#' @keywords internal
#'
#' @export
assert_cols <- function(.data, ..., ptype = NULL, n = NULL) {

  assert_dataframe(.data)

  data_ptype <- vec_ptype(.data)

  remove(.data)

  selection <- dplyr::select(data_ptype, ...)

  dplyr::mutate(
    selection,
    dplyr::across(.fns = ~ vec_assert(.x, ptype = ptype))
  )

  if (!is.null(n) & length(selection) != n) {
    rlang::abort(
      paste(n, "columns must be selected", class = "assert_cols")
    )
  }
}
