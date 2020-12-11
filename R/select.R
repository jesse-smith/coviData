#' Select Column Names Using Tidyselect Specifications
#'
#' `select_colnames()` selects the names of columns specified in `...`. It is
#' useful for standardizing a function's interface while providing a link to
#' underlying functions that may take a variety of column specifications.
#'
#' @param .data A data frame or data frame extension (e.g. a `tibble`)
#'
#' @param ... `<tidy-select>` One or more tidyselect specifications for the
#'   desired column names (including simply using those column names)
#'
#' @return A character vector of column names
#'
#' @keywords internal
#'
#' @export
select_colnames <- function(.data, ...) {

  assert_dataframe(.data)

  dplyr::select(.data, ...) %>% colnames()
}
