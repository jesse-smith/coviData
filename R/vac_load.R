#' Load Vaccination Data
#'
#' `vac_load()` loads vaccination data from a CSV or Excel file. By default,
#' it loads the most recent data available; this can be controlled by supplying
#' the `date` of the data to load.
#'
#' @inheritParams path_by_date
#'
#' @param path Optional `character`. The path to the file to load; if this is
#'   supplied, `date` and `ext` are ignored.
#'
#' @param ... Additional arguments to pass to \code{\link[vroom:vroom]{vroom()}}
#'   (if loading a csv file) or \code{\link[readxl:read_excel]{read_excel()}}
#'   (if loading an xlsx file)
#'
#' @return A `tibble` containing the loaded data. Unless otherwise specified in
#'   `...`, all variables are read as character (which means that Excel dates
#'   will be in Excel's numeric representation).
#'
#' @export
vac_load <- function(
  date = NULL,
  ext = c("csv", "xlsx"),
  path = character(),
  ...
) {

  # Check `ext` validity
  ext <- rlang::arg_match(ext)[[1L]]

  # Find path to vaccine data, or use given path
  if (rlang::is_empty(path)) {
    ext <- rlang::arg_match(ext)[[1L]]
    path <- path_vac(date, ext = ext)
  } else {
    path <- path_create(path)
    ext <- fs::path_ext(path)
    ext <- rlang::arg_match(ext)[[1L]]
  }

  # Load data
  if (ext == "csv") {
    read_file_delim(file = path, ...)
  } else {
    read_file_excel(file = path, ...)
  }
}
