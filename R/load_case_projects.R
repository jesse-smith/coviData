#' Load Last Downloaded Data for a REDcap Project
#'
#' `load_nit()` and `load_nca()` load the latest data downloaded from the
#' respective REDcap project. To find the file paths to these data, use
#' \code{\link[coviData:path_by_date]{path_nit()}} or
#' \code{\link[coviData:path_by_date]{path_nca()}}. To download the data, use
#' \code{\link[coviData:download_nit]{download_nit()}} or
#' \code{\link[coviData:download_nca]{download_nca()}}.
#'
#' @param date The download date of the file to read; the default (`NULL`) will
#'   read the most recent file
#'
#' @param path The path to the file to read; if supplied, the function will read
#'   this file instead
#'
#' @param clean_names Should column names be cleaned by
#'   \code{\link[janitor:clean_names]{clean_names()}}?
#'
#' @param ... Additional parameters to pass to
#'   \code{\link[coviData:read_file_delim]{read_file_delim()}}
#'
#' @inherit read_file_delim return
#'
#' @name load-case-projects
#'
#' @aliases load_nit load_nca
NULL

#' @rdname load-case-projects
#'
#' @export
load_nit <- function(
  date = NULL,
  path = path_nit(date),
  clean_names = TRUE,
  ...
) {
  data <- read_file_delim(path, ...)

  if (clean_names) janitor::clean_names(data) else data
}

#' @rdname load-case-projects
#'
#' @export
load_nca <- function(
  date = NULL,
  path = path_nca(date),
  clean_names = TRUE,
  ...
) {
  data <- read_file_delim(path, ...)

  if (clean_names) janitor::clean_names(data) else data
}
