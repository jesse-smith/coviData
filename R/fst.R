#' Save Commonly Used Files in `fst` Format
#'
#' @description
#' Each `fst_*` function is designed to easily convert a specific file, though
#' they will technically work with any tabular data file.
#'
#' `fst_inv()` converts an investigations file
#'
#' `fst_pcr()` converts a PCR file
#'
#' `fst_vac()` converts a vaccination file
#'
#' @details
#' For general conversion to `fst` format, see
#' \code{\link[coviData:fst_convert]{fst_convert()}}.
#'
#' @inheritParams path_inv
#'
#' @inheritParams fst_convert
#'
#' @return The dataset being converted, invisibly
#'
#' @name fst_convert-common
#'
#' @aliases fst_inv fst_pcr fst_vac
NULL

#' @rdname fst_convert-common
#'
#' @export
fst_inv <- function(
  date = NULL,
  dir = NULL,
  ext = c("csv", "xlsx"),
  save_as = NULL,
  ...
) {

  if (rlang::is_empty(dir)) {
    path <- path_inv(date, ext = ext)
  } else {
    path <- path_inv(date, dir = dir, ext = ext)
  }

  fst_convert(path, save_as = save_as, ...)
}

#' @rdname fst_convert-common
#'
#' @export
fst_pcr <- function(date = NULL, save_as = NULL, ...) {

  if (rlang::is_empty(dir)) {
    path <- path_pcr(date, ext = ext)
  } else {
    path <- path_pcr(date, dir = dir, ext = ext)
  }

  fst_convert(path, save_as = save_as, ...)
}

#' @rdname fst_convert-common
#'
#' @export
fst_vac <- function(path = path_vac(ext = "csv"), save_as = NULL, ...) {

  if (rlang::is_empty(dir)) {
    path <- path_vac(date, ext = ext)
  } else {
    path <- path_vac(date, dir = dir, ext = ext)
  }

  fst_convert(path, save_as = save_as, ...)
}

#' Convert Tabular Data Files to `fst` Format
#'
#' `fst_convert()` reads Excel, delimited, and fixed-width data file and
#' converts them to `fst` format.
#'
#' @param path Character. File path to read from. it will be passed to
#'   \code{\link[coviData:read_file_excel]{read_file_excel()}} if the file
#'   extension is "xlsx" or "xls", otherwise it will be passed to
#'   \code{\link[coviData:read_file_delim]{read_file_delim()}}
#'
#' @param save_as Character. The path to save the result `fst` file. This must
#'   have an `fst` extension; if it does not, the current extension (if any)
#'   will be removed and replaced with `fst`. If left `NULL`, the `path`
#'   argument is used (with extension replaced by `fst`).
#'
#' @param ... Additional arguments to pass to `read_file_delim()` or
#'   `read_file_excel()`. The `col_types` argument is particularly useful to
#'   control the type in which the data is saved (default is all `character`).
#'
#' @return The read `data`, invisibly
fst_convert <- function(path, save_as = NULL, ...) {

  path <- path_create(path)
  ext  <- fs::path_ext(path) %>% stringr::str_to_lower()
  save_as <- if (rlang::is_empty(save_as)) path else path_create(save_as)
  save_as <- fs::path_ext_set(save_as, ext = "fst")

  if (ext %in% c("xlsx", "xls")) {
    data <- read_file_excel(path, ...)
  } else {
    data <- read_file_delim(path, ...)
  }

  fst::write_fst(data, path = save_as, compress = 100L)
}
