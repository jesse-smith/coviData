#' Read a Tabular Data File
#'
#' `read_file` reads delimited- and Excel-type data files. The backend for
#' delimited files is \code{\link[data.table]{fread}}; the backend for Excel
#' files is \code{\link[readxl]{read_excel}}.
#'
#' @param path A string indicating the path to the data file; will be expanded
#'   if necessary
#'
#' @param file_type A string indicating the type of file to read; only
#'   "delimited" and "excel" files are supported. The default is "auto", which
#'   determines the file type from the file extension.
#'
#' @param type_convert Should `read_file()` attempt to guess the data type of
#'   the columns after reading?
#'
#'   \emph{Note: The default is `TRUE` for compatibility; want to transition to
#'   `vroom::vroom()` as backend and swap default to `FALSE`.}
#'
#' @param msg A message to be displayed prior to beginning a file read; for use
#'  inside other functions
#'
#' @return A tibble containing the read data
#'
#' @keywords internal
read_file <- function(
  path,
  file_type = c("auto", "delimited", "excel"),
  type_convert = TRUE,
  msg = "Reading file..."
) {

  # Sanitize, tidy, and expand the given path
  path %<>% create_path()

  # Clean up the file_type argument
  file_type <- file_type[[1]] %>%
    stringr::str_squish() %>%
    stringr::str_to_lower()

  # Guess the file type if file_type == "auto"
  if (file_type == "auto") file_type <- guess_filetype(path)

  # Make sure that file_type is supported
  assert(
    file_type %in% c("excel", "delimited"),
    message = paste0(
      "File type is unknown or unsupported.\n",
      "If this is a delimited text file with column separators in [,\t |;:], ",
      "please specify 'file_type = 'delimited''. ",
      "If this is an xls or xlsx file, please specify 'file_type = 'excel''.\n",
      "Other file types are not supported."
    )
  )

  # Use data.table::fread or readxl::read_excel, depending on type
  if (file_type == "delimited") {
    # Display 'msg' to console
    message(msg, appendLF = TRUE)
    # Read, convert to tibble, and attempt to guess column types
    # vroom::vroom(
    #   file = path,
    #   col_types = vroom::cols(.default = vroom::col_character()),
    #   na = c("", "NA", "N/A"),
    #   altrep = TRUE,
    #   progress = TRUE
    # ) %>%
    #   standardize_dates() %>%
    # purrr::when(
    # rlang::is_true(type_convert) ~ . %>%
    #   standardize_dates() %>%
    #   readr::type_convert(),
    #   ~ .
    # ) %T>%
    #   {message("Done.")}

    data.table::fread(
      file = path,
      header = TRUE,
      colClasses = "character",
      blank.lines.skip = TRUE,
      fill = TRUE,
      showProgress = TRUE
    ) %>%
      dplyr::as_tibble() %>%
      purrr::when(
        rlang::is_true(type_convert) ~ eval(.) %>%
          standardize_dates() %>%
          readr::type_convert(),
        ~ .
      ) %T>%
      {message("Done.")}

  } else if (file_type == "excel") {
    # Display 'msg' to console
    message(msg, appendLF = FALSE)
    # Read as tibble and attempt to guess column types (using all columns)
    readxl::read_excel(
      path,
      trim_ws = TRUE,
      guess_max = .Machine$integer.max %/% 100L
    ) %T>%
      {message("Done.")}
  }
}

#' Efficently Read Delimited Files
#'
#' `read_file_delim()` reads delimited files using
#' \code{\link[vroom:vroom]{vroom()}}. This allows the use of ALTREP columns,
#' which don't load data into memory until they are needed.
#'
#' By default, `read_file_delim()` does not attempt to guess column types and
#' reads all columns as character. This can be changed by setting
#' `col_types = vroom::cols(.default = vroom::col_guess())`. If columns are
#' guessed, the default is to use all rows; this can be changed by setting
#' `guess_max` to a different value.
#'
#' This saves a
#' significant amount of time and space when loading data with many rarely used
#' columns.`read_file_delim()` will eventually be paired with
#' `read_file_excel()` to replace the internals of
#' \code{\link[coviData:read_file]{read_file()}}.
#'
#' @inheritParams vroom::vroom
#'
#' @param ... Additional arguments to pass to \code{\link[vroom:vroom]{vroom()}}
#'
#' @return A `tibble` if reading one file; a list of `tibble`s if reading
#'   multiple
#'
#' @export
read_file_delim <- function(
  file,
  col_select = vroom::everything(),
  col_types = vroom::cols(.default = vroom::col_character()),
  na = c("", ".", "NA", "na", "Na", "N/A", "n/a", "N/a",
         "NULL", "null", "Null"),
  guess_max = .Machine$integer.max %/% 100L,
  delim = NULL,
  ...
) {
    vroom::vroom(
      file = path_create(file),
      delim = delim,
      col_types = col_types,
      col_select = col_select,
      na = na,
      guess_max = guess_max,
      ...
    )
}

#' Read Excel Files
#'
#' `read_file_excel()` reads Excel files using
#' \code{\link[readxl:read_excel]{read_excel()}}.
#'
#' By default, `read_file_excel()` does not attempt to guess column types and
#' reads all columns as character. This can be changed by setting
#' `col_types = "guess"`. If columns are
#' guessed, the default is to use all rows; this can be changed by setting
#' `guess_max` to a different value.
#'
#' Note that when reading Excel files as character, dates will be read as the
#' Excel numeric representation in character format
#' (i.e. the date 2020-01-01 will be read as `"43831"`). These dates can be
#' parsed into `Date` format using \code{\link[coviData:std_dates]{std_dates()}}
#' or any of the janitor package's date conversion functions (the most basic
#' being \code{\link[janitor:excel_numeric_to_date]{excel_numeric_to_date()}}).
#'
#' `read_file_excel()` will eventually be paired with
#' `read_file_delim()` to replace the internals of
#' \code{\link[coviData:read_file]{read_file()}}.
#'
#' @inheritParams readxl::read_excel
#'
#' @param file Path to the xls/xlsx file
#'
#' @param ... Additional arguments to pass to
#'   \code{\link[readxl:read_excel]{read_excel()}}
#'
#' @return A `tibble`
#'
#' @export
read_file_excel <- function(
  file,
  range = NULL,
  col_types = "text",
  na = c("", ".", "NA", "na", "Na", "N/A", "n/a", "N/a",
         "NULL", "null", "Null"),
  guess_max = .Machine$integer.max %/% 100L,
  ...
) {
  readxl::read_excel(
    path = path_create(file),
    range = range,
    col_types = col_types,
    na = na,
    guess_max = guess_max,
    ...
  )
}
