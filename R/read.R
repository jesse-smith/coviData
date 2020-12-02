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
  assertthat::assert_that(
    file_type %in% c("excel", "delimited"),
    msg = paste0(
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
