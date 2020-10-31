#' Find a File with the Specified Date + Pattern in the File Name
#'
#' \code{find_file} looks for a file in the folder \code{directory}
#' with the date \code{date} in its name. It returns the path to that file; if
#' more than one file is found, it returns the path to the first one and issues
#' a warning.
#'
#' @param date A \code{Date} indicating the date in the filename
#'
#' @param pattern The pattern to match when searching for the filename. The
#'   default is to look for any file with \code{date} in the name.
#'
#' @param directory The directory in which to search for the file.
#'
#' @param file_name If the search does not return a file known to exist,
#'   \code{filename} can be used to specify the file directly
#'
#' @param date_flag A string used to print more informative messages in some
#'   functions
#'
#' @return A string containing the full path to the file that was found
find_file <- function(
  date = Sys.Date(),
  pattern = paste0(".*", date, ".*"),
  directory = NULL,
  file_name = NULL,
  date_flag = NULL,
  rtn_error = TRUE
) {

  # 'file_name' should override 'pattern' so that the file can be found directly
  if (!is.null(file_name)) {
    pattern <- file_name
  }

  file_name <- list.files(directory, pattern = pattern)

  # The next section handles warnings (for multiple matches) and errors (for no
  # matches)

  # check_ael (and possibly other functions) use 'date_flag' to return more
  # informative messages; we display the 'date_flag' in the message if it's
  # given
  if (!is.null(date_flag)) {
    # A warning indicates that multiple matches were found, 'find_file' is
    # returning the first one
    wrn1 <- paste0(
      "'find_file' found multiple files matching ", date_flag, "'s date ",
      "(", date, "):\n\n  ",
      file_name %>%
        stringr::str_flatten(collapse = ";") %>%
        stringr::str_replace_all(pattern = ";", replacement = "\n  "),
      "\n\n  ",
      "By default, the first file is used; to select another file, use the ",
      "'", date_flag, "_file' argument."
    )

    # An error indicates that no matches were found
    stp1 <- paste0(
      "\n  'find_file' did not find a file matching ", date_flag, "'s date ",
      "(", date, ") ",
      "in the specified directory:\n",
      directory
    )
    # If used elsewhere
  } else {
    wrn1 <- paste0(
      "'find_file' found multiple files matching this date ",
      "(", date, "):\n\n  ",
      file_name %>%
        stringr::str_flatten(collapse = ";") %>%
        stringr::str_replace_all(pattern = ";", replacement = "\n  "),
      "\n\n  ",
      "By default, the first file is used; to select another file, use the ",
      "'file_name' argument."
    )

    stp1 <- paste0(
      "'find_file' did not find a file matching this date ",
      "(", date, ") ",
      "in the specified directory:\n",
      directory
    )
  }

  # Check and respond with warning or error
  if (length(file_name) > 1) {
    warning(wrn1)
  } else if (length(file_name) == 0 & rtn_error) {
    stop(stp1)
  } else if (length(file_name) == 0) {
    return(character())
  }

  # Return full path
  paste0(directory, file_name[[1]])
}

#' Read a Tabular Data File
#'
#' \code{read_file} reads delimited- and Excel-type data files. The backend for
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
#' @param msg A message to be displayed prior to beginning a file read; for use
#'  inside other functions
#'
#' @return A tibble containing the read data
read_file <- function(
  path,
  file_type = c("auto", "delimited", "excel"),
  msg = "Reading file..."
) {

  # Sanitize, tidy, and expand the given path
  path %<>% fs::path_tidy() %>% fs::path_expand()

  # Clean up the file_type argument
  file_type %<>% stringr::str_squish() %>% stringr::str_to_lower() %>% .[[1]]

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
    data.table::fread(
      file = path,
      header = TRUE,
      colClasses = "character",
      blank.lines.skip = TRUE,
      fill = TRUE,
      showProgress = TRUE
    ) %>%
      dplyr::as_tibble() %>%
      standardize_dates() %>%
      readr::type_convert() %T>%
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

is_open <- function(path) {

  # Clean path
  path %<>% fs::path_tidy()

  # Check that file exists - if not, can't be open, so return FALSE
  if (!fs::file_exists(path)) return(FALSE)

  suppressWarnings(
    try(
      withr::with_connection(list(c = file(path, open = "a")), code = ""),
      silent = TRUE
    ) %>%
      class() %>%
      {any(. == "try-error")}
  )
}

guess_filetype <- function(path) {
  switch(
    fs::path_ext(path),
    xlsx = "excel",
    xls  = "excel",
    csv  = "delimited",
    prn  = "delimited",
    tsv  = "delimited",
    txt  = "delimited",
    "unknown"
  )
}

#' @export
create_path <- function(directory, file_name, ext = NULL) {
  directory %>%
    fs::path_real() %>%
    fs::path_tidy() %>%
    fs::path_split() %>%
    .[[1]] %>%
    append(file_name) %>%
    fs::path_join() ->
  new_path

  if (!rlang::is_empty(ext)) {
    ext <- stringr::str_remove_all(ext[[1]], pattern = "[.]")

    new_path %>%
      fs::path_ext_remove() %>%
      fs::path_ext_set(ext = ext)
  } else {
    new_path
  }
}
