#' Download AEL File from Serv-U SFTP Site
#'
#' @param pattern A regular expression matching the desired filename in the AEL
#'   folder. The default is today's date, but a more specific pattern may be
#'   necessary if multiple files are found.
#'
#' @param usr Username for the Serv-U site. The default is to look for the
#'   system environment variable "sftp_usr", as storing credentials in a script
#'   is not recommended for security purposes.
#'
#' @param pwd Password for the Serv-U site. The default is to look for the
#'   system environment variable "sftp_pwd", as storing credentials in a script
#'   is not recommended for security purposes.
#'
#' @param tofolder Folder/directory to save the file to
#'
#' @return NULL
#'
#' @importFrom magrittr `%>%`

download_ael <- function(
  pattern = Sys.Date(),
  usr = Sys.getenv("sftp_usr"),
  pwd = Sys.getenv("sftp_pwd"),
  tofolder = "V:/EPI DATA ANALYTICS TEAM/AEL Data/"
) {

  # Create SFTP connection details
  sftp_con <- sftp::sftp_connect(
    server = "xfer.shelbycountytn.gov",
    folder = "AEL",
    username = usr,
    password = pwd
  )

  # Get files matching date
  sftp:::sftp_listfiles(sftp_connection = sftp_con) %>%
    dplyr::select(name) %>%
    dplyr::filter(
      stringr::str_detect(name, pattern = as.character(pattern))
    ) %>%
    .[[1]] ->
  filename

  # Check that exactly one matching file was found and take action
  if (length(filename) == 0L) {
    stop("No matching files were found.")
  } else if (length(filename) > 1L) {
    stop(
      paste0(
        "Multiple matching files were found. ",
        "Please specify a unique 'pattern' from the filenames below:\n",
        stringr::str_flatten(filename, collapse = "\n")
      )
    )
  } else {
    sftp::sftp_download(
      file = filename,
      tofolder = tofolder,
      sftp_connection = sftp_con
    )
    # Return NULL
    invisible(NULL)
  }
}

#' Load, Clean, & Merge AEL Data Files for 2 Consecutive Days
#'
#' \code{get_ael} prepares AEL data for analysis. It imports the data,
#' cleans name variables, restricts to our jurisdiction, and merges the files
#' for use with NBS.
#'
#' @importFrom magrittr `%>%`
get_ael <- function(
  day1 = Sys.Date(),
  dir_path = "V:/EPI DATA ANALYTICS TEAM/AEL Data/",
  day2 = day1 - 1,
  day1_file = NULL,
  day2_file = NULL,
  overwrite_names_1 = FALSE,
  overwrite_names_2 = FALSE,
  string_to_factor = FALSE,
  encoding = TRUE,
  check = TRUE
) {

  # Get path to day1 file, read data, and process name columns
  message(paste0("Reading file for ", day1, "..."))

  # File the file for day1
  find_file(
    day = day1,
    dir_path = dir_path,
    pattern = paste0(".*_", day1, "-.*xls.*"),
    file_name = day1_file,
    day_flag = "day1"
  ) %>%
    # Read the file - should handle data types automatically
    coviData::read_ael(
      string_to_factor = string_to_factor,
      encoding = encoding
    ) %>%
    # Restrict to TN and blanks
    dplyr::filter(PtState %in% c("TN", NA)) %>%
    # Split name columns, if not already done
    coviData::process_names(force = overwrite_names_1) %>%
    # Add FileDate for sorting
    dplyr::mutate(FileDate = day1) ->
  day1_data

  # Get path to day2 file, read data, and process name columns
  message(paste0("Reading file for ", day2, "..."))

  # Find the file for day2
  find_file(
    day = day2,
    dir_path = dir_path,
    pattern = paste0(".*_", day2, "-.*xls.*"),
    file_name = day2_file,
    day_flag = "day2"
  ) %>%
    # Read the file - should handle data types automatically
    coviData::read_ael(
      string_to_factor = string_to_factor,
      encoding = encoding
    ) %>%
    # Restrict to TN and blanks
    dplyr::filter(PtState %in% c("TN", NA)) %>%
    # Split name columns, if not already done
    coviData::process_names(force = overwrite_names_2) %>%
    # Add FileDate for sorting
    dplyr::mutate(FileDate = day2) ->
  day2_data

  # Combine files from both days
  # Not restricting AuthDate - want to be able to handle AEL backlogs. Will
  # still only show us new AEL data upon checking.
  day1_data %>%
    tibble::add_row(
      day2_data
    ) %>%
    dplyr::arrange(dplyr::desc(FileDate)) %>%
    dplyr::distinct(EpisodeNo, .keep_all = TRUE)
}

#' @importFrom magrittr `%>%`
check_ael <- function(
  .data = NULL,
  day1 = Sys.Date(),
  day2 = day1 - 1,
  dir_path = "V:/EPI DATA ANALYTICS TEAM/AEL Data/",
  day1_file = NULL,
  day2_file = NULL,
  overwrite_names_1 = FALSE,
  overwrite_names_2 = FALSE,
  encoding = TRUE,
  rtn_table = TRUE
) {

  # If no data is passed, get AEL data
  if (is.null(.data)) {

  # Create shorter day labels (MM/DD)
  day1_str <- as.character(day1, format = "%m/%d")
  day2_str <- as.character(day2, format = "%m/%d")

    # Get path to day1 file, read data, and process name columns
    message(paste0("Reading file for ", day1_str, "..."))
    coviData::find_file(
      day = day1,
      dir_path = dir_path,
      file_name = day1_file,
      day_flag = "day1"
    ) %>%
      coviData::read_ael(encoding = encoding) %>%
      dplyr::filter(PtState %in% c("TN", NA, "")) %>%
      coviData::process_names(force = overwrite_names_1) %>%
      # Add label for the date of the file
      dplyr::mutate(FileDate = day1_str) ->
    day1_data

    # Get path to day1 file, read data, and process name columns
    message(paste0("Reading file for ", day2_str, "..."))
    coviData::find_file(
      day = day2,
      dir_path = dir_path,
      file_name = day2_file,
      day_flag = "day2"
    ) %>%
      coviData::read_ael(encoding = encoding) %>%
      dplyr::filter(PtState %in% c("TN", NA, "")) %>%
      coviData::process_names(force = overwrite_names_2) %>%
      # Add label for the date of the file
      dplyr::mutate(FileDate = day2_str) ->
    day2_data

    larger_day <- if (day1 > day2) day1_str else day2_str
    smaller_day <- if (day1 > day2) day2_str else day1_str

    # Bind and get rid of duplicates to get combined data
    dplyr::bind_rows(day1_data, day2_data) %>%
      dplyr::arrange(FileDate, AuthDate) ->
    .data
  }

  message("Summarizing data...")

  .data %>%
    # Filter to only two days of interest
    dplyr::filter(AuthDate %in% c(day1, day2)) %>%
    # Make factors from dates
    dplyr::mutate(
      FileDate = factor(FileDate, levels = c("New Today", smaller_day, larger_day)),
      AuthDate = AuthDate %>% as.character(format = "%m/%d") %>% factor(levels = c("New Today", smaller_day,larger_day)),
      Result = Result %>%
        forcats::fct_relevel("POSITIVE", "PRESUMPTIVE POSITIVE", "NEGATIVE", "INDETERMINATE") %>% addNA()
    ) ->
  .data

  .data %>%
    # Count number of tests in each category
    dplyr::count(FileDate, AuthDate, Result, name = "Count") %>%
    dplyr::ungroup() %>%
    # Re-arrange
    dplyr::filter(!(FileDate == smaller_day & AuthDate == larger_day)) ->
  summary_all

  # Deduplicated summary
  .data %>%
    dplyr::distinct(EpisodeNo, .keep_all = TRUE) %>%
    # Count number of tests in each category
    dplyr::filter(FileDate == larger_day) %>%
    dplyr::count(FileDate, Result, name = "Count") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(AuthDate = "New Today") ->
  summary_distinct

  summary_all %>%
    tibble::add_row(summary_distinct) %>%
    dplyr::arrange(desc(FileDate), desc(AuthDate), Result) ->
  summary_data

  message("Done.")
  if (rtn_table) {
    summary_data %>%
      gt::gt() %>%
      gt::data_color(
        columns = dplyr::vars(FileDate, AuthDate),
        colors = colorspace::sequential_hcl(
          n = 3,
          l = c(70, 90),
          c1 = 70,
          c2 = 100,
          power = 1,
          rev = TRUE
        )
      ) %>%
      gt::data_color(
        columns = dplyr::vars(Result),
        colors = c("red", "pink", "green", "orange", "grey"),
        alpha = 0.5
      ) %>%
      gt::tab_header(title = "Test Counts by Day and Result") %>%
      methods::show()

    invisible(summary_data)
  } else {
    summary_data
  }
}

#' Add Updated Names Columns to AEL File and Replace Existing File
#'
#' @importFrom magrittr `%>%`
replace_ael <- function(
  day = Sys.Date(),
  dir_path = "V:/EPI DATA ANALYTICS TEAM/AEL Data/",
  day_file = NULL,
  overwrite_names = FALSE,
  encoding = TRUE
) {
  day_str <- as.character(day, format = "%m/%d")

  message(paste0("Reading file for ", day_str, "..."))

  ael_file <- coviData::find_file(
    day = day,
    dir_path = dir_path
  )

  ael_file %>%
    coviData::read_ael(string_to_factor = FALSE, encoding = encoding) %>%
    coviData::process_names(force = overwrite_names) ->
  processed_data

  modified <- attr(processed_data, which = "modified")

  if (modified) {
    message("Writing file for ", day_str, "...")
    openxlsx::write.xlsx(
      processed_data,
      file = ael_file,
      sheet = "Sheet1"
    )
  }

  invisible(processed_data)
}

#' Add Name Columns to AEL Excel File
#'
#' \code{process_names} creates standardized \code{PatientLastName},
#' \code{PatientFirstName}, \code{PatientMiddleName}, and
#' \code{PatientMidOthName} columns in AEL data, if they do not already exist.
#'
#' \code{process_names} first checks the input data for the above columns, as
#' well as any additional \code{PatientMidOthName} columns. If any of the
#' standardized columns do not exist, or non-standard \code{Patient...Name}
#' columns exist, the function deletes any previously created
#' \code{Patient...Name} columns and adds standardized ones using
#' \code{\link{split_names}}. If the data contains the standardized columns and
#' no additional \code{PatientMidOthName} columns, the data is returned as-is.
#'
#' @param data A data frame or tibble containing AEL data from one day
#'
#' @param force A logical; if \code{force == TRUE}, the data is (re-)processed
#'   regardless of whether it passes the above checks
#'
#' @return A tibble containing the input data plus standardized
#'   \code{PatientLastName}, \code{PatientFirstName}, \code{PatientMiddleName},
#'   and \code{PatientMidOthName} columns
#'
#' @importFrom magrittr `%>%`
process_names <- function(data, force = FALSE) {

  # Get column names in data and set reference columns
  col_names <- colnames(data)
  ref_cols <- c(
    "PatientLastName",
    "PatientFirstName",
    "PatientMiddleName",
    "PatientMidOthName"
  )

  # Check that all standardized columns exist
  ifelse(ref_cols %in% col_names, yes = TRUE, no = FALSE) %>%
    all() ->
  std_exist

  # Check that no more than the standardized columns exist
  stringr::str_detect(col_names, pattern = "Patient.+Name.*") %>%
    sum() %>%
    (function(x) x == 4) ->
  only_std

  # If the data passes both checks, return as-is (unless force == TRUE)
  if (std_exist & only_std & !force) {
    attr(data, which = "modified") <- FALSE
    return(data)
  }

  # Find and delete any Patient...Name columns, then create standardized ones
  # and return the result
  data %>%
    dplyr::select(-matches("Patient.+Name.*")) %>%
    coviData::split_names() ->
  new_data

  attr(new_data, which = "modified") <- TRUE

  new_data
}


#' Split Patient Names in AEL Data
#'
#' \code{split_names} creates columns for the last, first, first-middle, and
#' other-middle names of patients in AEL data
#'
#' @param .data A data frame or data frame extension (e.g. a
#'   \code{\link[tibble]{tibble}})
#'
#' @return The original data frame, tibble, etc. with character columns for
#'   \code{PatientLastName}, \code{PatientFirstName}, \code{PatientMiddleName},
#'   \code{PatientMidOthName} inserted after \code{PatientName} (which is a
#'   \code{\link[base]{factor}})
#'
#' @importFrom magrittr `%>%`
split_names <- function(.data) {
  # Create character matrix holding last names and first + other names
  .data$PatientName %>%
    as.character() %>%
    stringr::str_split_fixed(pattern = ",", n = 2) ->
    pt_names

  # Create character matrix holding first, middle, and other middle names
  pt_names[,2] %>%
    stringr::str_split_fixed(
      pattern = " ",
      n = 3
    ) ->
    pt_other_names

  # Create new columns
  .data %>%
    dplyr::transmute(
      last = pt_names[,1] %>%
        as.vector() %>%
        stringr::str_squish() %>%
        gsub(pattern = "^$", replacement = NA),
      first = pt_other_names[,1] %>%
        as.vector() %>%
        stringr::str_squish() %>%
        gsub(pattern = "^$", replacement = NA),
      middle = pt_other_names[,2] %>%
        as.vector() %>%
        stringr::str_squish() %>%
        gsub(pattern = "^$", replacement = NA),
      other = pt_other_names[,3] %>%
        as.vector() %>%
        stringr::str_squish() %>%
        gsub(pattern = "^$", replacement = NA)
    ) ->
  new_names

  # Add to existing data
  .data %>%
    tibble::add_column(
      PatientLastName = new_names$last,
      PatientFirstName = new_names$first,
      PatientMiddleName = new_names$middle,
      PatientMidOthName = new_names$other,
      .after = "PatientName"
    )
}


#' Read AEL Data from Excel File
#'
#' \code{read_xl} reads in an xls(x) file, converts date columns to
#' \code{Date} class, and optionally converts \code{character} columns to
#' \code{factor} columns. \code{read_ael} uses all rows of a file to guess the
#' column type (technically, the first 21,478,836 rows on 64-bit R). It is
#' intended for reading an Excel file containing data sent from AEL to the
#' Shelby County Health Department.
#'
#' @param path The file path to the Excel file
#'
#' @param string_to_factor A logical indicating whether to encode character
#'   columns as factors or leave as character
#'
#' @param encoding A logical indicating whether to convert the character
#'  encoding, or a string indicating the encoding to convert to. If
#'  \code{encoding == TRUE}, then characters are converted to 'UTF-8' encoding.
#'
#' @return A \code{\link[tibble]{tibble}} containing the contents of the Excel
#'   file
#'
#' @importFrom magrittr `%>%`
read_ael <- function(path, string_to_factor = FALSE, encoding = TRUE) {
  # Read Excel file
  readxl::read_excel(
    path,
    trim_ws = TRUE,
    guess_max = .Machine$integer.max %/% 100L,
    progress = TRUE
  ) %>%
    # Convert dates to 'Date' and datetimes to 'POSIXct'
    dplyr::mutate(
    dplyr::across(where(lubridate::is.Date), as.Date)) %>%
    dplyr::mutate(
      dplyr::across(where(lubridate::is.POSIXt), coviData::dttm_to_dt)) ->
  data

  if (string_to_factor) {
    # Convert character columns to factors
    data %>%
      dplyr::mutate(
        dplyr::across(
          where(is.character),
          coviData::str_to_factor,
          encoding = encoding
        )
      ) ->
    data
  } else if (encoding != FALSE) {
    # Convert encoding if string_to_factor == FALSE but encoding != FALSE
    encoding <- if (encoding == TRUE) "UTF-8" else encoding
    data %>%
      dplyr::mutate(
        dplyr::across(
          where(is.character),
          stringr::str_conv,
          encoding = encoding
        )
      ) ->
    data
  }

  # Return results
  tibble::as_tibble(data)
}

#' Convert Character Strings to Factors
#'
#' \code{str_to_factor} converts a character (string) vector to a factor after
#' removing extra whitespace, converting to title case, and (optionally)
#' converting the encoding a specified standard.
#'
#' @param string A character vector to convert
#'
#' @param encoding A logical indicating whether to convert the character
#'  encoding, or a string indicating the encoding to convert to. If
#'  \code{encoding == TRUE}, then characters are converted to 'UTF-8' encoding.
#'
#' @return A factor
#'
#' @importFrom magrittr `%>%`
str_to_factor <- function(string, encoding = FALSE) {
  # Perform conversion without re-encoding
  if (encoding == FALSE) {
    string %>%
      stringr::str_to_upper() %>%
      stringr::str_squish() %>%
      factor()
  # Perform conversion with re-encoding
  } else {
    if (encoding == TRUE) encoding <- "UTF-8"
    string %>%
      stringr::str_conv(encoding = encoding) %>%
      stringr::str_to_upper() %>%
      stringr::str_squish() %>%
      factor()
  }
}

#' Filter Dataframe from an AEL File to Memphis Area
#'
#' \code{filter_region} takes a data frame from \code{link{read_ael}} & removes
#' observations not in the region specified by \code{states} and \code{zips}.
#' The default is to keep Shelby County residents and missing values;
#' \code{incl_msr} ensures that the entire Memphis Statistical Region is
#' included (as well as any other regions specified). Note that \code{zips}
#' match to starting values of \code{PtZipcode}; if a 5-digit code is specified,
#' this is equivalent to matching an entire ZIP code.
#'
#' @param .data A data frame or data frame extention (e.g. a tibble)
#'
#' @param states Two-letter codes for the states to include in the output
#'
#' @param zips 1-to-5 digit codes matching the ZIP codes to include in the
#'   output
#'
#' @param incl_msr A logical indicating whether to ensure all states/ZIPs in
#'   the Memphis Statistical Region are included
#'
#' @param incl_na A logical indicating whether missing values are included or
#'   excluded
#'
#' @return A data frame, tibble, etc. containing observations from the specified
#'   region
#'
#' @importFrom magrittr `%>%`
filter_region <- function(
  .data,
  states = "TN",
  zips = c("380", "381"),
  incl_msr = FALSE,
  incl_na = TRUE
) {

  # Convert inputs to standardized format
  states <- stringr::str_to_upper(states)
  zips <- as.character(zips)

  # Make sure MSR is included, if desired
  if (incl_msr) {
    # Check states
    if (!("MS" %in% states)) states <- append(states, "MS")
    if (!("AR" %in% states)) states <- append(states, "AR")
    # Check zip codes
    if (!("386" %in% zips)) zips <- append(zips, "386")
    if (!("723" %in% zips)) zips <- append(zips, "723")
  }

  # Make sure NAs are included, if desired
  if (incl_na) {
    if (!any(is.na(states))) states <- append(states, NA)
    .data %>%
      dplyr::filter(
        PtState %in% states &
          (stringr::str_starts(PtZipcode, pattern = zips) | is.na(PtZipcode))
      )
  # Otherwise return matches to states and zips
  } else {
    .data %>%
      dplyr::filter(
        PtState %in% states &
          stringr::str_starts(PtZipcode, pattern = zips)
      )
  }
}

#' Assign Date Type to DateTime Variables
#'
#' \code{dttm_to_dt} is an opinionated formatter for dates and datetimes. It
#' prefers simple dates to datetimes, and checks any datetime variables for
#' additional information in the hour:minute:second portion of the variable. If
#' it finds none, it converts the variable to a standard date.
#'
#' @importFrom magrittr `%>%`
dttm_to_dt <- function(.x) {
  # If .x is already Date type, return as-is
  if (lubridate::is.Date(.x)) return(.x)

  # Otherwise, check for any additional information in the variable
  t <- (
    lubridate::hour(.x) +
    lubridate::minute(.x) / 60 +
    lubridate::second(.x) / 3600
  )
  tol <- sqrt(.Machine$double.eps)
  if (all(t == median(t, na.rm = TRUE) | is.na(t))) {
    lubridate::as_date(.x)
  } else if (lubridate::is.POSIXlt(.x)) {
    lubridate::as_datetime(.x)
  } else {

  }
}
