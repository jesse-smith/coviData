#' Download AEL File from Serv-U SFTP Site
#'
#' @param pattern A regular expression matching the desired filename in the AEL
#'   folder. The default is todate's date, but a more specific pattern may be
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
#'
#' @export

download_ael <- function(
  pattern = Sys.Date(),
  usr = Sys.getenv("sftp_usr"),
  pwd = Sys.getenv("sftp_pwd"),
  tofolder = "V:/EPI DATA ANALYTICS TEAM/AEL Data/",
  repair_name_cols = FALSE
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

    if (repair_name_cols & lubridate::is.Date(pattern)) {
      message("Standardizing name columns...")
      replace_ael(date = pattern, directory = tofolder, date_file = date_file)
      message("Done!")
    } else if (repair_name_cols) {
      wrn <- paste0(
        "Name columns were not repaired b/c 'pattern' must be a date to fix ",
        "names automatically. ",
        "Please call 'replace_ael' separately with the date of interest."
      )
      warning(wrn)
    }

    # Return NULL
    invisible(NULL)
  }
}

#' Load, Clean, & Merge AEL Data Files for 2 Consecutive dates
#'
#' \code{get_ael} prepares AEL data for analysis. It imports the data,
#' cleans name variables, restricts to our jurisdiction, and merges the files
#' for use with NBS.
#'
#' @importFrom magrittr `%>%`
#'
#' @export
load_and_process_ael <- function(
  date1 = Sys.Date(),
  directory = "V:/EPI DATA ANALYTICS TEAM/AEL Data/",
  date2 = date1 - 1,
  date1_file = NULL,
  date2_file = NULL,
  overwrite_names_1 = FALSE,
  overwrite_names_2 = FALSE,
  string_to_factor = TRUE,
  encoding = TRUE,
  check = TRUE
) {

  # Get path to date1 file, read data, and process name columns
  message(paste0("Reading file for ", date1, "..."))

  # File the file for date1
  find_file(
    date = date1,
    directory = directory,
    pattern = paste0(".*_", date1, "-.*xls.*"),
    file_name = date1_file,
    date_flag = "date1"
  ) %>%
    # Read the file - should handle data types automatically
    read_ael(
      string_to_factor = string_to_factor,
      encoding = encoding
    ) %>%
    # Restrict to TN and blanks
    dplyr::filter(PtState %in% c("TN", NA)) %>%
    # Split name columns, if not already done
    process_names(force = overwrite_names_1) %>%
    # Perform standard cleaning
    clean_generic(string_to_factor = string_to_factor) %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::ends_with("name", ignore.case = TRUE),
        standardize_names
      )
    ) %>%
    # Add FileDate for sorting
    dplyr::mutate(file_date = date1) ->
  date1_data

  # Get path to date2 file, read data, and process name columns
  message(paste0("Reading file for ", date2, "..."))

  # Find the file for date2
  find_file(
    date = date2,
    directory = directory,
    pattern = paste0(".*_", date2, "-.*xls.*"),
    file_name = date2_file,
    date_flag = "date2"
  ) %>%
    # Read the file - should handle data types automatically
    read_ael(
      string_to_factor = string_to_factor,
      encoding = encoding
    ) %>%
    # Restrict to TN and blanks
    dplyr::filter(PtState %in% c("TN", NA)) %>%
    # Split name columns, if not already done
    process_names(force = overwrite_names_2) %>%
    # Perform standard cleaning
    clean_generic(string_to_factor = string_to_factor) %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::ends_with("name", ignore.case = TRUE),
        standardize_names
      )
    ) %>%
    # Add FileDate for sorting
    dplyr::mutate(file_date = date2) ->
  date2_data

  # Combine files from both dates
  # Not restricting AuthDate - want to be able to handle AEL backlogs. Will
  # still only show us new AEL data upon checking.
  date1_data %>%
    tibble::add_row(
      date2_data
    ) %>%
    dplyr::arrange(dplyr::desc(file_date)) %>%
    dplyr::distinct(episode_no, .keep_all = TRUE)
}

#' @importFrom magrittr `%>%`
#'
#' @export
check_ael <- function(
  .data = NULL,
  date1 = Sys.Date(),
  date2 = date1 - 1,
  directory = "V:/EPI DATA ANALYTICS TEAM/AEL Data/",
  date1_file = NULL,
  date2_file = NULL,
  overwrite_names_1 = FALSE,
  overwrite_names_2 = FALSE,
  encoding = TRUE,
  rtn_table = TRUE
) {

  # If no data is passed, get AEL data
  if (is.null(.data)) {

  # Create shorter date labels (MM/DD)
  date1_str <- as.character(date1, format = "%m/%d")
  date2_str <- as.character(date2, format = "%m/%d")

    # Get path to date1 file, read data, and process name columns
    message(paste0("Reading file for ", date1_str, "..."))
    find_file(
      date = date1,
      directory = directory,
      file_name = date1_file,
      date_flag = "date1"
    ) %>%
      read_ael(encoding = encoding) %>%
      dplyr::filter(PtState %in% c("TN", NA, "")) %>%
      process_names(force = overwrite_names_1) %>%
      # Add label for the date of the file
      dplyr::mutate(FileDate = date1_str) ->
    date1_data

    # Get path to date1 file, read data, and process name columns
    message(paste0("Reading file for ", date2_str, "..."))
    find_file(
      date = date2,
      directory = directory,
      file_name = date2_file,
      date_flag = "date2"
    ) %>%
      read_ael(encoding = encoding) %>%
      dplyr::filter(PtState %in% c("TN", NA, "")) %>%
      process_names(force = overwrite_names_2) %>%
      # Add label for the date of the file
      dplyr::mutate(FileDate = date2_str) ->
    date2_data

    larger_date <- if (date1 > date2) date1_str else date2_str
    smaller_date <- if (date1 > date2) date2_str else date1_str

    # Bind to get combined data
    dplyr::bind_rows(date1_data, date2_data) %>%
      dplyr::arrange(FileDate, AuthDate) ->
    .data
  }

  message("Summarizing data...")

  .data %>%
    # Filter to only two dates of interest
    dplyr::filter(AuthDate %in% c(date1, date2)) %>%
    # Make factors from dates
    dplyr::mutate(
      FileDate = factor(
        FileDate,
        levels = c("New Todate", smaller_date, larger_date)),
      AuthDate = AuthDate %>%
        as.character(format = "%m/%d") %>%
        factor(levels = c("New Todate", smaller_date,larger_date)),
      Result = Result %>%
        as.character() %>%
        stringr::str_to_title() %>%
        factor(
          levels = c(
            "Positive",
            "Presumptive Positive",
            "Negative",
            "Indeterminate"
          )
        ) %>%
        addNA()
    ) ->
  .data

  .data %>%
    # Count number of tests in each category
    dplyr::count(FileDate, AuthDate, Result, name = "Count") %>%
    dplyr::ungroup() %>%
    # Re-arrange
    dplyr::filter(!(FileDate == smaller_date & AuthDate == larger_date)) ->
  summary_all

  # Deduplicated summary
  .data %>%
    dplyr::arrange(FileDate, AuthDate, Result, EpisodeNo) %>%
    dplyr::distinct(EpisodeNo, .keep_all = TRUE) %>%
    # Count number of tests in each category
    dplyr::filter(FileDate == larger_date) %>%
    dplyr::count(FileDate, Result, name = "Count") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(AuthDate = "New Todate") ->
  summary_distinct

  summary_all %>%
    tibble::add_row(summary_distinct) %>%
    dplyr::arrange(dplyr::desc(FileDate), dplyr::desc(AuthDate), Result) ->
  summary_data

  message("Done.")

  # Look at number in yesterdate's file that aren't in todate's
  suppressWarnings(
    .data %>%
      dplyr::arrange(desc(FileDate), AuthDate, Result, EpisodeNo) %>%
      dplyr::distinct(EpisodeNo, .keep_all = TRUE) %>%
      # Count number of tests in each category
      dplyr::filter(FileDate == smaller_date) %>%
      janitor::tabyl(Result) %>%
      janitor::adorn_totals() %>%
      janitor::adorn_title(col_name = "Gone Todate") %>%
      show()
  )

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
      gt::tab_header(title = "Test Counts by date and Result") %>%
      methods::show()

    invisible(summary_data)
  } else {
    summary_data
  }
}

#' Add Updated Names Columns to AEL File and Replace Existing File
#'
#' @importFrom magrittr `%>%`
#'
#' @export
replace_ael <- function(
  date = Sys.Date(),
  directory = "V:/EPI DATA ANALYTICS TEAM/AEL Data/",
  date_file = NULL,
  overwrite_names = FALSE,
  encoding = TRUE
) {
  date_str <- as.character(date, format = "%m/%d")

  message(paste0("Reading file for ", date_str, "..."))

  ael_file <- find_file(
    date = date,
    directory = directory
  )

  ael_file %>%
    read_ael(string_to_factor = FALSE, encoding = encoding) %>%
    process_names(force = overwrite_names) ->
  processed_data

  modified <- attr(processed_data, which = "modified")

  if (modified) {
    message("Writing file for ", date_str, "...")
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
#' @param data A data frame or tibble containing AEL data from one date
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
    dplyr::select(-dplyr::matches("Patient.+Name.*")) %>%
    split_names() ->
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
      dplyr::across(where(lubridate::is.POSIXt), dttm_to_dt)
    ) ->
  data

  if (string_to_factor) {
    # Convert character columns to factors
    data %>%
      dplyr::mutate(
        dplyr::across(
          where(is.character),
          str_to_factor,
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
