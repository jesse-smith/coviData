#' Check
check_ael <- function(
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

  # Create shorter day labels (MM/DD)
  day1_str <- as.character(day1, format = "%m/%d")
  day2_str <- as.character(day2, format = "%m/%d")

  # Get path to day1 file, read data, and process name columns
  message(paste0("Reading file for ", day1_str, "..."))
  find_file(
    day = day1,
    dir_path = dir_path,
    file_name = day1_file,
    day_flag = "day1"
  ) %>%
    read_ael() %>%
    process_names(force = overwrite_names_1, encoding = encoding) %>%
    # Add label for the date of the file
    mutate(FileDate = day1_str) ->
  day1_data

  # Get path to day1 file, read data, and process name columns
  message(paste0("Reading file for ", day2_str, "..."))
  find_file(
    day = day2,
    dir_path = dir_path,
    file_name = day2_file,
    day_flag = "day2"
  ) %>%
    read_ael() %>%
    process_names(force = overwrite_names_2, encoding = encoding) %>%
    # Add label for the date of the file
    mutate(FileDate = day2_str) ->
  day2_data

  message("Summarizing data...")

  larger_day <- if (day1 > day2) day1_str else day2_str
  smaller_day <- if (day1 > day2) day2_str else day1_str

  # Bind and get rid of duplicates to get combined data
  bind_rows(day1_data, day2_data) %>%
    # Filter to only two days of interest
    filter(AuthDate %in% c(day1, day2)) %>%
    # Make factors from dates
    mutate(
      FileDate = factor(FileDate),
      AuthDate = AuthDate %>% as.character(format = "%m/%d") %>% factor(),
      Result = Result %>% addNA()
    ) %>%
    # Count number of tests in each category
    count(FileDate, AuthDate, Result, name = "Count", .drop = FALSE) %>%
    ungroup() %>%
    arrange(desc(FileDate), desc(AuthDate)) %>%
    filter(!(FileDate == smaller_day & AuthDate == larger_day)) ->
  summary_data

  message("Done.")
  if (rtn_table) {
    summary_data %>%
      janitor::tabyl(FileDate, AuthDate, Result) %>%
      janitor::adorn_totals(where = c("row", "col")) %>%
    gt::gt() %>%
      # gt::data_color(
      #   columns = vars(FileDate, AuthDate),
      #   colors = colorspace::sequential_hcl(
      #     n = summary_data$FileDate %>% unique() %>% length(),
      #     l = c(70, 90),
      #     c1 = 70,
      #     c2 = 100,
      #     power = 1,
      #     rev = TRUE
      #   )
      # ) %>%
      # gt::data_color(
      #   columns = vars(Result),
      #   colors = c("orange", "green", "red", "pink", "grey"),
      #   alpha = 0.5
      # ) %>%
      # gt::tab_header(title = "Test Counts by Day and Result") %>%
      show()

    invisible(summary_data)
  } else {
    summary_data
  }
}

#' Add Updated Names Columns to AEL File and Replace Existing File
#'
replace_ael <- function(
  day = Sys.Date(),
  dir_path = "V:/EPI DATA ANALYTICS TEAM/AEL Data/",
  day_file = NULL,
  overwrite_names = FALSE,
  encoding = TRUE
) {
  day_str <- as.character(day, format = "%m/%d")

  message(paste0("Reading file for ", day_str, "..."))

  ael_file <- find_file(
    day = day,
    dir_path = dir_path
  )

  ael_file %>%
    read_ael(string_to_factor = FALSE, encoding = encoding) %>%
    process_names(force = overwrite_names) ->
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
process_names <- function(data, force = FALSE, encoding = FALSE, rtn_mod_attr = FALSE) {

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

  # Check that only one PatientMidOthName column exists
  str_detect(col_names, pattern = "Patient.+Name.*") %>%
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
    select(-matches("Patient.+Name.*")) %>%
    split_names() ->
  new_data

  attr(new_data, which = "modified") <- TRUE

  new_data
}

#' Find AEL File for a Specified Date
#'
#' \code{file_ael_file} looks for an xls(x) file for the specified date in the
#' specified folder
find_file <- function(
  day = Sys.Date(),
  dir_path = "V:/EPI DATA ANALYTICS TEAM/AEL Data/",
  pattern = paste0("*_", day, "-*"),
  file_name = NULL,
  day_flag = NULL
) {
  # Create pattern to match for the file
  if (!is.null(file_name)) {
    pattern <- file_name
  }

  file_name <- list.files(dir_path, pattern = pattern)

  # Handle multiple or no matches

  # If used in check_ael
  if (!is.null(day_flag)) {
    wrn1 <- paste0(
      "'check_ael' found multiple files matching ", day_flag, "'s date ",
      "(", day, "):\n\n  ",
      file_name %>%
        str_flatten(collapse = ";") %>%
        str_replace_all(pattern = ";", replacement = "\n  "),
      "\n\n  ",
      "By default, the first file is used; to select another file, use the ",
      "'", day_flag, "_file' argument."
    )

    stp1 <- paste0(
      "\n  'check_ael' did not find a file matching ", day_flag, "'s date ",
      "(", day, ") ",
      "in the specified directory:\n",
      dir_path
    )
  # If used elsewhere
  } else {
    wrn1 <- paste0(
      "'check_ael' found multiple files matching this date ",
      "(", day, "):\n\n  ",
      file_name %>%
        str_flatten(collapse = ";") %>%
        str_replace_all(pattern = ";", replacement = "\n  "),
      "\n\n  ",
      "By default, the first file is used; to select another file, use the ",
      "'file_name' argument."
    )

    stp1 <- paste0(
      "'check_ael' did not find a file matching this date ",
      "(", day, ") ",
      "in the specified directory:\n",
      dir_path
    )
  }

  # Check and respond with warning or error
  if (length(file_name) > 1) {
    warning(wrn1)
  } else if (length(file_name) == 0) {
    stop(stp1)
  }

  # Return full path
  paste0(dir_path, file_name[[1]])
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
split_names <- function(.data) {
  # Create character matrix holding last names and first + other names
  .data$PatientName %>%
    as.character() %>%
    str_split_fixed(pattern = ",", n = 2) ->
    pt_names

  # Create character matrix holding first, middle, and other middle names
  pt_names[,2] %>%
    str_split_fixed(
      pattern = " ",
      n = 3
    ) ->
    pt_other_names

  # Create new columns
  .data %>%
    transmute(
      last = pt_names[,1] %>%
        as.vector() %>%
        str_squish() %>%
        gsub(pattern = "^$", replacement = NA),
      first = pt_other_names[,1] %>%
        as.vector() %>%
        str_squish() %>%
        gsub(pattern = "^$", replacement = NA),
      middle = pt_other_names[,2] %>%
        as.vector() %>%
        str_squish() %>%
        gsub(pattern = "^$", replacement = NA),
      other = pt_other_names[,3] %>%
        as.vector() %>%
        str_squish() %>%
        gsub(pattern = "^$", replacement = NA)
    ) ->
  new_names

  # Add to existing data
  .data %>%
    add_column(
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
#' \code{Date} class, and converts \code{character} columns to \code{factor}
#' class. \code{read_ael} uses all columns of a file to guess the column type.
#' It is intended for reading an Excel file containing data sent from AEL to the
#' Shelby County Health Department.
#'
#' @param path The file path to the Excel file
#'
#' @return A \code{\link[tibble]{tibble}} containing the contents of the Excel
#'   file
read_ael <- function(path, string_to_factor = TRUE, encoding = TRUE) {
  # Read Excel file
  readxl::read_excel(
    path,
    trim_ws = TRUE,
    guess_max = .Machine$integer.max %/% 100L,
    progress = FALSE
  ) %>%
    # Convert dates to 'Date' class
    mutate(across(where(lubridate::is.POSIXt), lubridate::as_date)) ->
  data

  if (string_to_factor) {
    # Convert character columns to factors
    data %>%
      mutate(
        across(
          where(is.character),
          str_to_factor,
          encoding = encoding
        )
      )
  } else if (encoding != FALSE) {
    encoding <- if (encoding == TRUE) "UTF-8" else encoding
    data %>%
      mutate(across(where(is.character), str_conv, encoding = encoding))
  } else {
    data
  }
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
str_to_factor <- function(string, encoding = FALSE) {
  # Perform conversion without re-encoding
  if (encoding == FALSE) {
    string %>%
      stringr::str_to_upper() %>%
      stringr::str_squish() %>%
      base::factor()
  # Perform conversion with re-encoding
  } else {
    if (encoding == TRUE) encoding <- "UTF-8"
    string %>%
      stringr::str_conv(encoding = encoding) %>%
      stringr::str_to_upper() %>%
      stringr::str_squish() %>%
      base::factor()
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
filter_region <- function(
  .data,
  states = c("TN"),
  zips = c("380", "381"),
  incl_msr = FALSE,
  incl_na = TRUE
) {

  # Convert inputs to standardized format
  states <- str_to_upper(states)
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
