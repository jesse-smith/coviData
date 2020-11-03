#' @export
load_ael <- function(
  date = Sys.Date(),
  directory = "V:/EPI DATA ANALYTICS TEAM/AEL DATA/",
  ext = "xlsx"
) {
  message("Loading AEL file:\n")
  load_data(
    date = date,
    directory = directory,
    ext = ext[[1]],
    pattern = paste0(".*", date, ".*", ext[[1]])
  )
}

#' Load, Clean, & Merge AEL Data Files for 2 Consecutive dates
#'
#' DEPRECATED - please use \code{load_ael() %>% clean_generic()}.
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
  encoding = TRUE,
  check = TRUE
) {

  # Get path to date1 file, read data, and process name columns
  message(paste0("Reading file for ", date1, "..."))
  load_ael(date1, directory = directory) %>%
    dplyr::mutate(
      dplyr::across(where(lubridate::is.POSIXt), dttm_to_dt)
    ) %>%
    dplyr::mutate(
      dplyr::across(
        where(is.character),
        .fns = stringr::str_conv,
        encoding = "UTF-8"
      )
    ) %>%
    # Restrict to TN and blanks
    dplyr::filter(PtState %in% c("TN", NA)) %>%
    # Split name columns, if not already done
    process_names(force = overwrite_names_1) %>%
    # Perform standard cleaning
    preprocess() %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::ends_with("name", ignore.case = TRUE),
        standardize_string
      )
    ) %>%
    # Add FileDate for sorting
    dplyr::mutate(file_date = date1) ->
    date1_data

  # Get path to date2 file, read data, and process name columns
  message(paste0("Reading file for ", date2, "..."))
  load_ael(date1, directory = directory) %>%
    dplyr::mutate(
      dplyr::across(where(lubridate::is.POSIXt), dttm_to_dt)
    ) %>%
    dplyr::mutate(
      dplyr::across(
        where(is.character),
        .fns = stringr::str_conv,
        encoding = "UTF-8"
      )
    ) %>%
    # Restrict to TN and blanks
    dplyr::filter(PtState %in% c("TN", NA)) %>%
    # Split name columns, if not already done
    process_names(force = overwrite_names_2) %>%
    # Perform standard cleaning
    preprocess() %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::ends_with("name", ignore.case = TRUE),
        standardize_string
      )
    ) %>%
    # Add FileDate for sorting
    dplyr::mutate(file_date = date2) ->
    date2_data

  # Combine files from both dates
  # Not restricting AuthDate - want to be able to handle AEL backlogs. Will
  # still only show us new AEL data upon checking.
  date1_data %>%
    dplyr::add_row(
      date2_data
    ) %>%
    dplyr::arrange(dplyr::desc(file_date)) %>%
    dplyr::distinct(episode_no, .keep_all = TRUE)
}

#' Load Data from the Integrated Data Tool REDcap Project
#'
#' \code{load_integrated_data} loads data from the Integrated Data Tool REDcap
#' project. It is designed to be used in conjuction with
#' \link{download_integrated_data}.
#'
#' @param date A \code{Date} object indicating the date of the data file to load
#'
#' @param directory A string specifying the directory in which to search for
#'   files containing \code{date}
#'
#' @param ext The file type to search for
#'
#' @return A \code{link[tibble]{tibble}} containing the data from the specified
#'   file
#'
#' @export
load_integrated_data <- function(
  date = Sys.Date(),
  directory = paste0(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
    "Integrated data tool Case Interviews/"
  ),
  ext = "csv"
) {
  message("Loading Integrated Data...")
  load_data(
    date = date,
    directory = directory,
    ext = ext[[1]],
    pattern = paste0(".*", date, ".*", ext[[1]])
  )
}

#' @export
load_limited <- function(
  directory = "C:/Users/Jesse.Smith/Documents/Rt_estimates/data/",
  file_name = "status.xlsx"
) {
  load_data(
    directory = directory,
    file_name = file_name
  )
}

#' @export
load_nbs <- function(
  date = Sys.Date(),
  directory = paste0(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
    "Sandbox data pull Final/"
  ),
  ext = c("csv", "xlsx")
) {
  message("Loading NBS file:\n")
  load_data(
    date = date,
    directory = directory,
    ext = ext[[1]],
    pattern = paste0(".*", date, ".*", ext[[1]])
  )
}

load_nbs_deaths_as_html <- function(path) {
  xml2::read_html(path) %>%
    rvest::html_table() %>%
    .[[2]] %>%
    dplyr::as_tibble()
}

#' @export
load_pcr <- function(
  date = Sys.Date(),
  directory = "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/MSR PCR/",
  ext = c("csv", "xlsx")
) {
  message("Loading PCR file:\n")

  formatted_date <- format(date, format = "%m%d%Y")

  load_data(
    date = date,
    directory = directory,
    ext = ext[[1]],
    pattern = paste0(".*", formatted_date, ".*", ext[[1]])
  )
}

#' Load Cleaned Data from SAS Program
#'
#' @param date A \code{Date} indicating the date of the file to read
#'
#' @param directory A string indicating the directory of the file to read
#'
#' @param category A string indicating which cateory of cleaned data to read.
#'   Options are "Positive cases", "Negatives", "PCR_Positives", or
#'   "PCR_Negatives"
#'
#' @param ext The file extension to search for - "csv" by default
#'
#' @export
load_sas <- function(
  directory =
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/Data for R/"
  ,
  dataset = c("positive_ppl", "negative_ppl", "positive_pcr", "negative_pcr"),
  ext = c("csv", "xlsx")
) {
  message("Loading SAS cleaning results:\n")

  file <- create_path(directory, dataset[[1]], ext[[1]])

  date_modified <- as.Date(fs::file_info(file)$modification_time)

  load_data(
    date = date_modified,
    directory = directory,
    ext = ext[[1]],
    file_name = fs::path_file(file)
  ) %>%
    preprocess()
}
