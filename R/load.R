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
        tidyselect::ends_with("name", ignore.case = TRUE),
        standardize_names
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

#' @export
load_nbs <- function(
  date = Sys.Date(),
  directory = "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/Sandbox data pull Final/",
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
    tibble::as_tibble()
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
  date = Sys.Date(),
  directory = "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/Data for R/",
  category = c("positive_ppl", "negative_ppl", "positive_pcr", "negative_pcr"),
  ext = c("csv", "xlsx")
) {
  message("Loading SAS cleaning results:\n")

  formatted_date <- format(date, format = "%m-%d")

  load_data(
    date = date,
    directory = directory,
    ext = ext[[1]],
    pattern = paste0(".*", category, ".*", formatted_date, ".*", ext[[1]])
  ) %>%
    preprocess()
}
