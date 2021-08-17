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
    "Integrated data tool Case Interviews/data/"
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

#' Load Limited Dataset Sent by David
#'
#' @inheritParams load_data
#'
#' @inherit load_data return
#'
#' @noRd
#'
#' @keywords internal
load_limited <- function(
  directory = "C:/Users/allison.plaxco/Documents/Limited Dataset/",
  file_name = "status.xlsx"
) {
  load_data(
    directory = directory,
    file_name = file_name
  )
}

#' Load NBS Data from Disk
#'
#' `load_nbs()` loads NBS data from a file into a \code{\link[tibble]{tibble}}.
#' It is essentially a wrapper around \code{\link{load_data}} with defaults
#' specific to NBS data.
#'
#' @inherit load_data params return
#'
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

#' Load AEL Data from Disk
#'
#' `load_pcr()` loads PCR data from a file into a \code{\link[tibble]{tibble}}.
#' It is essentially a wrapper around \code{\link{load_data}} with defaults
#' specific to PCR data.
#'
#' @inherit load_data params return
#'
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
#' @param directory A string indicating the directory of the file to read
#'
#' @param dataset A string indicating which dataset to read
#'
#' @param ext The file extension to search for - "csv" by default
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
