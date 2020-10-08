#' Load a Data File Using the Directory, Date, and Extension
#'
#' \code{load_data} is meant to find and load a delimited or Excel data file
#' given the directory that it's in, the date of interest, and the file
#' extension.
#'
#' @param
#'
#' @export
load_data <- function(
  date,
  directory = "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/MSR PCR/",
  file_name = NULL,
  ext = c("csv", "xlsx"),
  pattern = paste0(".*", date, ".*", ext[[1]])
) {

  # We want to remind the user what date is being used
  message(paste0("\nDate used: ", date, "\n"))

  # We're ready to find and read the NBS data
  find_file(
    date = date,
    directory = directory,
    pattern = pattern,
    file_name = file_name
  ) %>%
    read_file()
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

#' @export
load_ael <- function(
  date = Sys.Date(),
  directory = "V:/EPI DATA ANALYTICS TEAM/AEL DATA/",
  ext = c("xlsx", "csv")
) {
  message("Loading AEL file:\n")
  load_data(
    date = date,
    directory = directory,
    ext = ext[[1]],
    pattern = paste0(".*", date, ".*", ext[[1]])
  )
}
