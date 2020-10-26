#' Load a Data File Using the Directory, Date, and Extension
#'
#' \code{load_data} is meant to find and load a delimited or Excel data file
#' given the directory that it's in, the date of interest, and the file
#' extension.
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
