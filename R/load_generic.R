#' Load a Data File Using the Directory, Date, and Extension
#'
#' \code{load_data} is meant to find and load a delimited or Excel data file
#' given the directory that it's in, the date used in the file name, and the
#' file extension. It is essentially a wrapper around \code{\link{find_file}}
#' and \code{\link{read_file}}.
#'
#' @param date The `Date` to look for in the file name
#'
#' @param directory The directory to search in using \code{\link{find_file}}
#'
#' @param file_name Optionally, the name of the file to load; this is useful
#'   if searching for non-standard files
#'
#' @param ext The extension of the file to load
#'
#' @param pattern A regular expression combining `directory`, `date`, `ext`, and
#'   (possibly) other pieces of the file to load
#'
#' @return A \code{\link[tibble]{tibble}} containing the loaded data
#'
#' @export
load_data <- function(
  date = NULL,
  directory,
  file_name = NULL,
  ext = c("csv", "xlsx"),
  pattern = paste0(".*", date, ".*", ext[[1]])
) {

  # We want to remind the user what date is being used
  if (!is.null(date)) {
    message(paste0("\nDate used: ", date, "\n"))
  }

  # We're ready to find and read the NBS data
  find_file(
    date = date,
    directory = directory,
    pattern = pattern,
    file_name = file_name
  ) %>%
    read_file()
}
