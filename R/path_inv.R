#' Return the Path to a NBS Investigations File
#'
#' `path_inv()` returns paths to one or more NBS investigations files containing
#' the supplied date in their names and ending with the supplied file extension.
#'
#' This will eventually replace \code{\link[coviData:find_file]{find_file()}}
#' for finding NBS investigations files.
#'
#' @param date A `Date` or string in the format "YYYY-MM-DD"
#'
#' @param dir The directory holding the investigations files
#'
#' @param ext The file type to return. NBS investigations files are saved as
#'   both "xlsx" and "csv" files.
#'
#' @return An `fs_path` character vector
#'
#' @export
path_inv <- function(
  date = NULL,
  dir = path_create(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
    "Sandbox data pull Final/"
  ),
  ext = c("csv", "xlsx")
) {

  # Check `ext`
  ext <- rlang::arg_match(ext)[[1L]]

  # Format date - works if `NULL` as well
  date <- date %>% lubridate::as_date() %>% format(format = "%Y-%m-%d")

  # Create file name
  glob <- paste0("*", date, " Final Data Pull.", ext)

  # Get list of files matching date, or all files if `date = NULL`
  files <- dir %>%
    path_create() %>%
    fs::dir_ls(type = "file", glob = glob) %>%
    vec_sort()

  # If `date = NULL`, return the file with the latest date in the file name
  # Otherwise just sort `files` and return
  if (rlang::is_empty(date)) {
    files %>%
      tibble::as_tibble_col("file") %>%
      dplyr::mutate(
        date = .data[["file"]] %>%
          fs::path_file() %>%
          stringr::str_extract(pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}") %>%
          lubridate::as_date()
      ) %>%
      dplyr::filter(.data[["date"]] == max(.data[["date"]], na.rm = TRUE)) %>%
      dplyr::pull(.data[["file"]])
  } else {
    files
  }
}
