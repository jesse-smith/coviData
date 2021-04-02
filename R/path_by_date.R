#' Return the Path to a NBS Investigations File
#'
#' `path_*` functions return paths to one or more files of interest
#' containing the supplied date in their names and ending with the supplied
#' file extension. These are convenience wrappers around `path_by_date()`, which
#' returns path that contain a specified date given additional parameters.
#'
#' \itemize{
#'   \item{`path_inv()` returns path(s) to NBS investigations files}
#'   \item{`path_pcr()` returns path(s) to NBS PCR files}
#'   \item{`path_vac()` returns path(s) to TennIIS vaccination files}
#'   \item{`path_nit()` returns path(s) to saved NIT records}
#'   \item{`path_nca()` returns path(s) to saved NCA records}
#' }
#'
#' This will eventually replace \code{\link[coviData:find_file]{find_file()}}
#' for finding specific files.
#'
#' @param date A `Date` or string in the format "YYYY-MM-DD"
#'
#' @param dir The directory holding the files of interest
#'
#' @param ext The file type to return. Files may be saved as "fst", "csv", or
#'   "xlsx".
#'
#' @param date_format The format of the date in the file name; see
#'   \code{\link[base:format]{format()}} for more information
#'
#' @param date_regex A regular expression matching the date in the file name.
#'   This will hopefully be deprecated in the future, but is currently needed
#'   for extracting dates from the file path strings.
#'
#' @param file_regex A regular expression matching the file names of interest.
#'   The location of the date in the file name is specified using `"{date}"`, as
#'   in the glue package.
#'
#' @param type The file type(s) to return, one or more of `"any"`, `"file"`,
#' `"directory"`, `"symlink"`, `"FIFO"`, `"socket"`, `"character_device"` or
#' `"block_device"`.
#'
#' @param force_latest If multiple files with the given `date` are found,
#'   should the function return only the file with the latest creation date
#'   (`TRUE`, the default), or should it return all file paths (`FALSE`)?
#'
#' @return An `fs_path` character vector
#'
#' @aliases path_inv path_pcr path_vac
#'
#' @export
path_by_date <- function(
  dir,
  date_format,
  date_regex,
  date = NULL,
  file_regex = ".*{date}.*",
  type = "any",
  force_latest = TRUE
) {

  # Create/clean `dir` path
  dir <- path_create(dir)

  # Format date - works if `NULL` as well by matching everything (".*")
  date_formatted <- date %>%
    lubridate::as_date() %>%
    format(format = date_format) %>%
    purrr::when(rlang::is_empty(.) ~ ".*", ~ .)

  # Create regex pattern - get rid of duplicate wildcards
  file_regex <- file_regex %>%
    stringr::str_replace(
      pattern = "[{]date[}]",
      replacement = date_formatted
    ) %>%
    stringr::str_replace_all(
      pattern = "([.][*]){2,}",
      replacement = ".*"
    )

  # Get list of files matching date, or all files if `date = NULL`
  files <- dir %>%
    fs::dir_ls(type = type, regexp = file_regex) %>%
    vec_sort()

  # If `date = NULL`, return the file with the latest date in the file name
  # Otherwise just sort `files` and return
  if (date_formatted == ".*") {
    files <- files %>%
      tibble::as_tibble_col("file") %>%
      dplyr::mutate(
        date = .data[["file"]] %>%
          fs::path_file() %>%
          stringr::str_extract(pattern = date_regex) %>%
          lubridate::fast_strptime(format = date_format, lt = FALSE) %>%
          lubridate::as_date()
      ) %>%
      dplyr::filter(.data[["date"]] == max(.data[["date"]], na.rm = TRUE)) %>%
      dplyr::pull(.data[["file"]])
  }

  if (vec_size(files) > 1L && force_latest) {
    files <- files %>%
      fs::file_info() %>%
      dplyr::select("path", "birth_time") %>%
      dplyr::filter(
        .data[["birth_time"]] == max(.data[["birth_time"]], na.rm = TRUE)
      ) %>%
      dplyr::pull(.data[["path"]])
  }

  files
}

#' @rdname path_by_date
#'
#' @export
path_inv <- function(
  date = NULL,
  dir = path_create(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
    "Sandbox data pull Final/"
  ),
  ext = c("csv", "xlsx", "fst")
) {

  # Check `ext`
  ext <- rlang::arg_match(ext)[[1L]]

  path_by_date(
    dir = dir,
    date_format = "%Y-%m-%d",
    date_regex = "[0-9]{4}-[0-9]{2}-[0-9]{2}",
    date = date,
    file_regex = paste0(".*{date} Final Data Pull.*[.]", ext),
    type = "file"
  )
}

#' @rdname path_by_date
#'
#' @export
path_pcr <- function(
  date = NULL,
  dir = "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/MSR PCR/",
  ext = c("csv", "xlsx", "fst")
) {

  # Check `ext`
  ext <- rlang::arg_match(ext)[[1L]]

  path_by_date(
    dir = dir,
    date_format = "%m%d%Y",
    date_regex = "[0-9]{8}",
    date = date,
    file_regex = paste0("MSR - All PCRs_{date}.*[.]", ext),
    type = "file"
  )
}

#' @rdname path_by_date
#'
#' @export
path_vac <- function(
  date = NULL,
  dir = path_create(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
    "COVID-19 Vaccine Reporting/data/COVID-19 vaccine data pull/"
  ),
  ext = c("csv", "xlsx", "fst")
) {

  # Check `ext`
  ext <- rlang::arg_match(ext)[[1L]]

  path_by_date(
    dir = dir,
    date_format = "%Y%m%d",
    date_regex = "[0-9]{8}",
    date = date,
    file_regex = paste0("COVID_VACC_MSR_{date}.*[.]", ext),
    type = "file"
  )
}

#' @rdname path_by_date
#'
#' @export
path_nit <- function(date = NULL, force_latest = TRUE) {
  path_by_date(
    dir ="V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/Data for R/nit/",
    date_format = "%Y-%m-%d",
    date_regex = "[0-9]{1,4}-[0-9]{1,2}-[0-9]{1,2}",
    date = date,
    file_regex = ".*/nit_data_{date}[.]csv$",
    type = "file",
    force_latest = force_latest
  )
}

#' @rdname path_by_date
#'
#' @export
path_nca <- function(date = NULL, force_latest = TRUE) {
  path_by_date(
    dir ="V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/Data for R/nca/",
    date_format = "%Y-%m-%d",
    date_regex = "[0-9]{1,4}-[0-9]{1,2}-[0-9]{1,2}",
    date = date,
    file_regex = ".*/nca_data_{date}[.]csv$",
    type = "file",
    force_latest = force_latest
  )
}
