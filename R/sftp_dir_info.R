#' Query File Metadata from SFTP Connection
#'
#' @description
#' `sftp_dir_info()` returns metadata for all files in the directory specified
#' by the `sftp_connection`.
#'
#' Note that the year in the `modification_time` is
#' *inferred* such that the resulting datetime is as close as possible to the
#' current time while remaining in the past or present. This is due to times
#' returned by \code{\link[curl:curl]{curl()}} not including a year.
#'
#' @param sftp_connection A list of connection parameters as created by
#'   \code{\link[coviData:sftp_connect]{sftp_connect()}}
#'
#' @return A `tibble` containing the `path`, `size`, `permissions`,
#'   `modification_time`, and `hard_links` of each file in the directory
#'
#' @export
#'
#' @keywords internal
sftp_dir_info <- function(sftp_connection, regexp = NULL) {

  handle <- curl::new_handle(
    port = sftp_connection$port,
    userpwd = sftp_connection$userpass,
    filetime = 1L
  )

  con <- curl::curl(
    url = sftp_connection$url,
    handle = handle
  )
  on.exit(close(con), add = TRUE)

  cols <- c(
    "permissions",
    "hard_links",
    "user",
    "group",
    "size",
    "month",
    "day",
    "time",
    "path"
  )

  sftp_dir <- path_create(sftp_connection$server, sftp_connection$folder)

  readLines(con) %>%
    tibble::as_tibble_col("files") %>%
    tidyr::separate("files", into = cols, sep = "\\s+", extra = "merge") %>%
    dplyr::mutate(path = path_create(sftp_dir, .data$path)) %>%
    purrr::when(
      is.null(regexp) ~ .,
      ~ dplyr::filter(., stringr::str_detect(.data$path, pattern = regexp))
    ) %>%
    dplyr::transmute(
      .data$path,
      size = fs::as_fs_bytes(.data$size),
      permissions = fs::fs_perms(stringr::str_sub(.data$permissions, 2L, 4L)),
      modification_time = sftp_parse_dttm(.data$month, .data$day, .data$time),
      hard_links = as.integer(.data$hard_links)
    )
}

# Helpers ----------------------------------------------------------------------

sftp_parse_dttm <- function(month, day, time) {
  datetime <- paste0(month, day, time) %>%
    guess_year(orders = "mdHM", tz = "UTC", force = "dttm", train = FALSE) %>%
    lubridate::with_tz("America/Chicago") %>%
    lubridate::force_tz("UTC")
}

guess_year <- function(x, orders, tz = "", ...) {
  dt <- std_dates(x, orders = orders, tz = tz, ...)
  dplyr::if_else(
    dt > lubridate::now(tzone = tz),
    dt - lubridate::years(1L),
    dt
  )
}
