#' Get Path(s) to Recent Files Matching a Pattern
#'
#' @description
#' `path_by_pattern()` finds the most recent file in a given directory that
#' matches a regular expression.
#'
#' @param dir The path to the directory of interest
#'
#' @param pattern The regular expression to use when filtering paths
#'
#' @param by Should recency be determined by modification or creation date?
#'
#' @return An `fs_path` vector
#'
#' @aliases path_teams path_nights_weekends
#'
#' @export
path_by_pattern <- function(dir, pattern, by = c("modified", "created")) {

  # Handle `by`
  by <- rlang::arg_match(by)[[1L]]

  if (by == "modified") {
    time <- "modification_time"
  } else if (by == "created") {
    time <- "birth_time"
  }

  # Create/clean `dir` path
  dir <- path_create(dir)

  # Get list of files in `dir` matching `pattern`
  fs::dir_info(
    path = dir,
    regexp = pattern,
    type = "file"
  ) %>%
    dplyr::filter(
      .data[[time]] == max(.data[[time]], na.rm = TRUE)
    ) %>%
    dplyr::pull(.data[["path"]])
}

#' Most Recent Deaths File
#'
#' `path_deaths()` finds the path to the most recent linelist of deaths from
#' NBS or the surveillance team.
#'
#' @param src Which deaths file to look for; `"nbs"` for the NBS file, or
#'   `"surv"` for the surviellance file
#'
#' @param dir The directory to search
#'
#' @return An `fs_path` character vector
path_deaths <- function(
  src = c("nbs", "surv"),
  dir = "V:/EPI DATA ANALYTICS TEAM/MORTALITY DATA/"
) {

  src <- rlang::arg_match(src)[[1L]]

  if (src == "nbs") {
    pattern <- "(?i).*/[^/~]*NBS\\s*Death\\s*Line\\s*List[^/]*[.]xls[x]?$"
  } else {
    pattern <- "(?i).*/[^/~]*Working\\s*Copy\\s*Death\\s*Epi[^/]*[.]xls[x]?$"
  }

  path_by_pattern(dir = path_create(dir), pattern = pattern)
}
