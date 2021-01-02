#' Load Night & Weekend Scheduling Data
#'
#' `asg_load_nights_weekends()` loads schedules for investigators working nights
#' and weekends. These team members do not follow a team-wide schedule and must
#' be dealt with individually.
#'
#' @param path The location of the nights and weekends scheduling Excel workbook
#'
#' @param sheet The sheet from which to read the schedule; either the sheet name
#'   or the sheet number
#'
#' @param clean_names Should names be cleaned?
#'
#' @return The data in the read sheet as a `tibble`
#'
#' @family Case Assignment
#'
#' @export
asg_load_nights_weekends <- function(
  path = path_create(
    "V:/Administration/Schedules/Night&Weekend staff",
    ext = "xlsx"
  ),
  sheet = "schedules",
  clean_names = TRUE
) {

  path <- path_create(path)

  data <- readxl::read_excel(path, sheet = sheet, col_types = "text")

  if (!rlang::is_false(clean_names)) {
    janitor::clean_names(data)
  }
}


#' Parse and Re-Shape Nights and Weekends Scheduling Data
#'
#' `asg_parse_nights_weekends()` parses night/weekend schedules into a standard
#' format for case assignment.
#'
#' @param .data Data read by
#'   \code{\link[covidata:asg_load_nights_weekends]{asg_load_nights_weekends()}}
#'
#' @return A `tibble` with one row per investigator and columns named `member`
#'   and `schedule`; the latter is a list-column containing named logical
#'   vectors of the weekly schedules for each investigator
#'
#' @family Case Assignment
#'
#' @export
asg_parse_nights_weekends <- function(.data) {
  .data %>%
    dplyr::mutate(role = standardize_string(.data[["role"]])) %>%
    dplyr::filter(.data[["role"]] == "Investigator") %>%
    dplyr::select(-c("role", "schedule", "notes")) %>%
    dplyr::mutate(
      member = asg_parse_names(.data[["member"]]),
      dplyr::across(!"member", ~ !is.na(.x))
    ) %>%
    tidyr::pivot_longer(
      !"member",
      names_to = "weekday",
      values_to = "scheduled"
    ) %>%
    dplyr::mutate(weekday = parse_weekday(.data[["weekday"]])) %>%
    dplyr::group_by(.data[["member"]]) %>%
    dplyr::summarize(schedule = list(schedule = set_names(scheduled, weekday)))
}
