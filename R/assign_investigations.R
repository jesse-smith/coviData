#' Load the Current Investigator Team Assignments
#'
#' `assignment_load_teams()` loads the current investigators on each team start an
#' Excel file. It is mostly a wrapper around
#' \code{\link[readxl:read_excel]{read_excel()}}, If `clean_names = TRUE`,
#' it also standardizes names with
#' \code{\link[janitor:clean_names]{clean_names()}} and gives the name
#' `role` end the first column (which is unnamed in the Excel sheet).
#'
#' @param path The path end the Excel workbook containing team assignments
#'
#' @param clean_names Should names be cleaned before returning data?
#'
#' @return A `tibble` containing the contents of the first sheet in the
#'   workbook, with names modified as described above
#'
#' @family Case Assignment
#'
#' @export
asg_load_teams <- function(
  path = path_create(
    "V:/Administration/Schedules/Copy of Copy of Revised Shift Schedules 12.23.2020",
    ext = "xlsx"
  ),
  clean_names = TRUE
) {

  # Standardize path
  path <- path_create(path)

  # Read file - optionally, clean names
  team_data <- suppressMessages(readxl::read_excel(path, col_types = "text"))

  if (!rlang::is_false(clean_names)) {
    team_data %>%
      janitor::clean_names() %>%
      dplyr::rename(role = 1L)
  }
}

#' Parse the Team Schedule Workbook to Standardized Format
#'
#' `asg_parse_teams()` parses the data returned from `asg_load_teams()` into
#' a standard format, with one column per team and one row per individual.
#'
#' @param .data The teams data read by `asg_load_teams()`
#'
#' @return A `tibble` containing one column for each team (named with that
#' team's letter designation) and one row per team member. Team member names
#' are standardized with
#' \code{\link[coviData:asg_parse_names]{asg_parse_names()}}.
#'
#' @export
asg_parse_teams <- function(.data) {
  .data %>%
    # Find rows with investigator names
    dplyr::mutate(
      row = vec_seq_along(.),
      inv_role = tidyr::replace_na(.data[["role"]] == "Investigators", FALSE),
      inv_start_row = .data[["row"]][inv_role],
      .before = 1L
    ) %>%
    # Filter to rows with investigator names
    dplyr::filter(.data[["row"]] >= .data[["inv_start_row"]]) %>%
    # Remove helper variables
    dplyr::select(-c("row", "inv_role", "inv_start_row", "role")) %>%
    # Remove empty rows
    janitor::remove_empty(which = "rows") %>%
    # Parse investigator names
    dplyr::mutate(
      dplyr::across(dplyr::everything(), ~ asg_parse_names(.x))
    )
}

#' Join the `team_schedules` Dataset to Teams
#'
#' `asg_join_schedules()` pivots the `teams` data to long format and joins the
#' schedules specified in \code{\link{team_schedules}} to the result.
#'
#' @param teams A data frame containing teams in columns and members in rows;
#'   designed to take the output of
#'   \code{\link[coviData:asg_parse_teams]{asg_parse_teams()}}
#'
#' @param schedules Schedules for each team, given by the `team_schedules`
#'   dataset
#'
#' @return A `tibble` with columns for `team`, `member`, `schedule`, and
#'   `anchor`
#'
#' @family Case Assignment
#'
#' @export
asg_join_schedules <- function(teams, schedules = team_schedules) {
  teams %>%
    tidyr::pivot_longer(
      dplyr::everything(),
      names_to = "team",
      values_to = "member"
    ) %>%
    na.omit() %>%
    dplyr::arrange(.data[["team"]], .data[["member"]]) %>%
    dplyr::left_join(
      team_schedules,
      by = "team"
    )
}

#' Join Nights and Weekends Schedules to Teams Data
#'
#' `asg_join_nights_weekends()` loads individual schedules for investigators on
#' night or weekend shifts and joins them with the output of
#' `asg_join_schedules()`.
#'
#' @inheritParams asg_load_nights_weekends
#'
#' @param .data The output of `as_join_schedules()`
#'
#' @return A `tibble` with a `schedule_custom` column added
#'
#' @family Case Assignment
#'
#' @export
asg_join_nights_weekends <- function(
  .data,
  path = path_create(
    "V:/Administration/Schedules/Night&Weekend staff",
    ext = "xlsx"
  ),
  sheet = "schedules"
) {

  nights_weekends_schedules <- path %>%
    asg_load_nights_weekends(
      sheet = sheet,
      clean_names = TRUE
    ) %>%
    asg_parse_nights_weekends()

  nights_weekends_teams <- .data %>%
    dplyr::filter(.data[["schedule"]] == "nights-weekends")

  # Join nights-weekends only for more informative message
  rlang::inform("Joining nights-weekends team schedules...")
  tidylog::left_join(
    nights_weekends_teams,
    nights_weekends_schedules,
    by = "member",
    suffix = c("", "_custom")
  )

  # Join full data for output
  .data %>%
    dplyr::left_join(
      nights_weekends_schedules,
      by = "member",
      suffix = c("", "_custom")
    ) %>%
    dplyr::rename(cycle = .data[["schedule_custom"]])
}

#' Calculate Whether Investigators Are Working on a Given Date
#'
#' `asg_calc_schedules()` takes the output of `asg_join_nights_weekends()` and
#' calcuates whether investigators are scheduled to work on `date`. Either a
#' predefined schedule or a custom schedule must be supplied for every
#' investigator.
#'
#' @param .data The output of `asg_join_nights_weekends()`
#'
#' @param date The date to use when checking whether investigators are scheduled
#'
#' @return A `tibble` with columns `team`, `member`, and `scheduled`
#'
#' @family Case Assignment
#'
#' @export
asg_calc_schedules <- function(.data, date = Sys.Date()) {
  # Convert to arguments expected by `asg_schedule_predefined()`
  data_predefined <- .data %>%
    dplyr::mutate(
      schedule_predefined = .data[["schedule"]] %>%
        stringr::str_remove_all(pattern = "-") %>%
        stringr::str_replace_all(
          pattern = "nightsweekends",
          replacement = "custom"
        )
    )

  data_predefined %>%
    dplyr::mutate(
      scheduled = data_predefined %>%
        dplyr::select("schedule_predefined", "anchor", "cycle") %>%
        purrr::pmap_lgl(
          ~ asg_schedule_predefined(
            schedule = ..1,
            start = date,
            end = date + 1L,
            anchor = ..2,
            cycle = ..3
          ) %>%
            dplyr::pull("scheduled") %>%
            extract2(1L)
        )
    ) %>%
    dplyr::select("team", "member", "scheduled")
}
