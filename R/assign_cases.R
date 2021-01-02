#' Load the Current Investigator Team Assignments
#'
#' `assignment_load_teams()` loads the current investigators on each team start an
#' Excel file. It is mostly a wrapper around
#' \code{\link[readxl:read_excel]{read_excel()}}, If `clean_names = TRUE`,
#' it also standardizes names with
#' \code{\link[janitor:clean_names]{clean_names()}} and gives the name
#' `role` end the first column (which is unnamed in the Excel sheet).
#'
#' @param path The path to the Excel workbook containing team membership data
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
    "V:/Administration/Schedules/Investigation Staff Schedule",
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
#' `asg_add_nights_weekends_schedules()` loads individual schedules for
#' investigators on night or weekend shifts and joins them with the output of
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
asg_add_nights_weekends_schedules <- function(
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
#' `asg_calc_schedules()` takes the output of
#' `asg_add_nights_weekends_schedules()` and calcuates whether investigators are
#' scheduled to work on `date`. Either a predefined schedule or a custom
#' schedule must be supplied for every investigator.
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

#' Get Investigators Scheduled to Work on a Given Date
#'
#' `asg_get_scheduled_investigators()` parses scheduling files and returns
#' investigators scheduled to work on `date`.
#'
#' @inheritParams asg_calc_schedules
#'
#' @param path_teams The location of the teams scheduling Excel workbook
#'   data
#'
#' @param path_nights_weekends The location of the nights and weekends
#'   scheduling Excel workbook
#'
#' @param team_schedules A `tibble` containing information for parsing schedules
#'   into dates; the default is the built-in `team_schedules` dataset and should
#'   probably not be changed
#'
#' @param scheduled_only Should only scheduled investigators be returned? If
#'   `FALSE`, all investigators will be returned. Useful for debugging.
#'
#' @return A `tibble` containing `team`, `investigator`, and `scheduled` columns
#'
#' @export
asg_get_scheduled_investigators <- function(
  date = Sys.Date(),
  path_teams = path_create(
    "V:/Administration/Schedules/Investigation Staff Schedule",
    ext = "xlsx"
  ),
  path_nights_weekends = path_create(
    "V:/Administration/Schedules/Night&Weekend staff",
    ext = "xlsx"
  ),
  team_schedules = team_schedules,
  scheduled_only = TRUE
) {
  rlang::inform("Loading teams...")
  asg_load_teams(path = path_teams) %>%
    asg_parse_teams() %>%
    asg_join_schedules(schedules = team_schedules) %>%
    asg_add_nights_weekends_schedules(path = path_nights_weekends) %>%
    dplyr::mutate(
      null_cycle = purrr::map_lgl(.data[["cycle"]], ~ is.null(.x))
    ) %>%
    # Get rid of investigators with no schedule
    dplyr::filter(
      !(.data[["schedule"]] == "nights-weekends" & .data[["null_cycle"]])
    ) %>%
    dplyr::select(-"null_cycle") %>%
    asg_calc_schedules(date = date) %>%
    dplyr::rename(investigator = .data[["member"]]) %>%
    purrr::when(
      scheduled_only ~ dplyr::filter(., .data[["scheduled"]]),
      ~ .
    )
}

#' Assign Cases to Scheduled Investigators
#'
#' `asg_assign_cases()` divides unassigned cases among REDcap investigators
#' scheduled to work on `date`. It distributes cases randomly and evenly across
#' investigators until either all are assigned or there are too few cases for
#' another round of even distribution. Any remaining cases are assigned randomly
#' with at most one additional case given to an investigator.
#'
#' @inheritParams asg_join_investigators
#'
#' @return A `tibble` containing the results of
#'   \code{
#'   \link[coviData:asg_download_redcap_cases]{asg_download_redcap_cases()}
#'   }, with
#'   `assign_date` and `investigator` filled
#'
#' @export
asg_assign_cases <- function(
  date = Sys.Date(),
  api_token = Sys.getenv("redcap_CA_token")
) {

  cases <- asg_download_redcap_cases(api_token = api_token)
  investigators <- asg_join_investigators(date = date, api_token = api_token)

  # Get number of unassigned cases and investigators
  n_cases <- vec_size(cases)
  n_investigators <- vec_size(investigators)

  # Get number of investigators needed
  n_reps <- n_cases %/% n_investigators
  n_additional <- n_cases - n_investigators * n_reps

  # Create randomized vector of investigator assignments
  investigator_ids <- investigators %>%
    vec_rep(times = n_reps) %>%
    dplyr::bind_rows(dplyr::slice_sample(investigators, n = n_additional)) %>%
    dplyr::slice_sample(prop = 1L) %>%
    dplyr::pull("id")

  # Replace `investigator` column with randomized `investigators` and fill
  # `assign_date`
  dplyr::mutate(
    cases,
    investigator = investigator_ids,
    assign_date = format(Sys.time(), "%Y-%m-%d %H:%M")
  )
}
