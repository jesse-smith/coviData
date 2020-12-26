#' Create a Scheduling Calendar
#'
#' `schedule_calendar()` creates a calendar from a given `schedule`, `start`
#' date, `end` date, and (if needed) `anchor` date. Custom schedules can be used
#' by setting `schedule = "custom"` and providing the scheduling cycle via
#' `cycle`. The schedule can be saved to pdf with `.pdf = TRUE`; the file name
#' is a standardized version of the title.
#'
#' @inheritParams asg_schedule
#'
#' @param title The title of the calendar
#'
#' @param schedule The schedule to use
#'
#' @param .pdf Should the schedule be saved as a pdf?
#'
#' @param .dir The directory in which to save the schedule if `.pdf = TRUE`;
#'   defaults to the `R_USER` directory
#'
#' @return A list defining the `calendR` object
schedule_calendar <- function(
  title = "Work Schedule",
  schedule = c("weekdays", "42", "5623", "custom"),
  start = "2021-01-01",
  end = "2021-12-31",
  anchor = start,
  cycle = NULL,
  .pdf = FALSE,
  .dir = "~/"
) {

  # Make sure 'calendR' is installed
  if (!rlang::is_installed("calendR")) {
    rlang::abort(
      paste0(
        "This function require the 'calendR' package; please install using\n\n",
        "`install.packages('calendR')`\n\n",
        "to continue."
      )
    )
  }

  # Check that `schedule` matches one of the acceptable arguments
  schedule <- rlang::arg_match(schedule)[[1]]

  # Handle cases of `schedule`
  if (schedule == "weekdays") {
    schedule <- calculate_schedule_weekdays(
      start = start,
      end = end
    )
  } else if (schedule == "42") {
    schedule <- calculate_schedule_42(
      start = start,
      end = end,
      anchor = anchor
    )
  } else if (schedule == "5623") {
    schedule <- calculate_schedule_5623(
      start = start,
      end = end,
      anchor = anchor
    )
  } else {
    schedule <- calculate_schedule(
      cycle = cycle,
      start = start,
      end = end,
      anchor = anchor
    )
  }

  # Convert the schedule to a vector of "special days" for calendR
  scheduled_days <- schedule %>%
    dplyr::mutate(row = vec_seq_along(.)) %>%
    dplyr::filter(.data[["scheduled"]]) %>%
    dplyr::pull(.data[["row"]])

  start_date <- min(schedule[["date"]], na.rm = TRUE)
  end_date <- max(schedule[["date"]], na.rm = TRUE)

  if (.pdf) {
    save_as <- path_create(.dir, snakecase::to_snake_case(title))
  } else {
    save_as <- ""
  }

  calendR::calendR(
    start_date = start_date,
    end_date = end_date,
    special.days = scheduled_days,
    title = title,
    font.style = "bold",
    title.col     = rgb(0.098, 0.098, 0.439, alpha = 0.9),
    mbg.col       = rgb(0.098, 0.098, 0.439, alpha = 0.9),
    weeknames.col = rgb(0.098, 0.098, 0.439, alpha = 0.9),
    bg.col     = "#f0f0f0",
    months.col = "#f0f0f0",
    low.col    = "#f0f0f0",
    days.col = rgb(0.098, 0.098, 0.439),
    col      = rgb(0.098, 0.098, 0.439),
    special.col = rgb(0.098, 0.098, 0.439, alpha = 0.3),
    doc_name = save_as,
    papersize = "A5",
    lwd = 1/3,
    pdf = .pdf
  )
}
