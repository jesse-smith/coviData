#' Quickly Create Pre-Defined Work Schedules
#'
#' @description
#'
#' These functions create work schedules for weekday, 4-2, and
#' (5-2)-(5-3)-(6-2)x4-(6-3) schedules between the given `start` and `end`
#' dates. Rotating schedules (the latter two) require specification of a
#' starting ("anchor") point for the scheduling calculation; this is supplied
#' using the `anchor` argument, which is set end `start` by default. Weekly
#' schedules do not require an anchor point.
#'
#' `asg_schedule_weekdays()` defines a Monday-Friday work schedule.
#'
#' `asg_schedule_42()` defines a rotating 4 on, 2 off work schedule.
#'
#' `asg_schedule_5623()` defines a rotating schedule with the following pattern:
#' \itemize{
#'   \item 5 on, 2 off
#'   \item 5 on, 3 off
#'   \item 6 on, 2 off
#'   \item 6 on, 2 off
#'   \item 6 on, 2 off
#'   \item 6 on, 2 off
#'   \item 6 on, 3 off
#' }
#'
#' @details
#'
#' These functions depend on the more general
#' \code{\link[coviData:asg_schedule]{asg_schedule()}} function, which takes an
#' arbitrary weekly or rotating schedule and (if needed) an anchor point. See
#' that function for implementing other schedules.
#'
#' @inherit asg_schedule params return
#'
#' @param schedule A string indicating the schedule to use. For custom
#'   schedules, use `schedule = "custom"`.
#'
#' @family Case Assignment
#'
#' @aliases asg_schedule_weekdays asg_schedule_42 asg_schedule_5623
asg_schedule_predefined <- function(
  schedule = c("weekdays", "42", "5623", "custom"),
  start = Sys.Date(),
  end = start + 29L,
  anchor = start,
  cycle = NULL
) {

  schedule <- rlang::arg_match(schedule)[[1L]]

  if (schedule == "weekdays") {
    asg_schedule_weekdays(start = start, end = end, anchor = anchor)
  } else if (schedule == "42") {
    asg_schedule_42(start = start, end = end, anchor = anchor)
  } else if (schedule == "5623") {
    asg_schedule_5623(start = start, end = end, anchor = anchor)
  } else {
    asg_schedule(cycle = cycle, start = start, end = end, anchor = anchor)
  }
}

#' @rdname asg_schedule_predefined
#'
#' @export
asg_schedule_weekdays <- function(
  start = Sys.Date(),
  end = start + 29L,
  anchor = start
) {

  # Define weekday cycle
  cycle <- c(
    Sunday = FALSE,
    Monday = TRUE,
    Tuesday = TRUE,
    Wednesday = TRUE,
    Thursday = TRUE,
    Friday = TRUE,
    Saturday = FALSE
  )

  asg_schedule(
    cycle = cycle,
    start = start,
    end = end
  )
}

#' @rdname asg_schedule_predefined
#'
#' @export
asg_schedule_42 <- function(
  start = Sys.Date(),
  end = Sys.Date() + 29L,
  anchor = start
) {

  cycle <- c(rep(TRUE, 4L), rep(FALSE, 2L))

  asg_schedule(
    cycle = cycle,
    start = start,
    end = end,
    anchor = anchor
  )
}

#' @rdname asg_schedule_predefined
#'
#' @export
asg_schedule_5623 <- function(
  start = Sys.Date(),
  end = Sys.Date() + 29L,
  anchor = start
) {

  c52 <- c(rep(TRUE, 5L), rep(FALSE, 2L))
  c53 <- c(rep(TRUE, 5L), rep(FALSE, 3L))
  c62 <- c(rep(TRUE, 6L), rep(FALSE, 2L))
  c63 <- c(rep(TRUE, 6L), rep(FALSE, 3L))

  cycle <- c(c52, c53, rep(c62, 4L), c63)

  asg_schedule(
    cycle = cycle,
    start = start,
    end = end,
    anchor = anchor
  )
}

#' Define Rotating or Weekly Work Schedules
#'
#' `asg_schedule()` defines work schedules based off of a given `cycle`. If this
#' cycle is named, it is assumed that the names are days of the week, and that
#' the schedule repeats weekly. If it is unnamed, the `cycle` begins at the
#' `anchor` date. Defining rotating weekly schedules
#' (i.e. T/R one week, M/W/F the next) is not currently supported using weekday
#' names; these schedules may be defined as a rotating schedule anchored to the
#' beginning of a week.
#'
#' `asg_schedule_by_cycle()` and `asg_schedule_by_day()` are the workhorses
#' underlying `as_schedule()`. They handle the general cyclic and weekly use
#' cases described above.
#'
#' @param cycle A logical vector defining one scheduling cycle. If named, names
#'   are passed to \code{\link[coviData:parse_weekday]{parse_weekday()}} for
#'   standardization of weekday names.
#'
#' @param start The start date of the returned schedule; either a string in
#'   "YYYY-MM-DD" format or a `Date` object
#'
#' @param end The end date of the returned schedule; either a string in
#'   "YYYY-MM-DD" format or a `Date` object
#'
#' @param anchor The date from which to start ("anchor") schedule calculations.
#'   This can be any valid date; no particular relationship to `start` or `end`
#'   is needed. It must be either a string in "YYYY-MM-DD" format or a
#'   `Date` object.
#'
#' @return A `tibble` with columns `date` (a `Date` column containing dates
#'   between `start` and `end`, inclusive), `weekday` (a `character` column
#'   containing full weekday names), and `scheduled` (a `logical` column
#'   defining whether a day is schedule ("on") or not ("off"))
#'
#' @family Case Assignment
#'
#' @aliases asg_schedule_by_cycle asg_schedule_by_day
#'
#' @export
asg_schedule <- function(
  cycle = c(
    Sun = FALSE,
    Mon = TRUE,
    Tue = TRUE,
    Wed = TRUE,
    Thu = TRUE,
    Fri = TRUE,
    Sat = FALSE
  ),
  start = Sys.Date(),
  end = Sys.Date() + 29L,
  anchor = start
) {

  cycle_is_named <- !is.null(names(cycle))

  if (!lubridate::is.Date(start)) {
    start <- tibble::tibble(date = start) %>%
      standardize_dates() %>%
      dplyr::pull(.data[["date"]])
  }

  if (!lubridate::is.Date(end)) {
    end <- tibble::tibble(date = end) %>%
      standardize_dates() %>%
      dplyr::pull(.data[["date"]])
  }

  if (!lubridate::is.Date(anchor)) {
    anchor <- tibble::tibble(date = anchor) %>%
      standardize_dates() %>%
      dplyr::pull(.data[["date"]])
  }

  if (cycle_is_named) {
    asg_schedule_by_day(
      cycle = cycle,
      start = start,
      end = end
    )
  } else {
    asg_schedule_by_cycle(
      cycle = cycle,
      start = start,
      end = end,
      anchor = anchor
    )
  }
}

#' @rdname asg_schedule
#'
#' @export
asg_schedule_by_cycle <- function(
  cycle = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
  start = Sys.Date(),
  end = Sys.Date() + 29L,
  anchor = start
) {

  vec_assert(cycle, ptype = logical())

  start <- lubridate::as_date(start)
  end <- lubridate::as_date(end)
  anchor <- lubridate::as_date(anchor)

  if (end < start) {
    start_switched <- end
    to_switched <- start

    start <- start_switched
    end <- to_switched

    remove(start_switched, to_switched)
  } else if (start == end) {
    rlang::abort("`start` must not equal `end`")
  }

  cycle_length <- vec_size(cycle)

  times_start <- abs(anchor - start) %>%
    divide_by(cycle_length) %>%
    ceiling() %>%
    as.integer()

  times_to <- abs(anchor - end) %>%
    divide_by(cycle_length) %>%
    ceiling() %>%
    as.integer()

  start_2_anchor <- rep(cycle, times_start)
  anchor_2_to <- rep(cycle, times_to)

  if (anchor <= start) {
    # `anchor` is also less than `end`
    start_temp <- anchor
    to_temp <- anchor + (cycle_length * times_to - 1L)
    scheduled <- anchor_2_to
  } else if (anchor >= end) {
    # `anchor` is also greater than `start`
    start_temp <- anchor - (cycle_length * times_start - 1L)
    to_temp <- anchor
    scheduled <- start_2_anchor
  } else {
    start_temp <- anchor - (cycle_length * times_start - 1L)
    to_temp <- anchor + (cycle_length * times_to - 1L)
    scheduled <- c(start_2_anchor, anchor_2_to[2:vec_size(anchor_2_to)])
  }

  tibble::tibble(
    date = seq(start_temp, to_temp, by = 1L),
    day = weekdays(date),
    scheduled = scheduled
  ) %>%
    dplyr::filter(dplyr::between(.data[["date"]], start, end))
}

#' @rdname asg_schedule
#'
#' @export
asg_schedule_by_day <- function(
  cycle = c(
    Sun = FALSE,
    Mon = TRUE,
    Tue = TRUE,
    Wed = TRUE,
    Thu = TRUE,
    Fri = TRUE,
    Sat = FALSE
  ),
  start = Sys.Date(),
  end = Sys.Date() + 29L
) {

  # Check that `days` is a logical with a potential value for each weekday
  vec_assert(cycle, ptype = logical(), size = 7L)

  # Check that `days` is a named vector
  days_are_named <- cycle %>%
    names() %>%
    is.null() %>%
    any() %>%
    not()

  if (!days_are_named) {
    rlang::abort(
      paste0(
        "`calculate_schedule_by_day()` requires `days` ",
        "end be a named logical vector"
      )
    )
  }

  # Standardize weekday names
  day_names <- cycle %>% names() %>% parse_weekday()

  # Check that all names are weekdays
  day_names_are_weekdays <- day_names %>%
    is.na() %>%
    any() %>%
    not()

  if (!day_names_are_weekdays) {
    rlang::abort(
      paste0(
        "`calculate_schedule_by_day()` requires day names ",
        "end be names of weekdays or valid abbreviations thereof"
      )
    )
  }

  # Check that an entry is present for each weekday
  day_names_are_unique <- vec_unique_count(day_names)

  if (!day_names_are_unique) {
    rlang::abort(
      paste0(
        "`calculate_schedule_by_day()` requires exactly one value ",
        "for each day of the week"
      )
    )
  }

  # Calculate Schedule
  start <- lubridate::as_date(start)
  end <- lubridate::as_date(end)

  if (end < start) {
    start_switched <- end
    to_switched <- start

    start <- start_switched
    end <- to_switched

    remove(start_switched, to_switched)
  }

  names(cycle) <- day_names

  tibble::tibble(
    date = seq(start, end, by = 1L),
    day = weekdays(date),
    scheduled = cycle[day]
  )
}
