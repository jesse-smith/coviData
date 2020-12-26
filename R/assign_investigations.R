load_investigator_schedule <- function(
  path = path_create(
    "V:/Administration/Schedules/Copy of Revised Shift Schedules 12.06.2020",
    ext = "xlsx"
  )
) {

  path <- path_create(path)

  path %>%
    readxl::read_excel(col_types = "text") %>%
    janitor::clean_names() %>%
    dplyr::rename(role = 1L)
}

parse_investigator_schedule <- function(.data) {
  .data %>%
    dplyr::mutate(
      row = vec_seq_along(.),
      inv_role = tidyr::replace_na(.data[["role"]] == "Investigators", FALSE),
      inv_start_row = .data[["row"]][inv_role],
      .before = 1L
    ) %>%
    dplyr::filter(.data[["row"]] >= .data[["inv_start_row"]]) %>%
    dplyr::select(-c("row", "inv_role", "inv_start_row", "role")) %>%
    janitor::remove_empty(which = "rows") %>%
    dplyr::mutate(
      dplyr::across(
        .fns = ~ .x %>%
          stringr::str_split(pattern = "[\n\t\r ]+") %>%
          purrr::map_chr(
            ~ .x %>%
              vec_slice(i = if (vec_size(.x) == 1L) 1L else c(1L, 2L)) %>%
              as.character() %>%
              stringr::str_flatten(collapse = " ")
          )
      )
    )
}

tibble::tribble(
    ~team,         ~schedule,       ~anchor,
      "a",        "weekdays", as.Date(NA),
      "b",        "weekdays", as.Date(NA),
      "c",        "weekdays", as.Date(NA),
      "d",             "4-2", as.Date("2020-11-15"),
      "e",             "4-2", as.Date("2020-11-14"),
      "f",             "4-2", as.Date("2020-11-12"),
      "g", "nights-weekends", as.Date(NA),
      "h", "nights-weekends", as.Date(NA)
)

create_schedules <- function(
  .investigator_schedule,
  .reference,
  date = Sys.Date()
) {
  .reference %>%
    dplyr::mutate(
      working = NA
    ) %>%
    dplyr::mutate(

    )
}


calculate_schedule_weekdays <- function(
  from = Sys.Date(),
  to = Sys.Date() + 29L,
  .anchor = from
) {

  cycle <- c(
    Sunday = FALSE,
    Monday = TRUE,
    Tuesday = TRUE,
    Wednesday = TRUE,
    Thursday = TRUE,
    Friday = TRUE,
    Saturday = FALSE
  )

  calculate_schedule(
    cycle = cycle,
    from = from,
    to = to
  )
}

calculate_schedule_42 <- function(
  from = Sys.Date(),
  to = Sys.Date() + 29L,
  .anchor = from
) {

  cycle <- c(rep(TRUE, 4L), rep(FALSE, 2L))

  calculate_schedule(
    cycle = cycle,
    from = from,
    to = to,
    .anchor = .anchor
  )
}

calculate_schedule_5623 <- function(
  from = Sys.Date(),
  to = Sys.Date() + 29L,
  .anchor = from
) {

  c52 <- c(rep(TRUE, 5L), rep(FALSE, 2L))
  c53 <- c(rep(TRUE, 5L), rep(FALSE, 3L))
  c62 <- c(rep(TRUE, 6L), rep(FALSE, 2L))
  c63 <- c(rep(TRUE, 6L), rep(FALSE, 3L))

  cycle <- c(c52, c53, rep(c62, 4L), c63)

  calculate_schedule(
    cycle = cycle,
    from = from,
    to = to,
    .anchor = .anchor
  )
}

calculate_schedule <- function(
  cycle = c(
    Sun = FALSE,
    Mon = TRUE,
    Tue = TRUE,
    Wed = TRUE,
    Thu = TRUE,
    Fri = TRUE,
    Sat = FALSE
  ),
  from = Sys.Date(),
  to = Sys.Date() + 29L,
  .anchor = from
) {

  cycle_is_named <- !is.null(names(cycle))

  if (!lubridate::is.Date(from)) {
    from <- tibble::tibble(date = from) %>%
      standardize_dates() %>%
      dplyr::pull(.data[["date"]])
  }

  if (!lubridate::is.Date(to)) {
    to <- tibble::tibble(date = to) %>%
      standardize_dates() %>%
      dplyr::pull(.data[["date"]])
  }

  if (!lubridate::is.Date(.anchor)) {
    .anchor <- tibble::tibble(date = .anchor) %>%
      standardize_dates() %>%
      dplyr::pull(.data[["date"]])
  }

  if (cycle_is_named) {
    calculate_schedule_by_day(
      cycle = cycle,
      from = from,
      to = to
    )
  } else {
    calculate_schedule_by_cycle(
      cycle = cycle,
      from = from,
      to = to,
      .anchor = .anchor
    )
  }
}

calculate_schedule_by_cycle <- function(
  cycle = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
  from = Sys.Date(),
  to = Sys.Date() + 29L,
  .anchor = from
) {

  vec_assert(cycle, ptype = logical())

  from <- lubridate::as_date(from)
  to <- lubridate::as_date(to)
  .anchor <- lubridate::as_date(.anchor)

  if (to < from) {
    from_switched <- to
    to_switched <- from

    from <- from_switched
    to <- to_switched

    remove(from_switched, to_switched)
  } else if (from == to) {
    rlang::abort("`from` must not equal `to`")
  }

  cycle_length <- vec_size(cycle)

  times_from <- abs(.anchor - from) %>%
    divide_by(cycle_length) %>%
    ceiling() %>%
    as.integer()

  times_to <- abs(.anchor - to) %>%
    divide_by(cycle_length) %>%
    ceiling() %>%
    as.integer()

  from_2_anchor <- rep(cycle, times_from)
  anchor_2_to <- rep(cycle, times_to)

  if (.anchor <= from) {
    # `.anchor` is also less than `to`
    from_temp <- .anchor
    to_temp <- .anchor + (cycle_length * times_to - 1L)
    scheduled <- anchor_2_to
  } else if (.anchor >= to) {
    # `.anchor` is also greater than `from`
    from_temp <- .anchor - (cycle_length * times_from - 1L)
    to_temp <- .anchor
    scheduled <- from_2_anchor
  } else {
    from_temp <- .anchor - (cycle_length * times_from - 1L)
    to_temp <- .anchor + (cycle_length * times_to - 1L)
    scheduled <- c(from_2_anchor, anchor_2_to[2:vec_size(anchor_2_to)])
  }

  tibble::tibble(
    date = seq(from_temp, to_temp, by = 1L),
    day = weekdays(date),
    scheduled = scheduled
  ) %>%
    dplyr::filter(dplyr::between(.data[["date"]], from, to))
}

calculate_schedule_by_day <- function(
  cycle = c(
    Sun = FALSE,
    Mon = TRUE,
    Tue = TRUE,
    Wed = TRUE,
    Thu = TRUE,
    Fri = TRUE,
    Sat = FALSE
  ),
  from = Sys.Date(),
  to = Sys.Date() + 29L
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
        "to be a named logical vector"
      )
    )
  }

  # Standardize weekday names
  day_names <- cycle %>% names() %>% match_weekdays()

  # Check that all names are weekdays
  day_names_are_weekdays <- day_names %>%
    is.na() %>%
    any() %>%
    not()

  if (!day_names_are_weekdays) {
    rlang::abort(
      paste0(
        "`calculate_schedule_by_day()` requires day names ",
        "to be names of weekdays or valid abbreviations thereof"
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
  from <- lubridate::as_date(from)
  to <- lubridate::as_date(to)

  if (to < from) {
    from_switched <- to
    to_switched <- from

    from <- from_switched
    to <- to_switched

    remove(from_switched, to_switched)
  }

  names(cycle) <- day_names

  tibble::tibble(
    date = seq(from, to, by = 1L),
    day = weekdays(date),
    scheduled = cycle[day]
  )
}

match_weekdays <- function(day) {

  vec_assert(day, ptype = character())

  day <- standardize_string(day)

  day_len_1 <- stringr::str_length(day) == 1L

  dplyr::case_when(
    day_len_1 & day == "U" ~ "Sunday",
    day_len_1 & day == "M" ~ "Monday",
    day_len_1 & day == "T" ~ "Tuesday",
    day_len_1 & day == "W" ~ "Wednesday",
    day_len_1 & day == "R" ~ "Thursday",
    day_len_1 & day == "F" ~ "Friday",
    day_len_1 & day == "S" ~ "Saturday",
    stringr::str_starts(day, "Su") ~ "Sunday",
    stringr::str_starts(day, "Mo") ~ "Monday",
    stringr::str_starts(day, "Tu") ~ "Tuesday",
    stringr::str_starts(day, "We") ~ "Wednesday",
    stringr::str_starts(day, "Th") ~ "Thursday",
    stringr::str_starts(day, "Fr") ~ "Friday",
    stringr::str_starts(day, "Sa") ~ "Saturday"
  )
}

create_schedule_calendar <- function(
  title = "Work Schedule",
  schedule = c("weekdays", "42", "5623", "custom"),
  from = "2021-01-01",
  to = "2021-12-31",
  .anchor = from,
  .cycle = NULL,
  .pdf = FALSE,
  .dir = ""
) {

  schedule <- rlang::arg_match(schedule)[[1]]

  if (schedule == "weekdays") {
    schedule <- calculate_schedule_weekdays(
      from = from,
      to = to
    )
  } else if (schedule == "42") {
    schedule <- calculate_schedule_42(
      from = from,
      to = to,
      .anchor = .anchor
    )
  } else if (schedule == "5623") {
    schedule <- calculate_schedule_5623(
      from = from,
      to = to,
      .anchor = .anchor
    )
  } else {
    schedule <- calculate_schedule(
      cycle = .cycle,
      from = from,
      to = to,
      .anchor = .anchor
    )
  }

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
