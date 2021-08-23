#' Set a Standard Theme for SCHD COVID Plots
#'
#' `set_covid_theme()` is a wrapper around
#' \code{\link[ggthemes:theme_fivethirtyeight]{theme_fivethirtyeight(base_size = 14)}}.
#' It standardizes the base theme used by all SCHD COVID plots.
#'
#' @param gg_obj A `ggplot` object
#'
#' @return The modified `ggplot` object
#'
#' @keywords internal
#'
#' @export
set_covid_theme <- function(gg_obj) {
  gg_obj + ggthemes::theme_fivethirtyeight(base_size = 14L)
}

#' Set Viewing Limits on a Plot
#'
#' `set_limits()` sets limits in the viewing panel of a `ggplot` object.
#'
#' @inherit set_covid_theme params return
#'
#' @param xlim The limits for the x axis, in the scale of the x axis
#'
#' @param ylim The limits for the y axis, in the scale of the y axis
#'
#' @keywords internal
#'
#' @export
set_axis_limits <- function(gg_obj, xlim = NULL, ylim = NULL) {

  if (!is.null(xlim)) {
    vec_assert(xlim, size = 2L)
  }

  if (!is.null(ylim)) {
    vec_assert(ylim, size = 2L)
  }

  gg_obj + ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
}

#' Add Axis Labels to a Plot
#'
#' `add_axis_labels()` adds axis labels to a `ggplot` object.
#'
#' @inherit set_covid_theme params return
#'
#' @param xlab The x axis label
#'
#' @param ylab The y axis label
#'
#' @keywords internal
#'
#' @export
add_axis_labels <- function(gg_obj, xlab = NULL, ylab = NULL) {
  gg_obj +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))
}

#' Add a Title, Subtitle, and/or Caption to a Plot
#'
#' `add_title_caption()` adds a title, subtitle, and/or caption with
#' standardized settings to a `ggplot` object.
#'
#' @inherit set_covid_theme params return
#'
#' @param title The title of the plot
#'
#' @param subtitle The subtitle of the plot
#'
#' @param caption The caption of the plot
#'
#' @keywords internal
#'
#' @export
add_title_caption <- function(
  gg_obj,
  title = NULL,
  subtitle = NULL,
  caption = NULL
) {
  gg_obj +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 16, hjust = 0.5),
      plot.caption = ggplot2::element_text(
        hjust = 0.5,
        size = 12,
        face = "italic"
      )
    )
}

#' Add an Event Marker on a Plot
#'
#' `add_event()` adds a vertical line and a label denoting an event to
#' a `ggplot` object.
#'
#' @inherit set_covid_theme params return
#'
#' @param date The date of the event
#'
#' @param lab_y The y coordinate of the event label
#'
#' @param lab The event label
#'
#' @param hjust The horizontal alignment of the event label; defaults to `0`
#'
#' @param vjust The vertical alignment of the event label; defaults to `0`
#'
#' @param angle The angle of the event label; defaults to `-90`, which reads
#'   "sideways" from top to bottom. This is a tradeoff that creates space on
#'   most time series plots.
#'
#' @param face The font face of the label text
#'
#' @param line The type of vertical line to use
#'
#' @param color Line and text color
#'
#' @param ... Additional arguments to pass to
#'   \code{\link[ggplot2:geom_text]{geom_text()}}
#'
#' @keywords internal
#'
#' @export
add_event <- function(
  gg_obj,
  date,
  lab,
  lab_y,
  hjust = 0,
  vjust = 0,
  angle = -90,
  face = c("bold", "italic", "bold.italic", "plain"),
  line = c("dashed", "solid", "blank", "dotted"),
  color = "grey30",
  ...
) {

  date <- lubridate::as_date(date)

  face <- rlang::arg_match(face)[[1L]]

  line <- rlang::arg_match(line)[[1L]]

  gg_obj +
    ggplot2::geom_vline(
      xintercept = date,
      linetype = line,
      size = 0.5,
      color = color,
    ) +
    ggplot2::annotate(
      "text",
      x = date + 1,
      y = lab_y,
      label = lab,
      hjust = hjust,
      vjust = vjust,
      angle = angle,
      fontface = face,
      color = color,
      ...
    )
}

#' Add a Set of Events From the Shelby County COVID Response to a Plot
#'
#' `add_covid_events()` adds a set of events from the Shelby County COVID-19
#' response to a `ggplot` object.
#'
#' @section Current Events:
#'
#' \itemize{
#'   \item{2020-03-24: Safer-at-Home}
#'   \item{2020-05-04: Phase 1 Reopen}
#'   \item{2020-05-18: Phase 2 Reopen}
#'   \item{2020-07-08: Bars Close, Mask Directive}
#'   \item{2020-09-22: Bars Reopen}
#'   \item{2020-09-29: End State Restrictions}
#'   \item{2020-11-23: Face Mask Order 3, Dining Restrictions}
#'   \item{2020-12-26: Face Mask Order 4, Safer-at-Home}
#'   \item{2021-01-23: Safer-at-Home Lifted}
#'   \item{2021-02-20: Broaden Safety Measures}
#'   \item{2021-03-20: Lift More Restrictions}
#'   \item{2021-04-17: Emphasize Vaccination}
#' }
#'
#' @inherit add_event params return
#'
#' @param ... Additional arguments to pass to
#'   \code{\link[coviData:add_event]{add_event()}}
#'
#' @keywords internal
#'
#' @export
add_covid_events <- function(gg_obj, lab_y, ...) {
  gg_obj %>%
    add_event("2020-03-24", "Safer-at-Home", lab_y = lab_y, ...) %>%
    add_event("2020-05-04", "Phase 1 Reopen", lab_y = lab_y, ...) %>%
    add_event("2020-05-18", "Phase 2 Reopen", lab_y = lab_y, ...) %>%
    add_event(
      "2020-07-08",
      "Face Mask Order 1\nBars Close",
      lab_y = lab_y,
      vjust = 0.6,
      ...
    ) %>%
    add_event("2020-09-22", "Bars Reopen", lab_y = lab_y, ...) %>%
    add_event("2020-09-29", "End State Restrictions", lab_y = lab_y, ...) %>%
    add_event(
      "2020-11-23",
      "Face Mask Order 3\nDining Restrictions",
      lab_y = lab_y,
      vjust = 0.6,
      ...
    ) %>%
    add_event(
      "2020-12-26",
      "Face Mask Order 4\nSafer-at-Home",
      lab_y = lab_y,
      vjust = 0.6,
      ...
    ) %>%
    add_event("2021-01-23", "Safer-At-Home Lifted", lab_y = lab_y, ...) %>%
    add_event("2021-02-20", "Broaden Safety Measures", lab_y = lab_y, ...) %>%
    add_event("2021-03-20", "Lift More Restrictions", lab_y = lab_y, ...) %>%
    add_event("2021-04-17", "Emphasize Vaccination", lab_y = lab_y, ...) %>%
    add_event("2021-05-15", "Ease Mask Requirements", lab_y = lab_y, ...)%>%
    add_event("2021-06-09", "Indoor Mask Requirements for Schools", lab_y = lab_y, ...) %>%
    add_event("2021-06-20", "Mask Requirements for Public, Indoor Settings", lab_y = lab_y, ...)
}

#' Add a Marker for Today's Date to a Plot
#'
#' `add_today()` adds a dotted line and a "Today" label to a `ggplot` object.
#'
#' @inherit add_event params return
#'
#' @param angle The angle of the event label; defaults to `0`
#'
#' @keywords internal
#'
#' @export
add_today <- function(
  gg_obj,
  lab_y,
  hjust = 0,
  vjust = 0,
  angle = 0,
  face = c("plain", "bold", "italic", "bold.italic"),
  line = c("dotted", "dashed", "solid", "blank")
) {
    add_event(
      gg_obj,
      date = lubridate::today(),
      lab_y = lab_y,
      lab = "Today",
      hjust = hjust,
      vjust = vjust,
      angle = angle,
      face = face,
      line = line
    )
}

#' Add a Monthly X-Axis Scale to a Time-Based Plot
#'
#' `add_scale_month()` adds a monthly scale to a `ggplot` object when the
#' x axis is a date or datetime.
#'
#' @inherit set_covid_theme params return
#'
#' @keywords internal
#'
#' @export
add_scale_month <- function(gg_obj) {
  gg_obj + ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%B")
}


#' Save a `ggplot` Object
#'
#' `save_plot()` saves a `ggplot` object to the specified path.
#'
#' @inheritParams set_covid_theme
#'
#' @param path The location and file for saving the plot. The image type is
#'   inferred from the file extension.
#'
#' @param ratio The aspect ratio of the figure; defaults to `c(16, 9)`
#'
#' @param size The relative size of the figure; defaults to `0.75`, meaning that
#'   the figure width will be `0.75 * ratio[[1]]`, and the figure height will be
#'   `0.75 * ratio[[2]]`. Useful for scaling an aspect ratio without calculating
#'   the precise dimensions.
#'
#' @param ... Additional arguments to pass to
#'   \code{\link[ggplot2:ggsave]{ggsave()}}
#'
#' @return The `ggplot` object (invisibly)
#'
#' @export
save_plot <- function(
  gg_obj = ggplot2::last_plot(),
  path,
  ratio = c(16, 9),
  size = 0.75,
  ...
) {

  ratio <- vec_cast(ratio, to = double())
  size <- vec_cast(size, to = double())
  vec_assert(ratio, size = 2L)
  vec_assert(size, size = 1L)

  ggplot2::ggsave(
    filename = path_clean(path),
    plot = gg_obj,
    width = ratio[[1]] * size,
    height = ratio[[2]] * size,
    ...
  )

  invisible(gg_obj)
}

#' Extract an Axis Variable Name
#'
#' `gg_var()` extracts the name of the variable used as the `"x"` or `"y"`
#' `axis` in a `ggplot` object.
#'
#' @inheritParams set_covid_theme
#'
#' @param axis The axis variable to extract
#'
#' @return A character vector of length 1 containing the variable name
#'
#' @keywords internal
#'
#' @export
gg_var <- function(gg_obj, axis = c("x", "y")) {

  axis <- rlang::arg_match(axis)

  var_quo <- gg_obj[["mapping"]][[axis]]

  if (rlang::quo_is_null(var_quo)) {
    NULL
  } else {
    var <- rlang::quo_get_expr(var_quo)
    select_colnames(gg_obj[["data"]], !!var)
  }
}
