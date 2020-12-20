count_daily <- function(.data, .col, cumulative = FALSE) {

  .col <- select_colnames(.data, .col)
  assert_cols(.data, .col, n = 1L)

  .data %>%
    dplyr::count(.data[[.col]]) %>%
    timetk::pad_by_time(.data[[.col]], .by = "day", .pad_value = 0) %>%
    purrr::when(
      rlang::is_true(cumulative) ~ dplyr::mutate(., n = cumsum(.data[["n"]])),
      ~ .
    )
}
