summarize_incidence <- function(.data, .col = "collection_date") {
  .data %>%
    dplyr::count(.col)
}
