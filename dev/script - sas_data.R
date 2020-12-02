library(tidyverse)
ppl_pos <- coviData::load_sas(dataset = "positive_ppl")
ppl_neg <- load_sas(date = Sys.Date() - 1, category = "negative_ppl")
pcr_pos <- coviData::load_sas(date = Sys.Date() - 1, category = "positive_pcr")
pcr_neg <- load_sas(date = Sys.Date() - 1, category = "negative_pcr")

ppl_pos %>%
  dplyr::filter(
    spcoldate >= "2020-03-05",
    spcoldate <= Sys.Date(),
    !is.na(spcoldate)
  ) %>%
  dplyr::mutate(spcoldate = as.Date(spcoldate), inv_rpt_dt = as.Date(inv_rpt_dt)) %>%
  dplyr::count(spcoldate) %>%
  dplyr::rename(date = spcoldate, cases = n) %>%
  dplyr::mutate(
    avg = cases %>%
      zoo::rollmean(k = 7, na.pad = TRUE, align = "right") %>%
      {.[1:(NROW(.)-5)]} %>%
      c(., rep(NA, 5))
  ) ->
cases

merge(
  x = cases,
  y = tibble(
    date = seq(
      min(cases$date, na.rm = TRUE),
      max(cases$date, na.rm = TRUE),
      by = 1
    )
  ),
  by = "date",
  all.y = TRUE
) %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(
    smth = cases %>%
      {.[1:(NROW(.)-4)]} %>%
      log1p() %>%
      extract_trend(dates = date %>% {.[1:(NROW(.)-4)]} %>% as.Date()) %>%
      expm1() %>%
      (function(x) {x[x < 0] <- sqrt(.Machine$double.eps); x}) %>%
      c(.,rep(NA, 4))
  ) ->
cases



ggplot2::ggplot(cases, ggplot2::aes(x = date)) +
  ggplot2::geom_area(ggplot2::aes(y = cases), width = 1, fill = "cornflowerblue", alpha = 0.8) +
  # ggplot2::geom_line(ggplot2::aes(y = smth), color = "navy", size = 1.5) +
  ggplot2::scale_x_date(
    name = "Date",
    breaks = seq(as.Date("2020-03-01"), as.Date("2030-01-01"), by = 7),
    minor_breaks = NULL,
    date_labels = "%b %d"
  ) +
  ggplot2::labs(
    title = "New Cases by Specimen Collection Date"
  ) +
  ggplot2::xlab("Date") +
  ggplot2::ylab("Cases") +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1)
  )

ggplot2::ggsave("curve.png", width = 16, height = 9, bg = "transparent")
