library(dplyr)
library(fs)
library(ggplot2)
library(tidylog)
import::from(coviData, load_nbs, preprocess)
import::from(fst, fst)
import::from(lubridate, as_date)
import::from(magrittr, `%>%`)

nbs_data <- load_nbs(Sys.Date() - 1) %>% preprocess()

report_dates_path <- path(
  path_expand_r("~"),
  "report_dates",
  "data",
  "report_dates",
  ext = "fst"
)

report_dates <- fst(report_dates_path) %>%
  as_tibble() %>%
  mutate(report_date = as_date(report_date))

left_join(nbs_data, report_dates, by = "inv_local_id") %>%
  select(specimen_coll_dt, report_date) %>%
  filter(
    !is.na(specimen_coll_dt),
    !is.na(report_date),
    between(specimen_coll_dt, as_date("2020-03-05"), Sys.Date() - 1),
    between(report_date, as_date("2020-04-12"), Sys.Date() - 1)
  ) %>%
  mutate(delay = report_date - specimen_coll_dt) %>%
  filter(between(delay, 0, Sys.Date() - as_date("2020-03-05"))) ->
report_data

ggplot(report_data, aes(x = report_date, y = delay)) +
  ggthemes::theme_fivethirtyeight() +
  geom_point(alpha = 1/3, color = "cornflowerblue") +
  theme(
    axis.title = element_text(face = "bold", hjust = 0.5),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(face = "bold", hjust = 0.5),
    plot.caption = element_text(face = "italic", hjust = 0.5)
    ) +
  xlab("Report Date") +
  ylab("Delay (Days)") +
  labs(
    title = "Reporting Delays by Report Date for All Investigations",
    subtitle = glue("As of {format(Sys.Date(), '%b %d, %Y')}"),
    caption = glue(
      "Excludes missing specimen collection and report dates (N = 13,835)\n",
      "Data Source: National Electronic Disease Surveillance System (NEDSS)"
    )
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%B")

report_data %>%
  transmute(
    report_date,
    delay = delay
  ) %>%
  arrange(report_date) %>%
  anomalize::time_decompose(
    target = delay,
    frequency = "7 days",
    trend = "30 days",
    method = "stl"
  ) %>%
  mutate(remainder = timetk::box_cox_vec(remainder - min(remainder, na.rm = TRUE))) %>%
  anomalize::anomalize(remainder, method = "gesd", alpha = 0.01, max_anoms = 0.01) %>%
  anomalize::plot_anomalies()



report_data %>%
  filter(report_date >= "2020-05-01") %>%
  count(specimen_coll_dt, delay) ->
modeling_data

NobBS::NobBS(
  data = report_data %>%
    select(-delay) %>%
    rename(onset_week = specimen_coll_dt, report_week = report_date) %>%
    as.data.frame(),
  units = "1 day",
  now = Sys.Date() - 1,
  onset_date = "onset_week",
  report_date = "report_week",
  moving_window = 30,
  specs = list(nSamp = 10000, nBurnin = 1000)
)$estimates

# Options

# beast + bsts/CausalImpact
# brms replication of NobBS (since the latter doesn't work :/ )
