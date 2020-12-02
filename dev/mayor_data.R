devtools::load_all()
library(tidyverse)

# Read positives
readxl::read_xlsx(
  path = "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/COVID CASES_MAYORS DATA REQUEST/Positives10152020/Postives_10-15.xlsx",
  guess_max = .Machine$integer.max %/% 100
) ->
mayor_data

# Standardize cities
mayor_data %>%
  standardize_dates() %>%
  dplyr::mutate(

  )

mayor_data %>%
  select(PATIENT_CITY) %>%
  unique()

# Write xlsx
openxlsx::write.xlsx(line_data, "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/COVID CASES_MAYORS DATA REQUEST/Positives10152020/clean_cities_10-15.xlsx")

line_data %>%
  filter(between(spcoldate, as.Date("2020-03-05"), as.Date("2020-09-30"))) %>%
  count(spcoldate, patient_city) %>%
  nest_by(patient_city) %>%
  summarize(data = fill_dates(data, date_col = spcoldate, start = "2020-03-05", end = "2020-09-30")) %>%
  transmute(patient_city, spcoldate = data$spcoldate, cases = replace_na(data$n, replace = 0)) %>%
  ungroup() %>%
  pivot_wider(names_from = patient_city, values_from = cases) ->
case_counts

openxlsx::write.xlsx(case_counts, "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/COVID CASES_MAYORS DATA REQUEST/Positives10152020/case_counts_10-15.xlsx")

ggplot(data, aes(x = event_date)) +
  facet_wrap(vars(patient_city), ncol = 2, scales = "free_y") +
  ggiraph::geom_area_interactive(aes(y = avg_cases, color = patient_city, fill = patient_city), show.legend = FALSE, alpha = 0.5) +
  ggiraph::geom_point_interactive(aes(y = cases, color = patient_city, tooltip = cases), show.legend = F, alpha = 0.25, shape = 1) +
  labs(
    title = "Case Counts by Specimen Collection Date for Each Municipality",
    subtitle = "7-Day Moving Average with Raw Data Points",
    caption = "Data Source: National Electronics Disease Surveillance System (NEDSS)"
  ) +
  xlab("Date") +
  ylab("Case Count") +
  theme(
    plot.title = element_text(size = 16),
    axis.title = element_text(size = 12)
  )
