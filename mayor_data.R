devtools::load_all()
library(tidyverse)

readxl::read_xlsx(
  path = "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/COVID CASES_MAYORS DATA REQUEST/Positives09152020/Positives_9-30.xlsx",
) %>%
  janitor::clean_names() ->
mayor_data

mayor_data %>%
  select(event_date, patient_city, patient_age_reported) %>%
  type_convert() %>%
  mutate(across(where(is.character), factor)) %>%
  mutate(event_date = as.Date(event_date,origin = "1900-01-01")) %>%
  mutate(
    patient_city = patient_city %>%
    addNA() %>%
    forcats::fct_collapse(
      Arlington = c("Arlington", "ARLINGTON"),
      Bartlett = c("Barlett", "BARLETT", "Bartlet", "bartlett", "Bartlett", "BARTLETT", "Bartlette"),
      Collierville = c("COLLERVILLE", "COLLIERVIELLE", "Colliervile", "collierville", "Collierville", "COLLIERVILLE", "Collerville"),
      Germantown = c("Gemantown", "GER", "germantown", "Germantown", "GERMANTOWN", "Gernantown"),
      Lakeland = c("Lakeland", "LAKELAND", "LAKLAND"),
      Memphis = c("Meaphis", "MEM", "Mem[his", "Memhis", "MEMHIS", "Memphiis", "memphis", "Memphis", "MeMphis", "MEmphis", "MEMPHIS", "Memphis,", "Memphjis", "MEMPHS", "Memphsi", "Mempis", "MEMPIS", "Mempjhis", "Mempphis", "Mephis", "MEPHIS", "Mmephis", "MMEPHIS", "MPHS", "MPHS,", "Mtemphis", "MEMPHI", "Memphid", "MEMPHIS TN 3810", "Memphis, TN 38104"),
      Millington = c("Millington", "MILLINGTON", "MILLINGTON TN", "Willington", "MILLINGTION")
      # other_level = "Other/Unincorporated/Missing"
    )
  ) ->
line_data

levels(line_data$patient_city)

line_data %>%
  count(patient_city, event_date) %>%
  arrange(patient_city, event_date) %>%
  group_by(patient_city) %>%
  summarize(event_date = event_date, cases = n) %>%
  merge(
    y = tibble(
      event_date = seq(min(line_data$event_date, na.rm = TRUE), max(line_data$event_date, na.rm = TRUE), 1)
    ),
    all.y = T
  ) %>%
  as_tibble() %>%
  pivot_wider(names_from = patient_city, values_from = cases) %>%
  mutate(across(!matches("event_date"), .fns = replace_na, replace = 0)) %>%
  pivot_longer(cols = !matches("event_date"), names_to = "patient_city", values_to = "cases") %>%
  group_by(patient_city) %>%
  summarize(
    event_date,
    cases,
    avg_cases = zoo::rollmean(cases, k = 7, na.pad = TRUE, align = "right"),
    smth_cases = smoother::smth.gaussian(cases, window = 21, alpha = 1.96, tails = TRUE)
  ) ->
data
data %>%
  select(patient_city, event_date, cases) %>%
  pivot_wider(names_from = patient_city, values_from = cases)

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


data %>%
  group_by(patient_city) %>%
  filter(event_date < "2020-07-01") %>%
  summarize(pre_avg = mean(cases, na.rm = TRUE)) ->
std_avg

data %>%
  merge(y = std_avg, all.x = T) %>%
  as_tibble() %>%
  mutate(pct_chg = 100*cases/pre_avg) %>%
  group_by(patient_city) %>%
  summarize(event_date, cases, pct_chg,
            s_pct_chg = 100*smoother::smth.gaussian(cases, window = 14, alpha = 1.96, tails = TRUE)/pre_avg - 100) %>%
  filter(event_date >= "2020-07-01") ->
data_2

ggplot(data_2, aes(x = event_date)) +
  facet_grid(vars(patient_city)) +
  geom_tile(aes(y = 1, fill = s_pct_chg)) +
  scale_y_continuous(breaks = NULL) +
  scale_fill_viridis_c(name = "Percent Change\nfrom Baseline", option = "A") +
  ggtitle("Cases in 5-18 Year Olds by Municipality (Standardized by Pre-July Average)")
