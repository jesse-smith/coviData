team_schedules <- tibble::tribble(
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

usethis::use_data(team_schedules, overwrite = TRUE)
