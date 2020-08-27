get_nbs <- function(
  day = Sys.Date(),
  dir_path = "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/Sandbox data pull Final/",
  file_name = NULL,
  encoding = TRUE,
  clean = TRUE,
  min_completion_rate = 0.5,
  clean_quietly = TRUE
) {

  message(paste0("\nDate used: ", day, "\n"))

  pattern <- paste0(day, ".*", ".csv")

  # Find, read, and clean NBS file
  find_file(
    day = day,
    dir_path = dir_path,
    pattern = pattern,
    file_name = file_name
  ) %>%
    read_nbs() ->
  data

  if (clean) {
    clean_nbs(
      data,
      min_completion_rate = min_completion_rate,
      quiet = clean_quietly
    )
  } else {
    data
  }
}

read_nbs <- function(
  path,
  nThread = 4
) {
  message("Reading NBS data file...")
  data.table::fread(
    path,
    header = TRUE,
    colClasses = "character",
    blank.lines.skip = TRUE,
    nThread = nThread
  )
}

standardize_dates <- function(
  .data,
  suffix = c("_dt", "_dob"),
  prob_format = c("dmy", "mdy", "ymd", "ymdT")
) {
  .data %>%
    select(ends_with(suffix)) ->
  date_data

  date_cols <- colnames(date_data)

  date_data %>%
    type_convert() %>%
    mutate(
      across(
        where(is.character),
        lubridate::parse_date_time,
        orders = prob_format
      )
    ) %>%
    mutate(across(.fns = as.Date)) ->
  .data[date_cols]

  .data
}

clean_nbs <- function(
  .data,
  min_completion_rate = 0.5,
  quiet = TRUE
) {

  message("\nCleaning variables and data types...", appendLF = FALSE)
  cleaned_data <- initial_clean(.data)
  message("Done.\n")

  message("Factoring data for further wrangling...", appendLF = FALSE)
  cleaned_data %>%
    mutate(across(where(not_factor_date), factor)) ->
  factor_data
  message("Done.\n")

  message("Finding informative variables in:")

  message("all data...")
  factor_data %>%
    cols_to_keep(min_completion_rate = min_completion_rate) ->
  all_cols

  message("cases...")
  factor_data %>%
    filter(inv_case_status == "C" | inv_case_status == "P") %>%
    cols_to_keep(min_completion_rate = min_completion_rate) ->
  case_cols

  message("hospitalizations...")
  factor_data %>%
    filter(hsptlizd_ind == "Y") %>%
    cols_to_keep(min_completion_rate = min_completion_rate) ->
  hosp_cols

  message("deaths...")
  factor_data %>%
    filter(die_from_illness_ind == "Y") %>%
    cols_to_keep(min_completion_rate = min_completion_rate) ->
  death_cols
  message("Done.\n")

  message(
    "Removing uninformative variables and observations...",
    appendLF = FALSE
  )
  all_cols$info %>%
    union(case_cols$info) %>%
    union(hosp_cols$info) %>%
    union(death_cols$info) ->
  keep_cols_info

  all_cols$missing %>%
    union(case_cols$missing) %>%
    union(hosp_cols$missing) %>%
    union(death_cols$missing) ->
  keep_cols_missing

  keep_cols <- intersect(keep_cols_info, keep_cols_missing)

  cleaned_data %>%
    select(
      matches(keep_cols)
    ) %>%
    janitor::remove_empty(which = "rows") %>%
    as_tibble() %>%
    (function(.data) {message("Done!"); return(.data)})
}


cols_to_keep <- function(.data, min_completion_rate = 0.5) {

  .factor_data <- mutate(.data, across(where(not_factor_date), factor))

  .factor_data %>%
    summarize(across(everything(), skimr::n_unique)) %>%
    pivot_longer(cols = everything()) %>%
    filter(value < NROW(.factor_data)) %>%
    select(name) %>%
    .[[1]] ->
  keep_cols_info

  .factor_data %>%
    summarize(across(.fns = skimr::complete_rate)) %>%
    pivot_longer(cols = everything()) %>%
    filter(value >= min_completion_rate) %>%
    select(name) %>%
    .[[1]] ->
  keep_cols_missing

  list(info = keep_cols_info, missing = keep_cols_missing)
}

not_factor_date <- function(x) {
  !(is.factor(x) | lubridate::is.POSIXt(x))
}

initial_clean <- function(.data) {
  suppressMessages(
    .data %>%
      janitor::clean_names() %>%
      standardize_dates() %>%
      type_convert() %>%
      janitor::remove_empty(which = "cols") %>%
      janitor::remove_constant(na.rm = TRUE) %>%
      mutate(across(where(is.character), str_to_factor, encoding = TRUE))
  )
}
