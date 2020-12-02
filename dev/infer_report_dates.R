library(fs)
library(dplyr)
library(purrr)
library(progress)
import::from(glue, glue)
import::from(stringr, str_starts, str_extract)
import::from(vctrs, vec_size)
import::from(magrittr, `%>%`, `%T>%`)
import::from(fst, fst, write_fst)
import::from(lubridate, as_date)

# Create reduction function ####################################################
combine_report_dates <- function(.accumulated, .next, .pb = NULL) {
  dplyr::left_join(
    tibble::as_tibble(.accumulated),
    tibble::as_tibble(.next),
    by = "inv_local_id"
  ) %>%
    dplyr::transmute(
      inv_local_id,
      report_date = dplyr::coalesce(report_date.y, report_date.x)
    ) %T>%
    {if (!is.null(.pb)) .pb$tick()}
}

# Create progress bar object ###################################################
pb_create <- function(total) {
  progress_bar$new(
    format = glue(
      "Combining Report Dates :percent ",
      "[:bar] Estimated Time Remaining: :eta"
    ),
    total = total - 1,
    show_after = 0
  )
}

# Read and combine report dates ################################################
data_dir <- path(path_expand_r("~"), "report_dates", "data")

data_dir %>%
  dir_info() %>%
  filter(path %>% path_file() %>% str_starts("nbs_data_")) ->
data_files

pb_reduce <- pb_create(vec_size(data_files))

data_files %>%
  transmute(
    path,
    birth_date = path %>%
      path_file() %>%
      str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}") %>%
      as_date(),
    fst_data = map(path, ~ fst(.x))
  ) %>%
  arrange(desc(birth_date)) %>%
  select(fst_data, birth_date, path) %>%
  pull(fst_data) %>%
  reduce(combine_report_dates, .pb = pb_reduce) %>%
  mutate(report_date = as_date(report_date)) %>%
  arrange(desc(report_date)) %T>%
  show() ->
data_report_date

# Save report dates ############################################################

write_fst(
  data_report_date,
  path = path(data_dir, "report_dates.fst"),
  compress = 100
)

# Clean up
remove(combine_report_dates, pb_create, data_dir, data_files, pb_reduce, data_report_date)
