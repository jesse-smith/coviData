library(dplyr)
library(purrr)
library(fs)
import::from(lubridate, as_date)
import::from(stringr, str_ends)
import::from(coviData, coalesce_dupes)

# write_dir <- file_temp("data", tmp_dir = path_expand_r("~"))
write_dir <- path(path_expand_r("~"), "data686010e4fc8")
# dir_create(write_dir)

read_dir <- path(
  "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
  "Sandbox Data Pull Final/"
)

read_dir %>%
  dir_info() %>%
  filter(str_ends(path, ".csv")) %>%
  transmute(path, birth_date = as_date(birth_time)) %>%
  arrange(desc(birth_date)) %>%
  coalesce_dupes(birth_date, pre_sort = FALSE) %>%
  mutate(save_as = path(write_dir, paste0("nbs_data_", birth_date, ".fst"))) ->
files

files_remaining1 <- filter(files, birth_date < "2020-07-06")

to_fst <- function(path, birth_date, save_as) {
  vroom::vroom(
    file = path,
    col_select = INV_LOCAL_ID,
    col_types = vroom::cols(INV_LOCAL_ID = vroom::col_character())
  ) %>%
    janitor::clean_names() %>%
    dplyr::mutate(report_date = birth_date) %>%
    fst::write_fst(path = save_as, compress = 100L)
}

walk(
  transpose(files_remaining1),
  ~ to_fst(.x$path, .x$birth_date, .x$save_as)
)

left_join(x, y, "inv_local_id") %>%
  transmute(
    inv_local_id,
    report_date = coalesce(report_date.y, report_date.x)
  )

load_next <- function(path, birth_date) {
  vroom::vroom(
    file = path,
    col_select = INV_LOCAL_ID,
    col_types = cols(INV_LOCAL_ID = col_character())
  ) %>%
    janitor::clean_names() %>%
    mutate(report_date = birth_date)
}

combine_data <- function(.data, path, birth_date) {
  dplyr::left_join(
    .data,
    load_next(path, birth_date),
    by = "inv_local_id"
  ) %>%
    transmute(
      inv_local_id,
      report_date = coalesce(report_date.y, report_date.x)
    )
}

# Read file using vroom and process with dplyr
# - Set `id = "path"` to get report date from file name
# - Read only `INV_LOCAL_ID` and `SPECIMEN_COLL_DT`; set types as character and
#   date
# - Parse `path` to get report date

# Create empty tibble `data` with `INV_LOCAL_ID` and `REPORT_DATE` variables

# Read file `new_data`

# Right join data and new_data on inv_local_id

# Coalesce report dates

# Save to data

# Iterate

