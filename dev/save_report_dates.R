library(dplyr)
library(purrr)
library(fs)
import::from(glue, glue)
import::from(lubridate, as_date)
import::from(stringr, str_ends, str_extract)
import::from(coviData, coalesce_dupes)

# Set up directories ###########################################################

read_dir <- path(
  "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
  "Sandbox Data Pull Final/"
)

data_dir <- path(path_expand_r("~"), "report_dates", "data")
if (!dir_exists(data_dir)) dir_create(data_dir)

# Get data files ###############################################################

read_dir %>%
  dir_info() %>%
  filter(str_ends(path, ".csv")) %>%
  transmute(path, birth_date = as_date(birth_time)) %>%
  arrange(desc(birth_date)) %>%
  coalesce_dupes(birth_date, pre_sort = FALSE) %>%
  dplyr::mutate(
    save_as = path_create(
      data_dir,
      paste0("nbs_data_", birth_date),
      ext = "fst"
    )
  ) ->
read_files

data_dir %>%
  dir_ls() %>%
  fs::path_tidy() %>%
  dplyr::as_tibble() %>%
  dplyr::rename(path = value) %>%
  dplyr::mutate(
    path,
    birth_date = path %>%
      path_file() %>%
      str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}") %>%
      as_date()
  ) %>%
  arrange(desc(birth_date)) ->
data_files

walk_files <- anti_join(read_files, data_files, by = "birth_date")

# Function for converting NBS data to ".fst" (investigation ID only) ###########

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

# Apply `to_fst()` to `files` ##################################################

walk(
  transpose(walk_files),
  ~ to_fst(.x$path, .x$birth_date, .x$save_as)
)

# Clean up #####################################################################

remove(read_dir, data_dir, read_files, data_files, walk_files, to_fst)
