#' Add Updated Names Columns to AEL File and Replace Existing File
#'
#' @export
replace_ael <- function(
  date = Sys.Date(),
  directory = "V:/EPI DATA ANALYTICS TEAM/AEL Data/",
  date_file = NULL,
  overwrite_names = FALSE
) {
  date_str <- as.character(date, format = "%m/%d")

  message(paste0("Reading file for ", date_str, "..."))

  ael_file <- find_file(
    date = date,
    directory = directory
  )

  load_ael(date = date, directory = directory) %>%
    standardize_dates() %>%
    # dplyr::mutate(AuthDate = lubridate::as_date(AuthDate)) %>%
    process_names(force = overwrite_names) ->
    processed_data

  modified <- attr(processed_data, which = "modified")

  if (modified) {
    message("Writing file for ", date_str, "...")
    openxlsx::write.xlsx(
      processed_data,
      file = ael_file,
      sheet = "Sheet1"
    )
  }

  invisible(processed_data)
}

#' @export
replace_deaths_id <- function(
  file = "Working Copy Death  Epi.xlsx",
  directory = "V:/EPI DATA ANALYTICS TEAM/MORTALITY DATA/",
  id = NBS,
  save_as = paste0("sas_data/cleaned_surveillance_copy.xlsx")
) {

  # Convert id to symbol to use as data-variable
  id <- rlang::enexpr(id) %>% rlang::as_name() %>% rlang::sym()

  # Create path
  path <- directory %>%
    fs::path_real() %>%
    fs::path_split() %>%
    .[[1]] %>%
    append(file) %>%
    fs::path_join() %>%
    fs::path_tidy()

  # Read file
  readxl::read_excel(
    path = path,
    trim_ws = TRUE,
    guess_max = .Machine$integer.max %/% 100L
  ) %>%
    # Clean PSN Number
    dplyr::mutate(std_nbs_id = standardize_nbs_id(!!id), .after = !!id) ->
  data

  # Save
  if (save_as != FALSE) {
    s_path <- directory %>%
      fs::path_real() %>%
      fs::path_split() %>%
      .[[1]] %>%
      append(save_as) %>%
      fs::path_join() %>%
      fs::path_tidy()

    if (!fs::dir_exists(fs::path_dir(s_path))) {
      fs::dir_create(fs::path_dir(s_path))
    }

    openxlsx::write.xlsx(data, file = s_path)

    invisible(data)
  }

}
