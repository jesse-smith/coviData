#' Add Updated Names Columns to AEL File and Replace Existing File
#'
#' @export
replace_ael <- function(
  date = Sys.Date(),
  directory = "V:/EPI DATA ANALYTICS TEAM/AEL Data/",
  date_file = NULL,
  overwrite_names = FALSE,
  encoding = TRUE
) {
  date_str <- as.character(date, format = "%m/%d")

  message(paste0("Reading file for ", date_str, "..."))

  ael_file <- find_file(
    date = date,
    directory = directory
  )

  ael_file %>%
    read_ael(string_to_factor = FALSE, encoding = encoding) %>%
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
  save_as = paste0("cleaned_copies/cleaned_surveillance_copy", Sys.Date(), ".xlsx")
) {

  # Convert id to symbol to use as data-variable
  id <- rlang::enexpr(id) %>% rlang::as_name() %>% rlang::sym()

  # Create path
  path <- directory %>%
    fs::path_real() %>%
    fs::path_split() %>%
    .[[1]] %>%
    append(file) %>%
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
      fs::path_tidy()

    openxlsx::write.xlsx(data, file = s_path)

    invisible(data)
  }
}
