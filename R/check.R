check_deaths <- function(
  surveillance_file = "Working Copy Death  Epi.xlsx",
  nbs_file = "NBSDeathLineList.xls",
  directory = "V:/EPI DATA ANALYTICS TEAM/MORTALITY DATA/",
  surveillance_file_id = NBS,
  nbs_file_id = PATIENT_LOCAL_ID,
  save = FALSE
) {

  # Make sure IDs are symbols
  surveillance_file_id <- rlang::enexpr(surveillance_file_id) %>%
    rlang::as_name() %>%
    rlang::sym()
  nbs_file_id <- rlang::enexpr(nbs_file_id) %>%
    rlang::as_name() %>%
    rlang::sym()

  # Create path to surveillance deaths file
  s_path <- directory %>%
    fs::path_expand() %>%
    fs::path_split() %>%
    .[[1]] %>%
    append(surveillance_file) %>%
    fs::path_join() %>%
    fs::path_tidy()

  # Create path to surveillance deaths copy file
  s_path_copy <- directory %>%
    fs::path_expand() %>%
    fs::path_split() %>%
    .[[1]] %>%
    append(
      paste0("surveillance_file_copy.", fs::path_ext(surveillance_file))
    ) %>%
    fs::path_join() %>%
    fs::path_tidy()

  # Create path to NBS deaths file
  n_path <- directory %>%
    fs::path_expand() %>%
    fs::path_split() %>%
    .[[1]] %>%
    append(nbs_file) %>%
    fs::path_join() %>%
    fs::path_tidy()

  # Create path to surveillance deaths file
  n_path_copy <- directory %>%
    fs::path_expand() %>%
    fs::path_split() %>%
    .[[1]] %>%
    append(
      paste0("nbs_file_copy.", fs::path_ext(nbs_file))
    ) %>%
    fs::path_join() %>%
    fs::path_tidy()

  # Create path to file with missing ids
  unmatched_ids_path <- directory %>%
    fs::path_expand() %>%
    fs::path_split() %>%
    .[[1]] %>%
    append("unmatched_nbs_ids.xlsx") %>%
    fs::path_join() %>%
    fs::path_tidy()

  # # Make copy of files to work around someone having it open
  # fs::file_copy(
  #   path = c(s_path, n_path),
  #   new_path = c(s_path_copy, n_path_copy)
  # )
  #
  # # Clean up on exit
  # on.exit(
  #   fs::file_delete(c(s_path_copy, n_path_copy)),
  #   add = TRUE
  # )

  # Read surveillance deaths file
  readxl::read_excel(
    path = s_path,
    trim_ws = TRUE,
    guess_max = .Machine$integer.max %/% 100L
  ) %>%
    # Clean PSN Number
    dplyr::mutate(std_nbs_id = standardize_nbs_id(!!surveillance_file_id)) ->
  surveillance_data

  # Try to read NBS file as an excel file
  nbs_data <- try(
    readxl::read_excel(
      path = n_path,
      trim_ws = TRUE,
      guess_max = .Machine$integer.max %/% 100L
    ),
    silent = TRUE
  )

  # If that fails, try reading as html
  if (!tibble::is_tibble(nbs_data)) {
    nbs_data <- read_nbs_deaths_as_html("V:/EPI DATA ANALYTICS TEAM/MORTALITY DATA/NBSDeathLineList.xls")
  }

  # Standardize NBS ID in NBS file
  nbs_data %>%
    dplyr::mutate(std_nbs_id = standardize_nbs_id(!!nbs_file_id)) ->
  nbs_data

  # Get entries in surveillance file not in NBS
  dplyr::anti_join(
    x = surveillance_data,
    y = nbs_data,
    by = "std_nbs_id"
  ) %>%
    dplyr::transmute(
      in_linelist = "surveillance",
      std_nbs_id,
      inv_case_status = as.character(`Case Status`),
      patient_last_name = as.character(`Last Name`),
      patient_first_name = as.character(`First Name`),
      patient_deceased_dt = lubridate::as_date(`Date of Death`)
    ) ->
  surveillance_only

  # Get entries in NBS not in surveillance file
  dplyr::anti_join(
    x = nbs_data,
    y = surveillance_data,
    by = c("std_nbs_id")
  ) %>%
    dplyr::transmute(
      in_linelist = "nbs",
      std_nbs_id,
      case_status = as.character(INV_CASE_STATUS),
      patient_last_name = as.character(PATIENT_LAST_NAME),
      patient_first_name = as.character(PATIENT_FIRST_NAME),
      patient_deceased_dt = lubridate::as_date(PATIENT_DECEASED_DT)
    ) ->
  unmatched_ids

  if (save) {
    openxlsx::write.xlsx(unmatched_ids, file = unmatched_ids_path)
  }

  unmatched_ids
}

read_nbs_deaths_as_html <- function(path) {
  xml2::read_html(path) %>%
    rvest::html_table() %>%
    .[[2]] %>%
    tibble::as_tibble()
}
