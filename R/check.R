#' @export
check_ael <- function(
  .data = NULL,
  date1 = Sys.Date(),
  date2 = date1 - 1,
  directory = "V:/EPI DATA ANALYTICS TEAM/AEL Data/",
  date1_file = NULL,
  date2_file = NULL,
  overwrite_names_1 = FALSE,
  overwrite_names_2 = FALSE,
  encoding = TRUE,
  rtn_table = TRUE
) {

  # If no data is passed, get AEL data
  if (is.null(.data)) {

    # Create shorter date labels (MM/DD)
    date1_str <- as.character(date1, format = "%m/%d")
    date2_str <- as.character(date2, format = "%m/%d")

    # Get path to date1 file, read data, and process name columns
    message(paste0("Reading file for ", date1_str, "..."))
    load_ael(date1, directory = directory) %>%
      dplyr::mutate(
        dplyr::across(where(lubridate::is.POSIXt), dttm_to_dt)
      ) %>%
      dplyr::mutate(
        dplyr::across(
          where(is.character),
          .fns = stringr::str_conv,
          encoding = "UTF-8"
        )
      ) %>%
      dplyr::filter(PtState %in% c("TN", NA, "")) %>%
      process_names(force = overwrite_names_1) %>%
      # Add label for the date of the file
      dplyr::mutate(FileDate = date1_str) ->
      date1_data

    # Get path to date1 file, read data, and process name columns
    message(paste0("Reading file for ", date2_str, "..."))
    load_ael(date2, directory = directory) %>%
      dplyr::mutate(
        dplyr::across(where(lubridate::is.POSIXt), dttm_to_dt)
      ) %>%
      dplyr::mutate(
        dplyr::across(
          where(is.character),
          .fns = stringr::str_conv,
          encoding = "UTF-8"
        )
      ) %>%
      dplyr::filter(PtState %in% c("TN", NA, "")) %>%
      process_names(force = overwrite_names_2) %>%
      # Add label for the date of the file
      dplyr::mutate(FileDate = date2_str) ->
      date2_data

    larger_date <- if (date1 > date2) date1_str else date2_str
    smaller_date <- if (date1 > date2) date2_str else date1_str

    # Bind to get combined data
    dplyr::bind_rows(date1_data, date2_data) %>%
      dplyr::arrange(FileDate, AuthDate) ->
      .data
  }

  message("Summarizing data...")

  .data %>%
    # Filter to only two dates of interest
    dplyr::filter(AuthDate %in% c(date1, date2)) %>%
    # Make factors from dates
    dplyr::mutate(
      FileDate = factor(
        FileDate,
        levels = c("New Todate", smaller_date, larger_date)),
      AuthDate = AuthDate %>%
        as.character(format = "%m/%d") %>%
        factor(levels = c("New Todate", smaller_date,larger_date)),
      Result = Result %>%
        as.character() %>%
        stringr::str_to_title() %>%
        factor(
          levels = c(
            "Positive",
            "Presumptive Positive",
            "Negative",
            "Indeterminate"
          )
        ) %>%
        addNA()
    ) ->
    .data

  .data %>%
    # Count number of tests in each category
    dplyr::count(FileDate, AuthDate, Result, name = "Count") %>%
    dplyr::ungroup() %>%
    # Re-arrange
    dplyr::filter(!(FileDate == smaller_date & AuthDate == larger_date)) ->
    summary_all

  # Deduplicated summary
  .data %>%
    dplyr::arrange(FileDate, AuthDate, Result, EpisodeNo) %>%
    dplyr::distinct(EpisodeNo, .keep_all = TRUE) %>%
    # Count number of tests in each category
    dplyr::filter(FileDate == larger_date) %>%
    dplyr::count(FileDate, Result, name = "Count") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(AuthDate = "New Todate") ->
    summary_distinct

  summary_all %>%
    dplyr::add_row(summary_distinct) %>%
    dplyr::arrange(dplyr::desc(FileDate), dplyr::desc(AuthDate), Result) ->
    summary_data

  message("Done.")

  # Look at number in yesterdate's file that aren't in todate's
  suppressWarnings(
    .data %>%
      dplyr::arrange(desc(FileDate), AuthDate, Result, EpisodeNo) %>%
      dplyr::distinct(EpisodeNo, .keep_all = TRUE) %>%
      # Count number of tests in each category
      dplyr::filter(FileDate == smaller_date) %>%
      janitor::tabyl(Result) %>%
      janitor::adorn_totals() %>%
      janitor::adorn_title(col_name = "Gone Todate") %>%
      methods::show()
  )

  if (rtn_table) {
    summary_data %>%
      gt::gt() %>%
      gt::data_color(
        columns = dplyr::vars(FileDate, AuthDate),
        colors = c("#C3DCFF", "#ACC1FF", "#96A7F7")
      ) %>%
      gt::data_color(
        columns = dplyr::vars(Result),
        colors = c("red", "pink", "green", "orange", "grey"),
        alpha = 0.5
      ) %>%
      gt::tab_header(title = "Test Counts by date and Result") %>%
      methods::show()

    invisible(summary_data)
  } else {
    summary_data
  }
}

#' @export
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

  # Create path to NBS deaths file
  n_path <- directory %>%
    fs::path_expand() %>%
    fs::path_split() %>%
    .[[1]] %>%
    append(nbs_file) %>%
    fs::path_join() %>%
    fs::path_tidy()

  # Create path to file with missing ids
  unmatched_ids_path <- directory %>%
    fs::path_expand() %>%
    fs::path_split() %>%
    .[[1]] %>%
    append("Missing IDs.xlsx") %>%
    fs::path_join() %>%
    fs::path_tidy()

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
  if (!is.data.frame(nbs_data)) {
    nbs_data <- load_nbs_deaths_as_html(n_path)
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
      !!surveillance_file_id,
      !!nbs_file_id := NA_character_,
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
      !!surveillance_file_id := NA_character_,
      !!nbs_file_id,
      inv_case_status = as.character(INV_CASE_STATUS),
      patient_last_name = as.character(PATIENT_LAST_NAME),
      patient_first_name = as.character(PATIENT_FIRST_NAME),
      patient_deceased_dt = lubridate::as_date(PATIENT_DECEASED_DT)
    ) ->
  nbs_only

  nbs_only %>%
    dplyr::add_row(surveillance_only) %>%
    dplyr::mutate(
      original_nbs_id = dplyr::coalesce(!!surveillance_file_id, !!nbs_file_id)
    ) %>%
    dplyr::select(in_linelist, original_nbs_id, dplyr::everything()) %>%
    dplyr::select(-!!surveillance_file_id, -!!nbs_file_id) ->
  unmatched_ids

  if (save) {

    if (is_open(unmatched_ids_path)) {
      ext <- fs::path_ext(unmatched_ids_path)
      unmatched_ids_path %<>%
        fs::path_ext_remove() %>%
        paste0(" - temp (please copy this data to original file)") %>%
        fs::path_ext_set(ext = ext)
    }
    openxlsx::write.xlsx(unmatched_ids, file = unmatched_ids_path)
  }

  list(
    surveillance = surveillance_data,
    nbs = nbs_data,
    surveillance_only = surveillance_only,
    nbs_only = nbs_only,
    unmatched = unmatched_ids
  )
}
