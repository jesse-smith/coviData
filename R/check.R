#' Compare Linelists from NBS and Surveillance Team and Report Differences
#'
#' @param surveillance_file The name of the file containing the surveillance
#'   team linelist
#'
#' @param nbs_file The name of the file containing the NBS exported linelist
#'
#' @param directory The path to the directory containing both linelists
#'
#' @param surveillance_file_id The NBS ID column in the surveillance file. If
#'   `NULL`, `check_deaths()` assumes that this is the first column in the file.
#'
#' @param nbs_file_id The NBS ID column in the NBS linelist.
#'
#' @param save Should the results be saved to a file?
#'
#' @export
check_deaths <- function(
  surveillance_file = path_deaths("surv"),
  nbs_file = path_deaths("nbs"),
  directory = "V:/EPI DATA ANALYTICS TEAM/MORTALITY DATA/",
  surveillance_file_id = NULL,
  nbs_file_id = PATIENT_LOCAL_ID,
  save = FALSE
) {

  # Create path to surveillance deaths file
  s_path <- create_path(surveillance_file)

  # Create path to NBS deaths file
  n_path <- create_path(nbs_file)

  # Create path to file with missing ids
  unmatched_dir  <- create_path(directory, "Missing IDs/")
  unmatched_name <- paste0("missing_ids_", Sys.Date(), ".xlsx")
  unmatched_ids_path <- create_path(unmatched_dir, unmatched_name)

  # Read surveillance deaths file
  surveillance_data <- readxl::read_excel(
    path = s_path,
    trim_ws = TRUE,
    sheet = "Sheet 1",
    guess_max = .Machine$integer.max %/% 100L
  )

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

  # Make sure IDs are symbols
  if (rlang::quo_is_null(rlang::enquo(surveillance_file_id))) {
    surveillance_file_id <- colnames(surveillance_data[1]) %>% rlang::sym()
  } else {
    surveillance_file_id <- rlang::ensym(surveillance_file_id)
  }

  if (rlang::quo_is_null(rlang::enquo(nbs_file_id))) {
    nbs_file_id <- colnames(nbs_data[1L]) %>% rlang::sym()
  } else {
    nbs_file_id <- rlang::ensym(nbs_file_id)
  }

  # Standardize NBS ID in surveillance file
  surveillance_data <- dplyr::mutate(
    surveillance_data,
    std_nbs_id = standardize_nbs_id(!!surveillance_file_id)
  )

  # Standardize NBS ID in NBS file
  nbs_data <- dplyr::mutate(
    nbs_data,
    std_nbs_id = standardize_nbs_id(!!nbs_file_id)
  )

  # Get entries in surveillance file not in NBS
  surveillance_only <- dplyr::anti_join(
    x = surveillance_data,
    y = nbs_data,
    by = "std_nbs_id"
  ) %>%
    dplyr::transmute(
      in_linelist = "surveillance",
      .data[["std_nbs_id"]],
      !!surveillance_file_id,
      !!nbs_file_id := NA_character_,
      inv_case_status = as.character(.data[["Case Status"]]),
      patient_last_name = as.character(.data[["Last Name"]]),
      patient_first_name = as.character(.data[["First Name"]]),
      patient_deceased_dt = lubridate::as_date(.data[["Date of Death"]])
    )

  # Get entries in NBS not in surveillance file
  nbs_only <- dplyr::anti_join(
    x = nbs_data,
    y = surveillance_data,
    by = c("std_nbs_id")
  ) %>%
    dplyr::transmute(
      in_linelist = "nbs",
      .data[["std_nbs_id"]],
      !!surveillance_file_id := NA_character_,
      !!nbs_file_id,
      inv_case_status = as.character(.data[["INV_CASE_STATUS"]]),
      patient_last_name = as.character(.data[["PATIENT_LAST_NAME"]]),
      patient_first_name = as.character(.data[["PATIENT_FIRST_NAME"]]),
      patient_deceased_dt = lubridate::as_date(.data[["PATIENT_DECEASED_DT"]])
    )

  nbs_only %>%
    dplyr::add_row(surveillance_only) %>%
    dplyr::mutate(
      original_nbs_id = dplyr::coalesce(!!surveillance_file_id, !!nbs_file_id)
    ) %>%
    dplyr::relocate("in_linelist", "original_nbs_id", .before = 1L) %>%
    dplyr::select(-!!surveillance_file_id, -!!nbs_file_id) ->
  unmatched_ids

  if (save) {
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



#' Check and Clean Dates in a Linelist
#'
#' `check_linelist_dates()` checks a linelist with collection and report dates
#' for errors and logical inconsistencies. It removes observations that fail
#' these tests.
#'
#' @param .data A linelist with one case/test per row
#'
#' @param .collection_date A `Date` column corresponding to the date of sample
#'   collection
#'
#' @param .report_date A `Date` column corresponding to the date the sample
#'   was reported
#'
#' @param today The `Date` to consider as "today"
#'
#' @param quiet Should messages be shown for each checking and removal step?
#'
#' @return The checked and cleaned linelist
#'
#' @export
check_linelist_dates <- function(
  .data,
  .collection_date = "collection_date",
  .report_date = "report_date",
  today = Sys.Date(),
  quiet = FALSE
) {

  collect_nm <- select_colnames(.data, .collection_date)
  report_nm <- select_colnames(.data, .report_date)

  assert_cols(.data, collect_nm, ptype = lubridate::Date(), n = 1L)
  assert_cols(.data, report_nm, ptype = lubridate::Date(), n = 1L)

  .data %T>%
    {if (!quiet) rlang::inform("Removing invalid dates")} %>%
    tidylog::filter(
      .data[[collect_nm]] >= as.Date("2020-03-05"),
      .data[[collect_nm]] <= today,
      .data[[report_nm]] >= as.Date("2020-03-05"),
      .data[[report_nm]] <= today
    ) %T>%
    {if (!quiet) rlang::inform("Removing illogical dates combinations")} %>%
    tidylog::filter(.data[[collect_nm]] <= .data[[report_nm]])
}

#' Check Two AEL Data Files for New or Removed Results
#'
#' `check_ael` reads two AEL data files and checks for results in each that are
#' not in the other. This is primarily useful if the files are sequential.
#'
#' @param .data If data has already been read into R, supply here (not used yet)
#'
#' @param date1 The `Date` corresponding to the first file
#'
#' @param date2 The `Date` corresponding to the second file
#'
#' @param directory The directory to look for AEL data in
#'
#' @param date1_file Optionally, the name of the first file; useful if searching
#'   for a file with non-standard naming
#'
#' @param date2_file Optionally, the name of the second file; useful if
#'   searching for a file with non-standard naming
#'
#' @param overwrite_names_1 Should the patient names in the first file be
#'   pre-processed with \code{\link{process_names}}?
#'
#' @param overwrite_names_2 Should the patient names in the second file be
#'   pre-processed with \code{\link{process_names}}?
#'
#' @param encoding Should character encoding be converted to \code{UFT-8}?
#'
#' @param rtn_table Should a \code{\link[gt]{gt}} table be returned for the
#'   first file?
#'
#' @noRd
NULL
# check_ael <- function(
#   .data = NULL,
#   date1 = Sys.Date(),
#   date2 = date1 - 1,
#   directory = "V:/EPI DATA ANALYTICS TEAM/AEL Data/",
#   date1_file = NULL,
#   date2_file = NULL,
#   overwrite_names_1 = FALSE,
#   overwrite_names_2 = FALSE,
#   encoding = TRUE,
#   rtn_table = TRUE
# ) {
#
#   # If no data is passed, get AEL data
#   if (is.null(.data)) {
#
#     # Create shorter date labels (MM/DD)
#     date1_str <- as.character(date1, format = "%m/%d")
#     date2_str <- as.character(date2, format = "%m/%d")
#
#     # Get path to date1 file, read data, and process name columns
#     message(paste0("Reading file for ", date1_str, "..."))
#     load_ael(date1, directory = directory) %>%
#       dplyr::mutate(
#         dplyr::across(where(lubridate::is.POSIXt), dttm_to_dt)
#       ) %>%
#       dplyr::mutate(
#         dplyr::across(
#           where(is.character),
#           .fns = stringr::str_conv,
#           encoding = "UTF-8"
#         )
#       ) %>%
#       dplyr::filter(PtState %in% c("TN", NA, "")) %>%
#       process_names(force = overwrite_names_1) %>%
#       # Add label for the date of the file
#       dplyr::mutate(FileDate = date1_str) ->
#       date1_data
#
#     # Get path to date1 file, read data, and process name columns
#     message(paste0("Reading file for ", date2_str, "..."))
#     load_ael(date2, directory = directory) %>%
#       dplyr::mutate(
#         dplyr::across(where(lubridate::is.POSIXt), dttm_to_dt)
#       ) %>%
#       dplyr::mutate(
#         dplyr::across(
#           where(is.character),
#           .fns = stringr::str_conv,
#           encoding = "UTF-8"
#         )
#       ) %>%
#       dplyr::filter(PtState %in% c("TN", NA, "")) %>%
#       process_names(force = overwrite_names_2) %>%
#       # Add label for the date of the file
#       dplyr::mutate(FileDate = date2_str) ->
#       date2_data
#
#     larger_date <- if (date1 > date2) date1_str else date2_str
#     smaller_date <- if (date1 > date2) date2_str else date1_str
#
#     # Bind to get combined data
#     dplyr::bind_rows(date1_data, date2_data) %>%
#       dplyr::arrange(FileDate, AuthDate) ->
#       .data
#   }
#
#   message("Summarizing data...")
#
#   .data %>%
#     # Filter to only two dates of interest
#     dplyr::filter(AuthDate %in% c(date1, date2)) %>%
#     # Make factors from dates
#     dplyr::mutate(
#       FileDate = factor(
#         FileDate,
#         levels = c("New Todate", smaller_date, larger_date)),
#       AuthDate = AuthDate %>%
#         as.character(format = "%m/%d") %>%
#         factor(levels = c("New Todate", smaller_date, larger_date)),
#       Result = Result %>%
#         as.character() %>%
#         stringr::str_to_title() %>%
#         factor(
#           levels = c(
#             "Positive",
#             "Presumptive Positive",
#             "Negative",
#             "Indeterminate"
#           )
#         ) %>%
#         addNA()
#     ) ->
#     .data
#
#   .data %>%
#     # Count number of tests in each category
#     dplyr::count(FileDate, AuthDate, Result, name = "Count") %>%
#     dplyr::ungroup() %>%
#     # Re-arrange
#     dplyr::filter(!(FileDate == smaller_date & AuthDate == larger_date)) ->
#     summary_all
#
#   # Deduplicated summary
#   .data %>%
#     dplyr::arrange(FileDate, AuthDate, Result, EpisodeNo) %>%
#     dplyr::distinct(EpisodeNo, .keep_all = TRUE) %>%
#     # Count number of tests in each category
#     dplyr::filter(FileDate == larger_date) %>%
#     dplyr::count(FileDate, Result, name = "Count") %>%
#     dplyr::ungroup() %>%
#     dplyr::mutate(AuthDate = "New Todate") ->
#     summary_distinct
#
#   summary_all %>%
#     dplyr::add_row(summary_distinct) %>%
#     dplyr::arrange(dplyr::desc(FileDate), dplyr::desc(AuthDate), Result) ->
#     summary_data
#
#   message("Done.")
#
#   # Look at number in yesterdate's file that aren't in todate's
#   suppressWarnings(
#     .data %>%
#       dplyr::arrange(desc(FileDate), AuthDate, Result, EpisodeNo) %>%
#       dplyr::distinct(EpisodeNo, .keep_all = TRUE) %>%
#       # Count number of tests in each category
#       dplyr::filter(FileDate == smaller_date) %>%
#       janitor::tabyl(Result) %>%
#       janitor::adorn_totals() %>%
#       janitor::adorn_title(col_name = "Gone Todate") %>%
#       methods::show()
#   )
#
#   if (rtn_table) {
#     summary_data %>%
#       gt::gt() %>%
#       gt::data_color(
#         columns = dplyr::vars(FileDate, AuthDate),
#         colors = c("#C3DCFF", "#ACC1FF", "#96A7F7")
#       ) %>%
#       gt::data_color(
#         columns = dplyr::vars(Result),
#         colors = c("red", "pink", "green", "orange", "grey"),
#         alpha = 0.5
#       ) %>%
#       gt::tab_header(title = "Test Counts by date and Result") %>%
#       methods::show()
#
#     invisible(summary_data)
#   } else {
#     summary_data
#   }
# }
