#' Add Updated Names Columns to AEL File and Replace Existing File
#'
#' Function to replace AEL data with cleaned names data.
#'
#' @param date The date of the file to replace
#'
#' @param directory The directory in which the file is located
#'
#' @param date_file IDK
#'
#' @param overwrite_names If names are already cleaned, should this be
#'   overwritten?
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

#' Replace NBS ID in Surveillance Deaths Linelist with Standardized ID
#'
#' `replace_deaths_id()` facilitates comparison of the surveillance deaths
#' linelist with the one exported from NBS by standardizing patient IDs. It
#' saves the result in the file specified in `save_as`.
#'
#' @param file The name of the surveillance linelist file
#'
#' @param directory The directory containing the linelist
#'
#' @param id The name of the column specifying the NBS ID in the linelist; if
#'   `NULL`, assumes that this is the first column
#'
#' @param save_as The file path specifying where to save the results
#'
#' @return The cleaned data as a \code{\link[tibble]{tibble}} (invisibly)
replace_deaths_id <- function(
  file = "Working Copy Death  Epi.xlsx",
  directory = "V:/EPI DATA ANALYTICS TEAM/MORTALITY DATA/",
  id = NULL,
  save_as = paste0("sas_data/cleaned_surveillance_copy.xlsx")
) {

  # Create path
  path <- create_path(directory, file)

  # Read file
  surveillance_data <- readxl::read_excel(
    path = path,
    trim_ws = TRUE,
    sheet = "Sheet 1",
    guess_max = .Machine$integer.max %/% 100L
  )

  # Get `id` if NULL
  if (rlang::quo_is_null(rlang::enquo(id))) {
    id <- colnames(surveillance_data[1]) %>% rlang::sym()
  } else {
    # Convert to symbol
    id <- rlang::enexpr(id) %>% rlang::as_name() %>% rlang::sym()
  }

  surveillance_data %>%
    # Clean PSN Number
    dplyr::mutate(std_nbs_id = standardize_nbs_id(!!id), .after = !!id) ->
  data

  # Save
  if (save_as != FALSE) {
    s_path <- create_path(directory, save_as)

    if (!fs::dir_exists(fs::path_dir(s_path))) {
      fs::dir_create(fs::path_dir(s_path))
    }

    openxlsx::write.xlsx(data, file = s_path)

    invisible(data)
  }

}
