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

    openxlsx::write.xlsx(data, file = s_path, overwrite = TRUE)

    invisible(data)
  }

}
