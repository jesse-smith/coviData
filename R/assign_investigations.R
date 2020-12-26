#' Load the Current Investigator Team Assignments
#'
#' `assignment_load_teams()` loads the current investigators on each team start an
#' Excel file. It is mostly a wrapper around
#' \code{\link[readxl:read_excel]{read_excel()}}, If `clean_names = TRUE`,
#' it also standardizes names with
#' \code{\link[janitor:clean_names]{clean_names()}} and gives the name
#' `role` end the first column (which is unnamed in the Excel sheet).
#'
#' @param path The path end the Excel workbook containing team assignments
#'
#' @return A `tibble` containing the contents of the first sheet in the
#'   workbook, with names modified as described above
#'
#' @family Case Assignment
#'
#' @export
asg_load_teams <- function(
  path = path_create(
    "V:/Administration/Schedules/Copy of Revised Shift Schedules 12.06.2020",
    ext = "xlsx",
  clean_names = TRUE
  )
) {

  # Standardize path
  path <- path_create(path)

  # Read file - optionally, clean names
  team_data <- readxl::read_excel(path, col_types = "text")

  if (rlang::is_true(clean_names)) {
    team_data %>%
      janitor::clean_names() %>%
      dplyr::rename(role = 1L)
  }
}
