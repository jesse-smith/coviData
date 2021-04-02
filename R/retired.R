# | AEL ########################################################################

#' Add Name Columns to AEL Excel File
#'
#' `process_names` creates standardized `PatientLastName`,
#' `PatientFirstName`, `PatientMiddleName`, and
#' `PatientMidOthName` columns in AEL data, if they do not already exist.
#'
#' `process_names` first checks the input data for the above columns, as
#' well as any additional `PatientMidOthName` columns. If any of the
#' standardized columns do not exist, or non-standard \code{Patient...Name}
#' columns exist, the function deletes any previously created
#' \code{Patient...Name} columns and adds standardized ones using
#' \code{\link{split_names}}. If the data contains the standardized columns and
#' no additional `PatientMidOthName` columns, the data is returned as-is.
#'
#' @param data A data frame or tibble containing AEL data from one date
#'
#' @param force A logical; if `force = TRUE`, the data is (re-)processed
#'   regardless of whether it passes the above checks
#'
#' @return A tibble containing the input data plus standardized
#'   `PatientLastName`, `PatientFirstName`, `PatientMiddleName`,
#'   and `PatientMidOthName` columns
#'
#' @noRd
NULL
# process_names <- function(data, force = FALSE) {
#
#   # Get column names in data and set reference columns
#   col_names <- colnames(data)
#   ref_cols <- c(
#     "PatientLastName",
#     "PatientFirstName",
#     "PatientMiddleName",
#     "PatientMidOthName"
#   )
#
#   # Check that all standardized columns exist
#   ifelse(ref_cols %in% col_names, yes = TRUE, no = FALSE) %>%
#     all() ->
#     std_exist
#
#   # Check that no more than the standardized columns exist
#   stringr::str_detect(col_names, pattern = "Patient.+Name.*") %>%
#     sum() %>%
#     (function(x) x == 4) ->
#     only_std
#
#   # If the data passes both checks, return as-is (unless force == TRUE)
#   if (std_exist & only_std & !force) {
#     attr(data, which = "modified") <- FALSE
#     return(data)
#   }
#
#   # Find and delete any Patient...Name columns, then create standardized ones
#   # and return the result
#   data %>%
#     dplyr::select(-dplyr::matches("Patient.+Name.*")) %>%
#     split_names() ->
#     new_data
#
#   attr(new_data, which = "modified") <- TRUE
#
#   new_data
# }


#' Split Patient Names in AEL Data
#'
#' \code{split_names} creates columns for the last, first, first-middle, and
#' other-middle names of patients in AEL data
#'
#' @param .data A data frame or data frame extension (e.g. a
#'   \code{\link[tibble]{tibble}})
#'
#' @return The original data frame, tibble, etc. with character columns for
#'   `PatientLastName`, `PatientFirstName`, `PatientMiddleName`,
#'   `PatientMidOthName` inserted after `PatientName` (which is a
#'   \code{\link[base]{factor}})
#'
#' @noRd
NULL
# split_names <- function(.data) {
#   # Create character matrix holding last names and first + other names
#   .data$PatientName %>%
#     as.character() %>%
#     stringr::str_split_fixed(pattern = ",", n = 2) ->
#     pt_names
#
#   # Create character matrix holding first, middle, and other middle names
#   pt_names[, 2] %>%
#     stringr::str_split_fixed(
#       pattern = " ",
#       n = 3
#     ) ->
#     pt_other_names
#
#   # Create new columns
#   .data %>%
#     dplyr::transmute(
#       last = pt_names[, 1] %>%
#         as.vector() %>%
#         stringr::str_squish() %>%
#         gsub(pattern = "^$", replacement = NA),
#       first = pt_other_names[, 1] %>%
#         as.vector() %>%
#         stringr::str_squish() %>%
#         gsub(pattern = "^$", replacement = NA),
#       middle = pt_other_names[, 2] %>%
#         as.vector() %>%
#         stringr::str_squish() %>%
#         gsub(pattern = "^$", replacement = NA),
#       other = pt_other_names[, 3] %>%
#         as.vector() %>%
#         stringr::str_squish() %>%
#         gsub(pattern = "^$", replacement = NA)
#     ) ->
#     new_names
#
#   # Add to existing data
#   .data %>%
#     dplyr::mutate(
#       PatientLastName = new_names$last,
#       PatientFirstName = new_names$first,
#       PatientMiddleName = new_names$middle,
#       PatientMidOthName = new_names$other,
#       .after = "PatientName"
#     )
# }

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
#' @noRd
NULL
# replace_ael <- function(
#   date = Sys.Date(),
#   directory = "V:/EPI DATA ANALYTICS TEAM/AEL Data/",
#   date_file = NULL,
#   overwrite_names = FALSE
# ) {
#   date_str <- as.character(date, format = "%m/%d")
#
#   message(paste0("Reading file for ", date_str, "..."))
#
#   ael_file <- find_file(
#     date = date,
#     directory = directory
#   )
#
#   load_ael(date = date, directory = directory) %>%
#     standardize_dates() %>%
#     process_names(force = overwrite_names) ->
#     processed_data
#
#   modified <- attr(processed_data, which = "modified")
#
#   if (modified) {
#     message("Writing file for ", date_str, "...")
#     openxlsx::write.xlsx(
#       processed_data,
#       file = ael_file,
#       sheet = "Sheet1"
#     )
#   }
#
#   invisible(processed_data)
# }

#' Load AEL Data from Disk
#'
#' `load_ael()` loads AEL data from a file into a \code{\link[tibble]{tibble}}.
#' It is essentially a wrapper around \code{\link{load_data}} with defaults
#' specific to AEL data.
#'
#' @inherit load_data params return
#'
#' @noRd
NULL
# load_ael <- function(
#   date = Sys.Date(),
#   directory = "V:/EPI DATA ANALYTICS TEAM/AEL DATA/",
#   ext = "xlsx"
# ) {
#   message("Loading AEL file:\n")
#   load_data(
#     date = date,
#     directory = directory,
#     ext = ext[[1]],
#     pattern = paste0(".*", date, ".*", ext[[1]])
#   )
# }

#' Download AEL File from Serv-U SFTP Site
#'
#' \code{download_ael} finds and download the AEL data for the input date and
#' saves it to a folder of your choice. To make the best use of this function,
#' you should save your username and password for Serv-U in a ".Renviron" file;
#' see
#'
#' @param date The creation date of the Serv-U file
#'
#' @param usr The username for Serv-U; the default pulls from a ".Renviron" file
#'   and is the recommended setup
#'
#' @param pwd The password for Serv-U; the default pulls from a ".Renviron" file
#'   and is the recommended setup
#'
#' @param directory The directory in which the new file will be saved
#'
#' @noRd
NULL
# download_ael <- function(
#   date = Sys.Date(),
#   usr = Sys.getenv("sftp_usr"),
#   pwd = Sys.getenv("sftp_pwd"),
#   directory = "V:/EPI DATA ANALYTICS TEAM/AEL Data/"
# ) {
#   download_servu(
#     date = date,
#     usr = usr,
#     pwd = pwd,
#     remote_dir = "AEL",
#     local_dir = directory,
#     new_file  = NULL
#   )
# }
