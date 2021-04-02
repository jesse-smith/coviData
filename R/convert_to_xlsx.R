#' @rdname convert_snapshot_to_xlsx
#'
#' @export
convert_nbs_snapshot <- function(
  date = Sys.Date(),
  directory = paste0(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
    "Sandbox data pull Final/"
  ),
  force = FALSE,
  open = FALSE
) {
  convert_snapshot_to_xlsx(
    date = date,
    directory = directory,
    force = force,
    open = open
  )
}

#' @rdname convert_snapshot_to_xlsx
#'
#' @export
convert_pcr_snapshot <- function(
  date = Sys.Date(),
  directory = "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/MSR PCR/",
  force = FALSE,
  open = FALSE
) {
  convert_snapshot_to_xlsx(
    date = date,
    date_format = "%m%d%Y",
    directory = directory,
    force = force,
    open = open
  )
}

#' @rdname convert_snapshot_to_xlsx
#'
#' @export
convert_vaccine_snapshot <- function(
  date = Sys.Date(),
  directory = paste0(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
    "COVID-19 Vaccine Reporting/data/COVID-19 Vaccine data pull/"
  ),
  force = FALSE,
  open = FALSE
) {
  convert_snapshot_to_xlsx(
    date = date,
    date_format = "%Y%m%d",
    directory = directory,
    force = force,
    open = open
  )
}

#' Find a Snapshot File and Convert to XLSX Format
#'
#' `convert_snapshot_to_xlsx()` finds a snapshot file in `directory` with `date`
#' in its name and converts it to an XLSX file of the same name. See
#' \code{\link{convert_to_xlsx}} for details.
#'
#' The `convert_*_snapshot()` functions make it easier to convert specific data
#' files.
#'
#' @param date The date included in the name of the file to convert
#'
#' @param date_format The format of the date in the file name. The default for
#'   the generic function is 'YYYY-MM-DD'; see \code{\link[base]{strptime}} for
#'   details on format specification and the body of `convert_pcr_snapshot()`
#'   for an implementation example (in 'MMDDYYYY' format).
#'
#' @param directory The directory in which to look for the file to convert
#'
#' @param force Should the file be converted if it will overwrite an existing
#'   XLSX file?
#'
#' @param open Should the XLSX file be opened after conversion?
#'
#' @return A tidy path to the new XLSX file (invisibly)
#'
#' @aliases convert_nbs_snapshot convert_pcr_snapshot convert_vaccine_snapshot
#'
#' @export
convert_snapshot_to_xlsx <- function(
  date = Sys.Date(),
  date_format = "%Y-%m-%d",
  directory,
  force = FALSE,
  open = FALSE
) {

  formatted_date <- format(date, date_format)

  # Get matching files from directory
  to_convert <- find_file(
    date = date,
    pattern = paste0(".*", formatted_date, ".*"),
    directory = directory,
    rtn_error = FALSE
  )

  existing <- find_file(
    date = date,
    pattern = paste0(".*", formatted_date, ".*[.]xls.*"),
    directory = directory,
    rtn_error = FALSE
  )

  # Don't run if any are found
  if (vec_size(existing) != 0 & !force) {
    msg = paste(
      "An existing file matches this date; conversion will not continue.",
      "To convert anyway, set 'force == TRUE'."
    )

    rlang::abort(msg)
  }

  # Convert

  xlsx_file <- create_path(to_convert[[1]], ext = ".xlsx")

  Sys.sleep(3)

  rlang::inform("Converting to xlsx...")

  convert_to_xlsx(file = to_convert[[1]], xlsx_file = xlsx_file)

  rlang::inform("Done.")

  if (open) openxlsx::openXL(xlsx_file)

  invisible(xlsx_file)
}

#' Convert a File to XLSX Format Natively in Excel
#'
#' `convert_to_xlsx()` converts a given file to XLSX format using Excel's
#' "Save As" functionality. This replicates the output you'd get if you opened
#' the file and saved it manually.
#'
#' This function solves the inverse problem of Excel's (in)famous changing of
#' data types; if your pipeline was built around Excel, you actually need to
#' consistently treat the data as Excel would, whether working interactively in
#' Excel or programmatically in R. There's no way of doing this natively in
#' \strong{R} (or any other language); `convert_to_xlsx()` creates and runs a
#' temporary VBA script that interfaces with Excel programmatically.
#'
#' If you're starting a data pipeline from scratch, please don't use
#' this function. Just avoid Excel.
#'
#' @param file The path to a file readable by Excel
#'
#' @param xlsx_file The path to the new xlsx file. The default only changes the
#'   extension of `file` to '.xlsx'. \emph{Note: if `xlsx_file` does not end in
#'   '.xlsx', the current extension (if any) will be dropped and '.xlsx' will
#'   be appended to the path.}
#'
#' @return `xlsx_file`, as a tidy path (invisibly)
#'
#' @keywords internal
convert_to_xlsx <- function(file, xlsx_file = NULL) {

  if (!is_windows()) {
    msg <- paste0(
      "`convert_to_xlsx()` currently only works on Windows",
      " due to VB dependencies"
    )

    rlang::abort(msg)
  }

  file <- file %>%
    create_path() %>%
    stringr::str_replace_all(pattern = "/", replacement = "\\\\")

  xlsx_file <- create_path(xlsx_file, ext = ".xlsx")

  xlsx_file_vba <- xlsx_file %>%
    stringr::str_replace_all(pattern = "/", replacement = "\\\\")

  temp_script <- fs::file_temp("script_", ext = "vbs")

  on.exit(fs::file_delete(temp_script), add = TRUE)

  c(
    paste0("With CreateObject(\"Excel.Application\")"),
    paste0("Set WB = .Workbooks.Open(\"", file, "\")"),
    paste0("WB.SaveAs \"", xlsx_file_vba, "\", 51"),
    paste0(".Quit"),
    paste0("End With")
  ) %>%
    readr::write_lines(file = temp_script)

  utils::capture.output(system2("cscript.exe", temp_script)) %>%
    stringr::str_remove_all("^\\[[0-9]+\\]") %>%
    stringr::str_flatten("\n") %>%
    rlang::inform(class = "cmd_output")

  invisible(xlsx_file)
}
