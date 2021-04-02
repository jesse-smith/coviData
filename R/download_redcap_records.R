#' Download New Interview Tool Data
#'
#' @description
#' `download_nit()` downloads records from the New Interview Tool (NIT) project
#'
#' `download_nca()` downloads records from the New Case Assignment (NCA) project
#'
#' @inherit download_redcap_records params return
#'
#' @name download-case-projects
#'
#' @aliases download_nit download_nca
#'
#' @examples
#' \dontrun{
#'   # Download the current NIT data
#'   download_nit()
#'
#'   # Download NIT and load immediately
#'   nit_data <- load_nit(download_nit())
#'
#'   # Do the same with pipe (`%>%`)
#'   nit_data <- download_nit() %>% load_nit()
#'
#'   # Overwrite the previous file
#'   download_nit(force = TRUE)
#'
#'   # Download NCA data
#'   download_nca()
#'
#'   # Download NCA and overwrite previous file
#'   nca_data <- download_nca() %>% load_nca()
#' }
NULL

#' @rdname download-case-projects
#'
#' @export
download_nit <- function(
  api_token = Sys.getenv("redcap_NIT_token"),
  headers = c("raw", "label"),
  values = c("raw", "label"),
  filter = NULL,
  dir = "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/Data for R/nit/",
  file = paste0("nit_data_", Sys.Date(), ".csv"),
  force = FALSE
) {
  download_redcap_records(
    api_token = api_token,
    headers = headers,
    values = values,
    filter = filter,
    dir = dir,
    file = file,
    force = force
  )
}

#' @rdname download-case-projects
#'
#' @export
download_nca <- function(
  api_token = Sys.getenv("redcap_NCA_token"),
  headers = c("raw", "label"),
  values = c("raw", "label"),
  filter = NULL,
  dir = "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/Data for R/nca/",
  file = paste0("nca_data_", Sys.Date(), ".csv"),
  force = FALSE
) {
  download_redcap_records(
    api_token = api_token,
    headers = headers,
    values = values,
    filter = filter,
    dir = dir,
    file = file,
    force = force
  )
}


#' Download New Interview Tool Data
#'
#' @description
#' `download_redcap_records()` downloads records from a REDcap project
#'
#' @param api_token `character`. API token/key for accessing the project
#'   programmatically. Best practice is to set as an environment variable and
#'   retrieve using `Sys.getenv("token_environment_variable")`. See
#'   \code{\link{env-variables}} for more information.
#'
#' @param headers `"raw"` or `"label"`. Should column names ("headers") be
#'   exported as the raw variable names (`"raw"`, the default) or the label
#'   shown in the UI (`"label"`)?
#'
#' @param values `"raw"` or `"label"`. Should values in multiple-choice and
#'   checkbox fields be exported as the raw coded value (`"raw"`, the default)
#'   or the label for the coded value (`"label"`)?
#'
#' @param filter `character`. REDcap filtering logic to apply prior to download.
#'   This must be in the REDcap logic syntax.
#'
#' @param dir `character`. Directory to save data; this should usually remain
#'   unchanged.
#'
#' @param file `character`. File name for data; this should usually remain
#'   unchanged. The resulting file will **always** have a "csv" extension; if
#'   you pass a `file` name without a "csv" extension, the function will
#'   replace it with a warning.
#'
#' @param force Should the download overwrite an existing file, if one exists?
#'   The default is `FALSE`, which errors if an existing file is found.
#'
#' @return The path to the data as an `fs_path` character vector
download_redcap_records <- function(
  api_token,
  dir,
  file,
  headers = c("raw", "label"),
  values = c("raw", "label"),
  filter = NULL,
  force = FALSE
) {

  # Check `api_token`
  if (!rlang::is_scalar_character(api_token)) {
    rlang::abort("`api_token` must be a scalar character string")
  }
  # Check `filter`
  if (!(is.null(filter) || rlang::is_scalar_character(filter))) {
    rlang::abort("`filter` must be `NULL` or a scalar character string")
  }
  # Check `force`
  if (!rlang::is_bool(force)) rlang::abort("`force` must be `TRUE` or `FALSE`")
  # Check `headers`
  headers <- rlang::arg_match(headers)[[1L]]
  # Check `values`
  values  <- rlang::arg_match(values)[[1L]]

  # Check and change file extension - warn if invalid
  ext <- file %>%
    fs::path_ext() %>%
    stringr::str_to_lower() %>%
    stringr::str_remove_all("\\s+")

  if (ext == "") {
    msg <- paste0(
      "`file` was supplied without a file extension; ",
      "the extension for the NIT data file must be 'csv'. ",
      "A 'csv' extension will be appended to `file`."
    )
    rlang::warn(msg)
  } else if (ext != "csv") {
    msg <- paste0(
      "`file` was supplied with the file extension '", ext, "'; ",
      "the extension for the NIT data file must be 'csv'. ",
      "The existing extension will be replaced with 'csv'."
    )
    rlang::warn(msg)
  }
  file <- fs::path_ext_set(file, ext = "csv")

  # Create full file path
  path <- path_create(dir, file)

  # Check whether file exists before downloading data
  if (!force && fs::file_exists(path)) {
    rlang::abort(
      "A file already exists at this location; to overwrite, set `force = TRUE`"
    )
  }

  # URL base for API
  api_url <- "https://redcap.shelbycountytn.gov/api/"

  # API parameters
  # `exportCheckboxLabel` is ignored if `rawOrLabel = "raw"`
  api_params <- list(
    token               = api_token,
    content             = "record",
    format              = "csv",
    type                = "flat",
    rawOrLabel          = values,
    rawOrLabelHeaders   = headers,
    exportCheckboxLabel = "true",
    returnFormat        = "json"
  )
  # Add filter logic if `filter` is not `NULL`
  if (!is.null(filter)) api_params <- c(api_params, filterLogic = filter)

  httr::RETRY(
    "POST",
    url = api_url,
    body = api_params,
    encode = "form",
    httr::progress(),
    httr::write_disk(path, overwrite = force)
  ) %>%
    httr::stop_for_status(paste("download REDcap data:", httr::content(.)))

  path
}
