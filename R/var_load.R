#' Load Variant Cases from NIT Project
#'
#' `var_load()` loads variant cases from the NIT REDcap project. If possible,
#' it downloads the most up-to-date information from the REDcap server; if not,
#' it reads from the most recently downloaded NIT file.
#'
#' @param api_token Character. An API token/key for the NIT project.
#'
#' @return A `tibble` of `character` columns containing all variant case records
#'
#' @export
var_load <- function(api_token = Sys.getenv("redcap_NIT_token")) {
  api_url <- "https://redcap.shelbycountytn.gov/api/"
  api_params <- list(content = "project", format = "json")
  ping <- try(
    httr::POST(url = api_url, body = api_params, encode = "form") %>%
      httr::stop_for_status(task = paste("query project:", httr::content(.))),
    silent = TRUE
  )

  remote_available <- rlang::inherits_any(ping, "try-error")

  if (remote_available) {
    tmp_dir <- fs::file_temp("variant")
    fs::dir_create(tmp_dir)
    tmp_file <- fs::file_temp("variant", tmp_dir = tmp_dir, ext = "csv") %>%
      fs::path_file()
    download_redcap_records(
      api_token = api_token,
      dir = tmp_dir,
      file = tmp_file,
      filter = '[variantcase(1)] = "1"',
      force = TRUE
    )
    read_file_delim(path_create(tmp_dir, tmp_file))
  } else {
    dplyr::filter(load_nit(), .data[["variantcase"]] == "1")
  }
}
