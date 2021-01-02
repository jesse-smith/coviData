#' Download Cases from Case Assignment REDcap Project
#'
#' `asg_download_redcap_cases()` downloads records from the
#' \strong{Case Assignment} REDcap project. If `incl_assigned = FALSE`, it only
#' downloads unassigned cases; if `incl_assigned = TRUE`, it downloads all
#' cases.
#'
#' @param api_token The API token for the Case Assignment REDcap project. The
#' default pulls this from the environment variable `redcap_CA_token`
#'
#' @param unassigned_only Should only unassigned cases be downloaded?
#'
#' @return A tidy `tibble` with all variables in `character` format
asg_download_redcap_cases <- function(
  api_token = Sys.getenv("redcap_CA_token"),
  unassigned_only = FALSE
) {

  # URL base for API
  api_uri <- "https://redcap.health.tn.gov/redcap/api/"

  api_params <- list(
    token = api_token,
    content = "record",
    format = "json",
    type = "flat"
  )

  if (unassigned_only) {
    api_params <- c(api_params, filterLogic = "[investigator]=''")
  }

  httr::RETRY(
    "POST",
    api_uri,
    body = api_params,
    times = 12L,
    pause_cap = 300L
  ) %>%
    httr::stop_for_status() %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON() %>%
    dplyr::as_tibble() %>%
    dplyr::select(-"six")
}

#' Download Investigators Listed in the Case Assignment REDcap Project
#'
#' `asg_download_redcap_investigators()` downloads all investigators available
#' for assignment in the \strong{Case Assignment} REDcap project, as well as the
#' `id` assigned to each investigator in REDcap.
#'
#' @param api_token The API token for the Case Assignment REDcap project. The
#' default pulls this from the environment variable `redcap_CA_token`.
#'
#' @return A `tibble` with one row for each investigator and columns for `id`
#'   and `investigator` (name)
#'
#' @export
asg_download_redcap_investigators <- function(
  api_token = Sys.getenv("redcap_CA_token")
) {
  # URL base for API
  api_uri <- "https://redcap.health.tn.gov/redcap/api/"

  api_params <- list(
    token = api_token,
    format = "json",
    content = "metadata"
  )

  httr::RETRY(
    "POST",
    api_uri,
    body = api_params,
    times = 12L,
    pause_cap = 300L
  ) %>%
    httr::stop_for_status() %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON() %>%
    dplyr::filter(.data[["field_name"]] == "investigator") %>%
    dplyr::pull("select_choices_or_calculations") %>%
    stringr::str_split(pattern = " [|] ") %>%
    purrr::flatten_chr() %>%
    tibble::as_tibble_col("inv") %>%
    tidyr::separate(
      col = "inv",
      into = c("id", "investigator"),
      sep = "[,] "
    ) %>%
    dplyr::mutate(
      investigator = asg_parse_names(.data[["investigator"]])
    )
}

#' Join Investigators Listed in Investigation Schedule & Case Assignment Project
#'
#' `asg_join_investigators()` joins the investigators scheduled to work on
#' `date` with the list of investigators from the \strong{Case Assignment}
#' REDcap project. By default, it performs an inner join, since only
#' investigators in both lists are available for case assignment.
#'
#' While the default behavior is an inner join, `asg_join_investigators()` can
#' also perform anti-joins with either the scheduled or REDcap investigator list
#' as the primary table. This is useful for debugging and checking that all
#' scheduled investigators are able to have cases assigned.
#'
#' @inheritParams asg_get_scheduled_investigators
#'
#' @inheritParams asg_download_redcap_investigators
#'
#' @param type The type of join to use. `"inner"` returns only investigators in
#'   both lists (the default). `"anti_join_scheduled"` returns investigators who
#'   are scheduled but not listed in REDcap; `"anti_join_redcap"` returns
#'   investigators who are listed in REDcap but not scheduled.
#'
#' @param quiet Should update messages be suppressed?
#'
#' @return A `tibble` with one row per investigator and an `investigator`
#'   column. If `type = "inner"`, an `id` column is also included.
#'
#' @export
asg_join_investigators <- function(
  date = Sys.Date(),
  type = c("inner", "anti_schedule", "anti_redcap"),
  api_token = Sys.getenv("redcap_CA_token"),
  quiet = FALSE
) {

  type <- rlang::arg_match(type)[[1]]

  if (!quiet) rlang::inform("Loading investigators...")
  inv_scheduled <- suppressMessages(
    asg_get_scheduled_investigators(date = date)
  )
  inv_redcap <- suppressMessages(
    asg_download_redcap_investigators(api_token = api_token)
  )

  if (type == "inner") {
    if (!quiet) rlang::inform("Returning investigators in both lists...")
    dplyr::inner_join(
      inv_scheduled,
      inv_redcap,
      by = "investigator"
    ) %>%
      dplyr::select("id", "investigator") %>%
      dplyr::arrange("investigator")
  } else if (type == "anti_schedule") {
    if (!quiet) rlang::inform("Returning names only in scheduled list...")
    dplyr::anti_join(
      .schedule,
      .redcap,
      by = "investigator"
    ) %>%
      dplyr::select("investigator") %>%
      dplyr::arrange("investigator")
  } else {
    if (!quiet) rlang::inform("Returning names only in REDcap list...")
    dplyr::anti_join(
      .redcap,
      .schedule,
      by = "investigator"
    ) %>%
      dplyr::select("investigator") %>%
      dplyr::arrange("investigator")
  }
}
