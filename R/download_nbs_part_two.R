#' Download a File from the Data for Regions REDcap Project
#'
#' @description
#' These functions downloads a file posted on the Data for Regions
#' project. You'll need API access to the project (and an API token for it) to
#' use this function.
#'
#' `download_extra_case_file()` downloads the second half of the current snapshot of NBS
#' investigation data (if posted).
#'
#'
#' @inherit download_data_for_regions params return
#'
#' @aliases download_extra_case_file download_nbs_snapshot download_pcr_snapshot
#'   download_antigen_snapshot download_serology_snapshot
#'
#' @md
#'
#' @name data-for-regions-snapshots
#'
#' @aliases download_nbs_snapshot download_pcr_snapshot
#'   download_antigen_snapshot download_serology_snapshot
#'   download_vaccine_snapshot
NULL

#' @rdname data-for-regions-snapshots
#'
#' @export
download_extra_case_file <- function(
  date = Sys.Date(),
  api_token = Sys.getenv("redcap_DFR_token"),
  redcap_file = "alt_upload2",
  #redcap_file = "alt_upload_2",
  directory = paste0(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
    "Additional case file/"
  ),
  new_file = paste0(date, " Additional Data Pull.csv"),
  force = FALSE
) {
  message("Starting Additional NBS snapshot download...\n")
  download_data_for_regions(
    date = date,
    api_token = api_token,
    redcap_file = redcap_file,
    directory = directory,
    new_file = new_file,
    force = force
  )
  message("\nFinished NBS snapshot download.")

  invisible(path_create(directory, new_file))
}
