#' Archive `fst` Files of NBS Report Dates Based on Date of Creation
#'
#' `archive_report_date()` infers the date reported for NBS IDs using the date
#' of creation for the data files in `data_dir`. It saves the inferred report
#' dates in `archive_dir`.
#'
#' @param data_dir The directory containing the NBS data files to archive
#'
#' @param archive_dir The location of the archive directory
#'
#' @return A `tibble` containing the data files paths, creation (birth) date,
#'   and archive file paths for newly created archive data
#'
#' @export
archive_report_date <- function(
  data_dir = path_create(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
    "Sandbox data pull Final/"
  ),
  archive_dir = path_create(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
    "Report Date Linelist/archive/"
  )
) {

  # Get data files
  rlang::inform("Getting file info...")
  data_dir %>%
    path_clean() %>%
    fs::dir_info() %>%
    dplyr::filter(stringr::str_ends(.data[["path"]], ".csv")) %>%
    dplyr::transmute(
      .data[["path"]],
      birth_date = lubridate::as_date(.data[["birth_time"]])
    ) %>%
    dplyr::arrange(dplyr::desc(.data[["birth_date"]])) %>%
    coalesce_dupes(.data[["birth_date"]]) %>%
    dplyr::mutate(
      save_as = path_create(
        archive_dir,
        paste0("nbs_data_", .data[["birth_date"]]),
        ext = "fst"
      )
    ) ->
  data_files

  # Get already archived files
  rlang::inform("Checking archive for existing data...")
  archive_dir %>%
    path_clean() %>%
    fs::dir_ls() %>%
    dplyr::as_tibble() %>%
    dplyr::transmute(
      path = path_clean(.data[["value"]]),
      birth_date = .data[["path"]] %>%
        fs::path_file() %>%
        stringr::str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}") %>%
        lubridate::as_date()
    ) %>%
    dplyr::arrange(dplyr::desc(.data[["birth_date"]])) ->
  archive_files

  # Only process new files
  new_files <- dplyr::anti_join(data_files, archive_files, by = "birth_date")

  if (vec_is_empty(new_files)) {

    # Do nothing if no new files
    rlang::inform("No new data to archive.")

  } else {

    # These can be large, and may need a lot of memory for reading NBS data
    remove(data_files, archive_files)

    # Archive new files
    rlang::inform("Archiving new data...")
    new_files %>%
      purrr::transpose() %>%
      purrr::walk(~ to_report_date_fst(.x$path, .x$birth_date, .x$save_as))
    rlang::inform("Done.")
  }

  invisible(new_files)

}

#' Convert NBS Data to `fst` Files Containing Report Dates
#'
#' `to_report_date_fst()` is an internal function called by
#' \code{\link[coviData:archive_report_date]{archive_report_date()}}. It saves
#' inferred report dates (`birth_date`) to `save_as` for each NBS ID in the
#' files in `path`.
#'
#' @param path The paths to data files to read
#'
#' @param birth_date The date of creation of each file - used as inferred report
#'   date
#'
#' @param save_as The location to save each file
#'
#' @return A `tibble` containing the input vectors
#'   `path`, `birth_date`, `save_as` (invisibly)
#'
#' @keywords internal
to_report_date_fst <- function(path, birth_date, save_as) {
  vroom::vroom(
    file = path,
    col_select = INV_LOCAL_ID,
    col_types = vroom::cols(INV_LOCAL_ID = vroom::col_character())
  ) %>%
    janitor::clean_names() %>%
    dplyr::mutate(report_date = birth_date) %>%
    fst::write_fst(path = save_as, compress = 100L)

  invisible(
    dplyr::tibble(path = path, birth_date = birth_date, save_as = save_as)
  )
}

#' Coalesce Report Dates from Archived `fst` Files
#'
#' `coalesce_report_date()` coalesces the output of `archive_report_date()` into
#' a `tibble`. Internally, this is a series of join-then-coalesce operations.
#'
#' @inheritParams archive_report_date
#'
#' @param id The name of the column to use when joining data; defaults to
#'   "inv_local_id"
#'
#' @return A `tibble` containing the `id` column and a `report_date` column
#'
#' @export
coalesce_report_date <- function(
  archive_dir = path_create(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
    "Report Date Linelist/archive/"
  ),
  id = "inv_local_id"
) {

  # Get files
  rlang::inform("Reading archived data...")
  archive_dir %>%
    path_clean() %>%
    fs::dir_ls() %>%
    dplyr::as_tibble() %>%
    dplyr::rename(path = value) %>%
    dplyr::filter(
      path %>%
        fs::path_file() %>%
        stringr::str_detect(pattern = "^nbs_data_.*[.]fst$")
    ) ->
  archive_files

  # Create progress bars if used in interactive session
  if (interactive()) {
    pb_reduce <- progress::progress_bar$new(
      format = paste0(":percent [:bar] Estimated Time Remaining: :eta"),
      total = vec_size(archive_files) - 1,
      show_after = 0
    )
  } else {
    pb_reduce <- NULL
  }

  # Read fst datasets
  archive_files %>%
    dplyr::transmute(
      path,
      # Birth date of original file - used to sort
      birth_date = path %>%
        fs::path_file() %>%
        stringr::str_extract(pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2}") %>%
        lubridate::as_date(),
      fst_data = purrr::map(path, ~ fst::fst(.x))
    ) %>%
    dplyr::arrange(dplyr::desc(birth_date)) %>%
    dplyr::pull(fst_data) ->
  fst_data

  remove(archive_dir, archive_files)

  id <- rlang::ensym(id)

  # Coalesce report dates by file

  rlang::inform("Coalescing report dates...")
  if (interactive()) pb_reduce$tick(0)

  fst_data %>%
    purrr::reduce(reduce_fst_report_dates, .by = id, .pb = pb_reduce) %>%
    dplyr::mutate(report_date = lubridate::as_date(report_date)) %>%
    dplyr::arrange(dplyr::desc(report_date)) %>%
    set_attr("is_report_date", TRUE) %T>%
    {rlang::inform("Done.")}

}

#' Join Report Dates from `fst` Files
#'
#' `reduce_fst_report_dates()` is the workhorse of `coalesce_report_date()`. It
#' performs the `reduce()` operations that sequentially convert fst objects to
#' tibbles, left join the result, and coalesce the `report_date` columns in each
#' to a single column. The column from `.next` gets preference in the
#' `coalesce()` operation.
#'
#' @param .accumulated The accumulated results from `reduce()`
#'
#' @param .next The next data set to reduce
#'
#' @param .by The name of the column to use when joining data; defaults to
#'   "inv_local_id"
#' @param .pb An optional progress bar object created by
#'   \code{\link[progress]{progress_bar}}; will be incremented after each
#'   reduction step
#'
#' @return The reduced and coalesced data as a `tibble`
#'
#' @keywords internal
reduce_fst_report_dates <- function(
  .accumulated,
  .next,
  .by,
  .pb = NULL
) {

  dplyr::left_join(
    tibble::as_tibble(.accumulated),
    tibble::as_tibble(.next),
    by = rlang::expr_name(.by),
    suffix = c(".accumulated", ".next")
  ) %>%
    dplyr::transmute(
      {{ .by }},
      report_date = dplyr::coalesce(report_date.next, report_date.accumulated)
    ) %T>%
    {if (!rlang::is_null(.pb)) .pb$tick()}

}

#' Add Specimen Collection Date to a Report Date Tibble
#'
#' `add_collection_date()` reads the specimen collection date column from an
#' NBS data file and adds it to the input data (typically the result of
#' `coalesce_report_date()`).
#'
#' @param .data A `tibble` returned from `coalesce_report_date()`
#'
#' @param .from_col A string indicating the variable in `.from_file` to use as
#'   specimen collection date
#'
#' @param .id_col A string indicating the variable in `.data` and `from_file`
#'   to use as an ID variable
#'
#' @param ... `<tidy-select>` Additional variables to add
#'
#' @param date The date of the NBS file to use for collection dates
#'
#' @param from_file A file path to the NBS data with the desired specimen
#'   collection dates; if only a directory is provided, the NBS file
#'   corresponding to `date` will be used
#'
#' @export
add_collection_date <- function(
  .data,
  .from_col = "specimen_coll_dt",
  .id_col = "inv_local_id",
  ...,
  date = Sys.Date(),
  from_file = path_create(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP Data/",
    "Sandbox data pull Final/"
  )
) {

  vec_assert(.id_col, ptype = character())
  vec_assert(.from_col, ptype = character())

  assert_cols(.data, dplyr::matches(paste0("^", .id_col, "$")), n = 1L)
  assert_cols(
    .data,
    dplyr::matches(paste0("^", c(.from_col, "collection_date"), "$")),
    n = 0L
  )

  if (!collection_date_0) {
    rlang::warn(
      paste0(
        "`.data` already contains columns matching `.from_col` ",
        "or named 'collection_date'"
      )
    )
  }

  from_file <- path_clean(from_file)

  # If `.from_file` is a directory, load the latest NBS file from it
  if (fs::is_dir(from_file)) {
    from_file <- find_file(
      date = date,
      directory = paste0(from_file, "/"),
      pattern = paste0(".*", date, ".*[.]csv")
    ) %>% path_clean()
  }

  # Read `.id_col` and `.from_col` from `.from_file`
  rlang::inform("Reading `.from_file`...")
  collection_date <- vroom::vroom(
    from_file,
    col_types = vroom::cols(.default = vroom::col_character())
  ) %>%
    janitor::clean_names() %>%
    dplyr::select(.id_col, .from_col, ...) %>%
    standardize_dates() %>%
    dplyr::rename(collection_date = .from_col)

  rlang::inform("Joining by `.id_col`...")
  tidylog::inner_join(
    .data,
    collection_date,
    by = .id_col,
    suffix = c(".data", ".collection_date")
  ) %>%
    set_attr("is_report_date", TRUE) %T>%
    {rlang::inform("Done.")}

}

#' Save Report Dates to `fst` File
#'
#' `save_report_date()` is a simple wrapper around
#' \code{\link[fst:write_fst]{write_fst()}} that saves reduced report date data
#' to an `fst` file. It exists purely for convenience and semantic clarity; it
#' can only be used with the output of `coalesce_report_date()` or
#' `add_collection_date()`.
#'
#' @param .data A `tibble` returned from `coalesce_report_date()` or
#'   `add_collection_date()`
#'
#' @param save_as The write path to the new `fst` file
#'
#' @return The input data (invisibly)
#'
#' @export
save_report_date <- function(
  .data,
  save_as = path_create(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
    "Report Date Linelist/report_date_linelist",
    ext = "fst"
  )
) {

  assert(
    rlang::is_true(attr(.data, "is_report_date")),
    message= paste0(
      "`.data` must be a tibble returned by `add_report_date()` ",
      "or `coalesce_report_date()`"
    )
  )

  fst::write_fst(.data, path = path_clean(save_as), compress = 100L)

  paste0("`.data` saved to ", path_clean(save_as)) %>% rlang::inform()

  invisible(.data)
}


#' Load Report Dates from `fst` File
#'
#' `load_report_date()` is a simple wrapper around
#' \code{\link[fst:fst]{fst()}} that reads reduced report date data
#' from an `fst` file. It exists purely for convenience and semantic clarity; it
#' should only be used with files created by `save_report_date()`.
#'
#' @param path The path to a file created by `save_report_date()`
#'
#' @return An `fst_tbl` object
#'
#' @export
load_report_date <- function(
  path = path_create(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/",
    "Report Date Linelist/report_date_linelist",
    ext = "fst"
  )
) {
  fst::fst(path_clean(path))
}
