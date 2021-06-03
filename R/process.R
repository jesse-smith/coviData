#' Run the Cases Workflow for Daily Report
#'
#' @description
#' The `process_*` functions transform NBS data in preparation for analysis.
#'
#' `process_inv()` transforms investigation data.
#'
#' `process_pcr()` transforms PCR data.
#'
#' `pos()` and `neg()` extract positive or negative data from the result.
#'
#' @details
#' `process_inv()` is special in that it saves records identified as positive
#' and negative to the `V:` drive as an `fst` file. This allows significant
#' speedup on subsequent runs, since the only necessary operations are mutating
#' of key variables prior to filtering.
#'
#' `process_pcr()` joins PCR data to the results of `process_inv()`. These
#' results can be supplied by the `inv` argument; by default, they are
#' calculated on-the-fly.
#'
#' Both functions need the download date of their data.
#' \code{\link[coviData:read_inv]{read_inv()}} and
#' \code{\link[coviData:read_pcr]{read_pcr()}} supply this as a `date`
#' attribute; if this exists in the input data, it is used
#' (with a warning if it conflicts with the `date` argument). If it does not
#' exist, the `date` argument defaults to the most recent download date;
#' in general, the `date` should either be supplied by an attribute (as above)
#' or explicitly passed to the `date` argument.
#'
#' `process_positive_people()`, `process_negative_people()`,
#' `process_positive_tests()`, and `process_negative_tests()` are deprecated
#' in favor of `pos(process_inv())`, `neg(process_inv())`, `pos(process_pcr())`,
#' and `neg(process_pcr())`
#'
#' @param data A `tibble`. Input data- either investigations for
#'   `process_inv()` or PCR tests for `process_pcr()`
#'
#' @param inv A `tibble`. Input investigation data for `process_pcr()`, as
#'   returned by `process_inv()`.
#'
#' @param date The download date of the data. If the data has a `date`
#'   attribute, this is used; otherwise, the date must be supplied. See
#'   `Details` above.
#'
#' @param replace Should the filtering results be recalculated and replace the
#'   existing results (if any)?
#'
#' @param quiet Should intermediate step summaries be output as messages?
#'
#' @return A nested 2x2 `tibble` with `logical` column `positive` and
#'   `list_of<tbl_df>` column `data`; the latter contains the transformed
#'   positive and negative data
#'
#' @name process-nbs
NULL

#' @rdname process-nbs
#'
#' @export
process_inv <- function(
  data = read_inv(),
  date = attr(data, "date"),
  replace = FALSE,
  quiet = FALSE
) {

  if (is_processed(data)) return(data)

  date    <- date_inv(date)
  data_dt <- attr(data, "date")

  if (rlang::is_true(date != data_dt) && !vec_is_empty(data_dt)) {
    rlang::warn(paste(
      "The `date` argument does not match the `date` attribute of `data`;",
      "the attribute will be used"
    ))
    date <- data_dt
  }

  assert_bool(replace)
  assert_bool(quiet)

  processed <- !vec_is_empty(path_inv_id(date))
  if (processed && !replace) {
    data %>%
      janitor::clean_names() %>%
      mutate_inv(date = date, quiet = quiet) %>%
      join_inv_id(date = date, quiet = quiet) %>%
      set_attr("date", date)
  } else {
    data %>%
      janitor::clean_names() %>%
      filter_inv(date = date, quiet = quiet) %>%
      mutate_inv(date = date, quiet = quiet) %>%
      nest_inv(date = date) %>%
      distinct_inv(date = date, quiet = quiet) %>%
      write_inv_key(date = date) %>%
      set_attr("date", date)
  }
}

#' @rdname process-nbs
#'
#' @export
process_pcr <- function(
  data = read_pcr(),
  inv = process_inv(date = date),
  date = attr(data, "date"),
  quiet = FALSE
) {

  if (is_processed(data)) return(data)

  date    <- date_inv(date)
  data_dt <- attr(data, "date")

  if (rlang::is_true(date != data_dt) && !vec_is_empty(data_dt)) {
    rlang::warn(paste(
      "The `date` argument does not match the `date` attribute of `data`;",
      "the attribute date will be used"
    ))
    date <- data_dt
  }

  inv_is_processed <- all(
    rlang::is_true(all.equal(c("positive", "data"), colnames(inv))),
    vec_is(inv[["positive"]], ptype = logical()),
    rlang::inherits_all(inv[["data"]],c("vctrs_list_of", "vctrs_vctr", "list")),
    tibble::is_tibble(inv[["data"]][[1L]])
  )

  if (!inv_is_processed) inv <- process_inv(inv)

  data %>%
    janitor::clean_names() %>%
    mutate_pcr() %>%
    join_pcr_inv(inv = inv)
}

#' @rdname process-nbs
#'
#' @export
pos <- function(data) pull_processed(data, "+")

#' @rdname process-nbs
#'
#' @export
neg <- function(data) pull_processed(data, "-")

#' @rdname process-nbs
#'
#' @export
process_positive_people <- function(
  data = read_inv(date),
  date = NULL,
  replace = FALSE
) {
  pos(process_inv(
    data = data,
    date = if (!vec_is_empty(date)) date else attr(data, "date"),
    replace = replace
  ))
}

#' @rdname process-nbs
#'
#' @export
process_negative_people <- function(
  data = read_inv(date),
  date = NULL,
  replace = FALSE
) {
  neg(process_inv(
    data = data,
    date = if (!vec_is_empty(date)) date else attr(data, "date"),
    replace = replace
  ))
}

#' @rdname process-nbs
#'
#' @export
process_positive_tests <- function(
  data = read_pcr(date),
  date = NULL
) {
  pos(process_pcr(
    data = data,
    date = if (!vec_is_empty(date)) date else attr(data, "date")
  ))
}

#' @rdname process-nbs
#'
#' @export
process_negative_tests <- function(
  data = read_pcr(date),
  date = NULL
) {
  neg(process_pcr(
    data = data,
    date = if (!vec_is_empty(date)) date else attr(data, "date")
  ))
}
