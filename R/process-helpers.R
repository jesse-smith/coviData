# Investigation data frames ----------------------------------------------------
filter_inv <- function(
  data = read_inv(),
  date = attr(data, "date"),
  quiet = FALSE
) {
  date <- date_inv(date)
  assert_bool(quiet)
  missings <- c(NA_character_, "Na", "", " ")

  filter <- if (quiet) dplyr::filter else tidylog::filter

  data %>%
    janitor::clean_names() %>%
    # Filter to only our jurisdiction/county/state
    filter(
      stringr::str_detect(
        .data[["jurisdiction_nm"]],
        "(?i).*Memphis.*Shelby.*County.*"
      ),
      .data[["alt_county"]] %in% c("Shelby County", {{ missings }}),
      .data[["alt_county"]] == "Shelby County" |
        as.numeric(.data[["patient_state"]]) %in% c(47, NA)
    ) %>%
    # Filter out unknowns, suspects, and antibody/antigen negatives
    dplyr::mutate(
      .obs_tmp_ = stringr::str_starts(.data[["inv_local_id"]], "(?i)OBS"),
      .not_pcr_tmp_ = .data[["negative_ig_a"]] %in% "1"
      | .data[["negative_ig_a"]] %in% "1"
      | .data[["negative_ig_g"]] %in% "1"
      | .data[["negative_ig_m"]] %in% "1"
      | .data[["negative_antigen"]] %in% "1"
    ) %>%
    filter(
      !(.data[[".obs_tmp_"]] & .data[[".not_pcr_tmp_"]]),
      .data[["inv_case_status"]] %in% c("C", "P", "N")
    ) %>%
    dplyr::select(-c(".obs_tmp_", ".not_pcr_tmp_")) %>%
    set_attr("date", date) %T>%
    {gc(verbose = FALSE)}
}

mutate_inv <- function(
  data = filter_inv(),
  date = attr(data, "date"),
  quiet = FALSE
) {
  date <- date_inv(date)
  assert_bool(quiet)

  mutate <- if (quiet) dplyr::mutate else tidylog::mutate
  rename <- if (quiet) dplyr::rename else tidylog::rename

  data %>%
    janitor::clean_names() %>%
    mutate(
      patient_last_name = .data[["patient_last_name"]] %>%
        str_to_ascii() %>%
        stringr::str_to_upper() %>%
        stringr::str_remove_all(pattern = "\\s+"),
      patient_first_name = .data[["patient_first_name"]] %>%
        str_to_ascii() %>%
        stringr::str_to_upper() %>%
        stringr::str_remove_all(pattern = "\\s+"),
      patient_dob = std_dates(
        .data[["patient_dob"]],
        orders = "ymdT",
        force = "dt",
        train = FALSE
      ),
      patient_street_addr_1 = .data[["patient_street_addr_1"]] %>%
        str_to_ascii() %>%
        stringr::str_to_upper() %>%
        stringr::str_remove_all(pattern = "\\s+"),
      patient_zip = .data[["patient_zip"]] %>%
        str_to_ascii() %>%
        stringr::str_trunc(width = 5, ellipsis = ""),
      specimen_coll_dt = std_dates(
        .data[["specimen_coll_dt"]],
        orders = "ymdT",
        force = "dt",
        train = FALSE
      )
    ) %>%
    rename(patient_street_addr = "patient_street_addr_1") %>%
    set_attr("date", date) %T>%
    {gc(verbose = FALSE)}
}

nest_inv <- function(data = mutate_inv(), date = attr(data, "date")) {
  date <- date_inv(date)
  data %>%
    janitor::clean_names() %>%
    dplyr::nest_by(positive = .data[["inv_case_status"]] %in% c("C","P")) %>%
    dplyr::arrange(dplyr::desc(.data[["positive"]])) %>%
    set_attr("date", date) %T>%
    {gc(verbose = FALSE)}
}

distinct_inv <- function(
  data = nest_inv(),
  date = attr(data, "date"),
  quiet = FALSE
) {
  date <- date_inv(date)
  assert_bool(quiet)

  data %>%
    janitor::clean_names() %>%
    dplyr::summarize(
      .data[["positive"]],
      data = list_of(
        distinct_inv_(
          .data[["data"]],
          is_positive = .data[["positive"]],
          date = date,
          quiet = quiet
        )
      )
    ) %>%
    set_attr("date", date) %T>%
    {gc(verbose = FALSE)}
}

distinct_inv_ <- function(data, is_positive, date, quiet = FALSE) {
  date <- date_inv(date)
  if (is_positive) {
    distinct_pos(data, date = date, quiet = quiet)
  } else {
    distinct_neg(data, date = date, quiet = quiet)
  }
}

distinct_pos <- function(data, date, quiet = FALSE) {
  date <- date_inv(date)
  distinct <- if (quiet) dplyr::distinct else tidylog::distinct

  data %>%
    janitor::clean_names() %>%
    dplyr::arrange(.data[["inv_local_id"]]) %>%
    distinct(.data[["inv_local_id"]], .keep_all = TRUE) %T>%
    # {gc(verbose = FALSE)} %>%
    # dplyr::arrange(.data[["patient_local_id"]], .data[["inv_case_status"]]) %>%
    # distinct(.data[["patient_local_id"]], .keep_all = TRUE) %T>%
    {gc(verbose = FALSE)} %>%
    dplyr::arrange(
      .data[["patient_last_name"]],
      .data[["patient_first_name"]],
      .data[["patient_dob"]],
      .data[["inv_case_status"]]
    ) %>%
    distinct(
      .data[["patient_last_name"]],
      .data[["patient_first_name"]],
      .data[["patient_dob"]],
      .keep_all = TRUE
    ) %>%
    set_attr("date", date) %T>%
    {gc(verbose = FALSE)}
}

distinct_neg <- function(data, date, quiet = FALSE) {
  date <- date_inv(date)
  distinct <- if (quiet) dplyr::distinct else tidylog::distinct
  data %>%
    janitor::clean_names() %>%
    dplyr::arrange(.data[["patient_local_id"]]) %>%
    distinct(.data[["patient_local_id"]], .keep_all = TRUE) %T>%
    {gc(verbose = FALSE)} %>%
    dplyr::arrange(
      .data[["patient_last_name"]],
      .data[["patient_first_name"]],
      .data[["patient_dob"]],
      .data[["patient_street_addr"]],
      .data[["patient_zip"]]
    ) %>%
    distinct(
      .data[["patient_last_name"]],
      .data[["patient_first_name"]],
      .data[["patient_dob"]],
      .data[["patient_street_addr"]],
      .data[["patient_zip"]],
      .keep_all = TRUE
    ) %>%
    set_attr("date", date) %T>%
    {gc(verbose = FALSE)}
}

write_inv_key <- function(data = distinct_inv(), date = attr(data, "date")) {
  date <- date_inv(date)
  data %>%
    janitor::clean_names() %>%
    dplyr::rowwise() %>%
    dplyr::group_walk(
      ~ write_inv_key_(
        .x[["data"]][[1L]],
        date = date,
        is_positive = .x[["positive"]]
      )
    ) %>%
    set_attr("date", date) %T>%
    {gc(verbose = FALSE)}
}

write_inv_key_ <- function(
  data,
  date,
  is_positive,
  dir = path_create(
    "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/Data for R/",
    "inv_local_id"
  )
) {

  assert_dataframe(data)
  assert_bool(is_positive)
  assert(lubridate::is.Date(date), message = "`date` must be a date")
  date <- date_inv(date)
  assert(rlang::is_string(dir), message = "`dir` must be a string")

  flag <- if (is_positive) "pos_" else "neg_"
  path <- path_create(dir, paste0("inv_local_id_", flag, date), ext = "fst")

  fst::write_fst(data["inv_local_id"], path = path, compress = 100L)

  invisible(path)
}

join_inv_id <- function(
  data = read_inv(),
  pos_id = read_inv_id(date, "+"),
  neg_id = read_inv_id(date, "-"),
  date = attr(data, "date"),
  quiet = FALSE
) {
  assert_bool(quiet)
  dplyr::tibble(
    positive = c(TRUE, FALSE),
    data = list_of(
      join_inv_id_(data, inv_id = pos_id, quiet = quiet),
      join_inv_id_(data, inv_id = neg_id, quiet = quiet)
    )
  ) %>%
    dplyr::rowwise(.data[["positive"]])
}

join_inv_id_ <- function(data, inv_id, quiet = FALSE) {
  semi_join <- if (quiet) dplyr::semi_join else tidylog::semi_join
  data %>%
    janitor::clean_names() %>%
    semi_join(inv_id, by = "inv_local_id")
}

# PCR data frames --------------------------------------------------------------
mutate_pcr <- function(data = read_pcr(), date = attr(data, "date")) {
  data %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      specimen_coll_dt = std_dates(
        .data[["specimen_coll_dt"]],
        orders = "ymdT",
        force = "dt",
        train = FALSE
      )
    ) %T>%
    {gc(verbose = FALSE)}
}

join_pcr_inv <- function(pcr, inv, quiet = FALSE) {
  assert_bool(quiet)
  force(pcr)
  inv %>%
    janitor::clean_names() %>%
    dplyr::summarize(
      .data[["positive"]],
      data = list_of(
        {{ pcr }} %>%
          janitor::clean_names() %>%
          filter_pcr(
            status = if (.data[["positive"]]) "+" else "-",
            quiet = quiet
          ) %>%
          join_pcr_inv_(inv = .data[["data"]], quiet = quiet)
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rowwise(.data[["positive"]]) %T>%
    {gc(verbose = FALSE)}
}

join_pcr_inv_ <- function(pcr, inv, quiet = FALSE) {

  # Handle dplyr/tidylog functions
  inner_join <- if (quiet) dplyr::inner_join else tidylog::inner_join
  mutate     <- if (quiet) dplyr::mutate     else tidylog::mutate

  inv_suf <- "_inv_"

  dupe_cols <- dplyr::intersect(colnames(pcr), colnames(inv)) %>%
    vec_slice(i = !equals(., "inv_local_id"))

  pcr %>%
    inner_join(inv, by = "inv_local_id", suffix = c("", inv_suf)) %T>%
    {gc(verbose = FALSE)} %>%
    mutate(
      dplyr::across(
        {{ dupe_cols }},
        ~ dplyr::coalesce(.x, col_inv(suffix = inv_suf))
      )
    ) %>%
    dplyr::select(-dplyr::ends_with(inv_suf)) %T>%
    {gc(verbose = FALSE)}
}

filter_pcr <- function(
  data = read_pcr(),
  status = c("+", "-"),
  date = attr(data, "date"),
  quiet = FALSE
) {
  assert_bool(quiet)
  filter <- if (quiet) dplyr::filter else tidylog::filter

  status <- rlang::arg_match(status)[[1L]]
  pattern <- if (status == "+") "(?i)positive|presumpt" else "(?i)negative"
  data %>%
    janitor::clean_names() %>%
    filter(
      stringr::str_detect(.data[["lab_result"]], {{ pattern }})
    ) %T>%
    {gc(verbose = FALSE)}
}

# Helpers ----------------------------------------------------------------------
col_inv <- function(
  data = dplyr::cur_data_all(),
  col = dplyr::cur_column(),
  suffix = "_inv_"
) {
  dplyr::pull(data, paste0(col, suffix))
}

pull_processed <- function(data, status = c("+", "-")) {
  status <- rlang::arg_match(status)
  date   <- attr(data, "date")
  data %>%
    assert_processed() %>%
    dplyr::filter(
      if ({{ status }} == "+") .data[["positive"]] else !.data[["positive"]]
    ) %>%
    dplyr::pull("data") %>%
    extract2(1L) %>%
    set_attr("date", date)
}

is_processed <- function(data) {
  data_is_processed <- try(all(
    rlang::is_true(all.equal(c("positive", "data"), colnames(data))),
    vec_is(data[["positive"]], ptype = logical()),
    rlang::inherits_all(data[["data"]],c("vctrs_list_of","vctrs_vctr","list")),
    vec_size(data) == 2L,
    tibble::is_tibble(data[["data"]][[1L]])
  ), silent = TRUE)

  rlang::is_true(data_is_processed)
}

assert_processed <- function(data) {
  assert(
    is_processed(data),
    message = "`data` must be processed investigation or PCR data"
  )
  invisible(data)
}
