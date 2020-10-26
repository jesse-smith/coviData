join_nbs_pcr <- function(
  nbs,
  pcr,
  join_by = "inv_local_id",
  ...
) {
  # Provide defaults if `...` is empty
  if (rlang::dots_n(...) == 0) {
    coalesce_args <- rlang::quos(
      dplyr::desc(collect_date),
      inv_case_status,
      dplyr::desc(inv_local_id)
    )
  } else {
    coalesce_args <- rlang::quos(...)
  }

  # Join data and keep all entries
  dplyr::full_join(
    x = nbs,
    y = pcr,
    by = join_by,
    suffix = c("_from_nbs", "_from_pcr"),
    na_matches = "never"
  ) %>%
    # Coalesce columns in both datasets
    dplyr::mutate(
      patient_local_id = dplyr::coalesce(
        patient_local_id_from_pcr,
        patient_local_id_from_nbs
      ),
      inv_case_status = dplyr::coalesce(
        inv_case_status_from_pcr,
        inv_case_status_from_nbs
      ),
      patient_state = dplyr::coalesce(
        patient_state_from_pcr,
        patient_state_from_nbs
      ),
      collect_date = dplyr::coalesce(
        specimen_coll_dt_from_pcr,
        lubridate::as_datetime(spec_date_only),
        lubridate::as_datetime(specimen_coll_dt_from_nbs)
      )
    ) %>%
    # Remove redundant columns
    dplyr::select(
      -dplyr::contains(c("_from_pcr", "from_nbs")),
      -dplyr::contains("result_desc"),
      -dplyr::contains("_test_cd"),
      -spec_date_only
    ) %>%
    # Make sure the dedup variables are in the correct format
    dplyr::mutate(
      inv_case_status = factor(
        inv_case_status,
        levels = c("C", "P", "N", "I", "S", "U")
      ),
      collect_date = lubridate::as_datetime(collect_date),
      inv_local_id = factor(inv_local_id)
    ) %>%
    # Coalesce rowwise duplicates
    coalesce_dupes(!!!coalesce_args)
}
