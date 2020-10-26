preprocess_nbs <- function(
  .nbs_data,
  min_completion_rate = 0.5
) {

  message("\nCleaning patient names...")
  .nbs_data %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::matches(c("_name", "_nm")),
        .fns = standardize_string
      )
    ) %T>%
    {message("Done.")} %T>%
    {message("\nCleaning variables and data types...", appendLF = FALSE)} %>%
    preprocess() %T>%
    {message("Done.\n")} %T>%
    {message("Factoring data for further wrangling...", appendLF = FALSE)} %>%
    dplyr::mutate(
      dplyr::across(where(not_factor_date), factor)
    ) %T>%
    {message("Done.\n")} ->
    cleaner_data

  message("Finding informative variables in:")

  message("all data...")
  cleaner_data %>%
    cols_to_keep(min_completion_rate = min_completion_rate) ->
    all_cols

  message("cases...")
  cleaner_data %>%
    dplyr::filter(inv_case_status == "C" | inv_case_status == "P") %>%
    cols_to_keep(min_completion_rate = min_completion_rate) ->
    case_cols

  message("hospitalizations...")
  cleaner_data %>%
    dplyr::filter(hsptlizd_ind == "Y") %>%
    cols_to_keep(min_completion_rate = min_completion_rate) ->
    hosp_cols

  message("deaths...")
  cleaner_data %>%
    dplyr::filter(die_from_illness_ind == "Y") %>%
    cols_to_keep(min_completion_rate = min_completion_rate) ->
    death_cols

  message("Done.\n")

  message(
    "Removing uninformative variables and observations...",
    appendLF = FALSE
  )

  all_cols$info %>%
    dplyr::union(case_cols$info) %>%
    dplyr::union(hosp_cols$info) %>%
    dplyr::union(death_cols$info) %>%
    dplyr::union("inv_local_id") ->
    keep_cols_info

  all_cols$missing %>%
    dplyr::union(case_cols$missing) %>%
    dplyr::union(hosp_cols$missing) %>%
    dplyr::union(death_cols$missing) %>%
    dplyr::union("inv_local_id") ->
    keep_cols_missing

  keep_cols <- dplyr::intersect(keep_cols_info, keep_cols_missing)

  cleaner_data %>%
    dplyr::select(dplyr::matches(keep_cols)) %>%
    janitor::remove_empty(which = "rows") %>%
    dplyr::as_tibble() %T>%
    {message("Done!\n")}
}
