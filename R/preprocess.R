preprocess_nbs <- function(
  .nbs_data,
  min_completion_rate = 0.5
) {

  message("\nCleaning patient names...")
  .nbs_data %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::matches(c("_name", "_nm")),
        .fns = standardize_names
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
    generics::union(case_cols$info) %>%
    generics::union(hosp_cols$info) %>%
    generics::union(death_cols$info) %>%
    generics::union("inv_local_id") ->
    keep_cols_info

  all_cols$missing %>%
    generics::union(case_cols$missing) %>%
    generics::union(hosp_cols$missing) %>%
    generics::union(death_cols$missing) %>%
    generics::union("inv_local_id") ->
    keep_cols_missing

  keep_cols <- generics::intersect(keep_cols_info, keep_cols_missing)

  cleaner_data %>%
    dplyr::select(tidyselect::matches(keep_cols)) %>%
    janitor::remove_empty(which = "rows") %>%
    tibble::as_tibble() %T>%
    {message("Done!\n")}
}
