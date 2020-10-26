coviData::download_integrated_data(force = TRUE)
data <- coviData::load_integrated_data(date = Sys.Date()) %>%
  preprocess()

data %>%
  dplyr::filter(!is.na(infecstart) & !is.na(infecend)) %>%
  dplyr::mutate(
    some_address = dplyr::coalesce(
      emp1address,
      emp1address_10,
      emp1address_7,
      emp1address_19,
      cont_exp_t3,
      cont_exp_t16,
      exphealth_2,
      non_health_3,
      name_sch_3,
      exposure1details4,
      exposure1details14,
      exposure1details15
    )
  ) %>%
  dplyr::filter(!is.na(some_address)) %>%
  dplyr::select(-some_address) %>%
  tidyr::pivot_longer(
    cols = c(
      emp1address,
      emp1address_10,
      emp1address_7,
      emp1address_19,
      cont_exp_t3,
      cont_exp_t16,
      exphealth_2,
      non_health_3,
      name_sch_3,
      exposure1details4,
      exposure1details14,
      exposure1details15
    ),
    names_to = "new_address_field",
    values_to = "new_address"
  ) %>%
  dplyr::mutate(
    clustered_address = new_address %>% refinr::key_collision_merge()
  ) %>%
  dplyr::mutate(
    infecstart = lubridate::as_date(infecstart),
    infecend   = lubridate::as_date(infecend)
  ) ->
clean_addresses

clean_addresses %>%
  dplyr::count(student) %>%
  dplyr::arrange(dplyr::desc(n))

clean_addresses %>%
  dplyr::filter(student == "Yes") %>%
  dplyr::count(school_type) %>%
  dplyr::arrange(dplyr::desc(n))

clean_addresses %>%
  dplyr::transmute(
    record_id,
    clustered_address,
    exposure_window = lubridate::interval(
      start = infecstart - 12,
      end = infecstart - 1
    ) %>% lubridate::int_standardize(),
    infection_window = lubridate::interval(
      start = infecstart,
      end = infecend
    )
  ) ->
  intervals

intervals %>%
  dplyr::mutate(
    exposure_id = record_id,
    infection_id = record_id,
    exposure_address = clustered_address,
    infection_address = clustered_address
  ) %>%
  tidyr::expand(nesting(infection_address, infection_id, infection_window), nesting(exposure_address, exposure_id, exposure_window)) %>%
  dplyr::filter(
    lubridate::int_overlaps(exposure_window, infection_window),
    exposure_address == infection_address,
    infection_id != exposure_id
  ) %>%
  dplyr::transmute(clustered_address = infection_address, infection_id, infection_window, exposure_id, exposure_window) %>%
  dplyr::arrange(clustered_address, infection_window, exposure_window, infection_id, exposure_id)
address_cluster_data

openxlsx::write.xlsx(address_cluster_data, file = "V:/EPI DATA ANALYTICS TEAM/Cluster Analysis/address_clusters.xlsx")
