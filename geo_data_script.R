library(coviData)

nbs <- load_nbs(min_completion_rate = 0)

nbs %>%
  dplyr::filter(!is.na(patient_street_addr_1) & !is.na(x) & !is.na(y)) %>%
  dplyr::slice_sample(n = 100) %>%
  dplyr::select(x, y) %>%
  dplyr::mutate(dplyr::across(.fns = ~ .x %>% as.character() %>% as.numeric())) %>%
  dplyr::mutate(object_id = 1:100) %>%
  dplyr::select(object_id, x, y) ->
sample_geo

openxlsx::write.xlsx(sample_geo, "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/Cluster Investigations/Cluster_Investigations_all cases_09302020/geo_sample.xlsx")

install.packages("rgdal")


nbs %>%
  dplyr::filter(
    patient_last_name == "Wright" &
      stringr::str_detect(patient_first_name, pattern = "Michael") &
      specimen_coll_dt >= as.Date("2020-09-01")
    ) %>%
  dplyr::select(patient_dob)

nbs$dob
