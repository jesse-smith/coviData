library(coviData)
# Download data
download_nbs_snapshot(convert = TRUE)
download_pcr_snapshot(convert = TRUE)
download_antigen_snapshot(convert = FALSE)
download_serology_snapshot(convert = FALSE)
download_ael()
replace_ael()
check_ael()

# Read data
nbs <- load_nbs(Sys.Date())
pcr <- load_pcr(Sys.Date())
ael <- load_ael(Sys.Date())

# Next, merge with AEL

merged_nbs %>%
  dplyr::mutate(
    lab_result = relevel_pcr(lab_result),
    collect_date_join = lubridate::as_date(collect_date),
    patient_last_name =  as.character(patient_last_name),
    patient_first_name = as.character(patient_first_name),
    patient_dob = lubridate::as_date(patient_dob),
    patient_street_addr_1 = as.character(patient_street_addr_1),
    patient_zip = as.character(patient_zip)
  ) ->
pre_mnbs

ael %>%
  dplyr::mutate(
    lab_result = relevel_pcr(result),
    collect_date_join = lubridate::as_date(collect_date),
    patient_last_name = as.character(patient_last_name),
    patient_first_name = as.character(patient_first_name),
    patient_dob = lubridate::as_date(pt_dob),
    patient_street_addr_1 = as.character(pt_add1),
    patient_zip = as.character(pt_zipcode)
  ) ->
pre_ael

pre_mnbs %>%
  dplyr::filter(perform_facility_name %in% c("Ael", "Ameresolabcentcntr")) %>%
  dplyr::full_join(
    y = pre_ael,
    by = c("patient_last_name", "patient_first_name", "patient_dob")
  )
