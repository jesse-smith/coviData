shelby_zip <- readr::read_csv("https://www2.census.gov/geo/docs/maps-data/data/rel/zcta_county_rel_10.txt") %>%
  dplyr::filter(STATE == 47L, COUNTY == 157L) %>%
  dplyr::pull(ZCTA5)

usethis::use_data(shelby_zip, overwrite = TRUE)
