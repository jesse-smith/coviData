import::from(magrittr, `%>%`)

# Base URI for all requests
essence_ts_uri <- "https://essence.tn.gov/tn_state/api/timeSeries"

# Total ED Visit Query
ed_total_query <- list(
  datasource = "va_hosp",
  percentParam = "noPercent",
  geographySystem = "hospital",
  publicHealthRegionByHospital = "memphis-shelby",
  medicalGroupingSystem = "essencesyndromes",
  timeResolution = "daily",
  detector = "probrepswitch",
  stratVal = "",
  multiStratVal = "",
  graphOnly = "true",
  numSeries = "0",
  graphOptions = "multipleSmall",
  seriesPerYear = "false",
  startMonth = "January",
  nonZeroComposite = "false",
  startDate = format(as.Date("2020-03-01"), "%d%b%y"),
  endDate = format(Sys.Date(), "%d%b%y")
)

httr::GET(
  url = essence_ts_uri,
  query = ed_total_query,
  httr::authenticate(
    user = Sys.getenv("essence_tn_usr"),
    password = Sys.getenv("essence_tn_pwd")
  )
) %>%
  httr::content(as = "text") %>%
  jsonlite::fromJSON() %>%
  .[["timeSeriesData"]] %>%
  dplyr::as_tibble() %>%
  dplyr::transmute(
    date = as.Date(date),
    count = as.integer(count)
  )

