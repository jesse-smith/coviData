library(jsonlite)
library(httr)
library(tidyverse)
library(lubridate)

# Base URI
api_uri <- "https://redcap.health.tn.gov/redcap/api/";
api_token <- Sys.getenv("redcap_api_token")

# General API parameters
api_params <- list(
  token = api_token
  )

# Add field to get date updated
api_date_params <- list(
  token        = api_token,
  content      = "record",
  format       = "json",
  type         = "flat",
  records      = "MSR",
  fields       = "date_updated",
  returnFormat = "json"
)

# Add form to get investigations data
api_nbs_params <- list(
  token        = api_token,
  content      = "file",
  action       = "export",
  record       = "MSR",
  field        = "nbs_daily_upload",
  returnFormat = "json"
)

# Checking date updated - no use downloading data we already have
httr::POST(api_uri, body = api_date_params) %>%
  httr::content(as = "text") %>%
  jsonlite::fromJSON() %>%
  purrr::as_vector() %>%
  lubridate::as_date() ->
date_updated



# Create temporary directory for new files
if (!dir.exists("temp")) {
  dir.create("temp")
} else {
  files <- list.files("temp")
  if (length(files) != 0) file.remove(list(files))
}

# Downloading most recent investigations file
httr::POST(
  api_uri,
  body = api_nbs_params,
  httr::write_disk("temp/nbs.zip"),
  httr::progress()
)

# Unzip new file
unzip("temp/nbs.zip")

# Rename new file - can move at this step as well
temp_nbs <- paste0("temp/", Sys.Date(), " Final Data Pull.csv")
file.rename(from = "temp/MSR INVS.csv", to = temp_nbs)

# Move new file
file.copy(from = temp_nbs, to = "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/Sandbox data pull Final/")

# Read columns as character, convert using all rows
read_csv("MSR INVS.csv")

