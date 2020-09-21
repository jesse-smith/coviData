library(jsonlite)
library(httr)
library(tidyverse)
library(lubridate)

# Rename new file - can move at this step as well
temp_nbs <- paste0("temp/", Sys.Date(), " Final Data Pull.csv")
file.rename(from = "temp/MSR INVS.csv", to = temp_nbs)

# Move new file
file.copy(from = temp_nbs, to = "V:/EPI DATA ANALYTICS TEAM/COVID SANDBOX REDCAP DATA/Sandbox data pull Final/")

# Read columns as character, convert using all rows
read_csv("MSR INVS.csv")
