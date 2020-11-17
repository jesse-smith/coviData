# Get file info for final data pull folder
# - Prefer latest birth time if multiple files have the same date

# Read file using vroom and process with dplyr
# - Set `id = "path"` to get report date from file name
# - Read only `INV_LOCAL_ID` and `SPECIMEN_COLL_DT`; set types as character and
#   date
# - Parse `path` to get report date

# Create empty tibble `data` with `INV_LOCAL_ID` and `REPORT_DATE` variables

# Read file `new_data`

# Right join data and new_data on inv_local_id

# Coalesce report dates

# Save to data

# Iterate

