library(coviData)

# Download and process AEL data - this will be available first
download_ael()
Sys.sleep(1) # Give R time to release file (1 second)
replace_ael()
Sys.sleep(1) # Give R time to release file (1 second)

# Download snapshots from "Data for Regions" REDcap project
# Includes:
#   - NBS snapshot
#   - PCR labs snapshot
#   - Serology labs snapshot
#   - Antigen labs snapshot
#
# The NBS and PCR functions will open Excel for you to save as .xlsx; to do this
# with any of the `download_*`functions, set `convert = TRUE`.
download_nbs_snapshot(convert = TRUE)
Sys.sleep(60) # Give Excel time to open (1 minute)
download_pcr_snapshot(convert = TRUE)
Sys.sleep(5) # Give R enough time to delete temporary files (5 seconds)
download_serology_snapshot()
Sys.sleep(5) # Give R enough time to delete temporary files (5 seconds)
download_antigen_snapshot()
Sys.sleep(5) # Give R enough time to delete temporary files (5 seconds)

# Check AEL as last step
check_ael()
