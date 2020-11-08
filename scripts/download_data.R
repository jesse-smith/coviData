library(coviData)

# Download and process AEL data - this will be available first
download_ael()
replace_ael()
check_ael()

# Download snapshots from "Data for Regions" REDcap project
# Includes:
#   - NBS snapshot
#   - PCR labs snapshot
#   - Serology labs snapshot
#   - Antigen labs snapshot
#
# The NBS and PCR functions will open Excel for you to save as .xlsx; to do this
# with any of the functions, set `convert = TRUE`.
download_nbs_snapshot(convert = TRUE)
Sys.sleep(60) # Give Excel time to open (1 minute)
download_pcr_snapshot(convert = TRUE)
Sys.sleep(1) # Give R enough time to delete temporary files (1 second)
download_serology_snapshot()
Sys.sleep(1) # Give R enough time to delete temporary files (1 second)
download_antigen_snapshot()
