library(coviData)

# Download and process AEL data - this will be available first
download_ael()
Sys.sleep(3) # Give R time to release file (3 second)
replace_ael()
Sys.sleep(3) # Give R time to release file (3 second)

# Download snapshots from "Data for Regions" REDcap project
# Includes:
#   - NBS snapshot
#   - PCR labs snapshot
#   - Serology labs snapshot
#   - Antigen labs snapshot
#
# The NBS and PCR functions will open Excel for you to save as .xlsx; to do this
# with any of the `download_*` functions, set `convert = TRUE`.
download_nbs_snapshot()
Sys.sleep(3) # Give R enough time to delete temporary files (3 seconds)
convert_nbs_snapshot()
Sys.sleep(30) # Give Excel time to open (30 seconds)
download_pcr_snapshot()
Sys.sleep(3) # Give R enough time to delete temporary files (3 seconds)
convert_pcr_snapshot()
Sys.sleep(30) # Give Excel time to open (30 seconds)
download_serology_snapshot()
Sys.sleep(3) # Give R enough time to delete temporary files (3 seconds)
download_antigen_snapshot()
Sys.sleep(3) # Give R enough time to delete temporary files (3 seconds)

# Check AEL as last step
check_ael()
