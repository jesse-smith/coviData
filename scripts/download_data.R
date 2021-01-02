library(coviData)

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
Sys.sleep(3) # Give Excel time to release files (3 seconds)
download_pcr_snapshot()
Sys.sleep(3) # Give R enough time to delete temporary files (3 seconds)
convert_pcr_snapshot()
Sys.sleep(3) # Give Excel time to release files (3 seconds)
download_serology_snapshot()
Sys.sleep(3) # Give R enough time to delete temporary files (3 seconds)
download_antigen_snapshot()
Sys.sleep(3) # Give R enough time to delete temporary files (3 seconds)

# Check AEL as last step
# check_ael()
