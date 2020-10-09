# Replace the NBS ID with a standardized one
coviData::replace_deaths_id()

# Cross-reference NBS linelist with Surveillance linelist and output differences
coviData::check_deaths(save = TRUE)
