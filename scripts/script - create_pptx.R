# Library loads packages - here, we're loading the `coviData` package
library(tidyverse)
library(coviData)
library(officer)

# officer functions are NOT pure; they modify the object you pass to them

# Load template
pptx <- officer::read_pptx("~/template.pptx")

# Add title slide
pptx <- add_slide(
  pptx,
  layout = "Title Slide",
  master = "HD Blue and White"
)

# Create title object
title <- glue::glue(
  "COVID-19 Daily Status Report",
  "\n\n",
  format(Sys.Date(), '%m/%d/%y')
)

# Add title to title slide
pptx <- ph_with(
  pptx,
  value = title,
  location = ph_location_label(ph_label = "Title 1")
)

# Add epi curve slide

# Add plot to epi curve slide

#
