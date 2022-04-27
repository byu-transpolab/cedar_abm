library(targets)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
source("R/get_files.R")

options(tigris_use_cache = TRUE)

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "tigris"))

pumas <- "4953001"
gtfs_url <- "http://www.fivecounty.utah.gov/transit/google_transit.zip"
geofabrik_url <- "https://download.geofabrik.de/north-america/us/utah-220426.osm.pbf"

# End this file with a list of target objects.
list(

  # Compile the r5 folder
  tar_target(bounding_box, get_bb(pumas)),
  tar_target(gtfs, get_gtfs(gtfs_url), format = "file"),
  tar_target(rawpbf, get_osmpbf(geofabrik_url, bounding_box), format = "file")

)
