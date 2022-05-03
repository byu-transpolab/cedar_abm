library(targets)
library(future)
library(future.apply)
library(future.callr)
plan(callr)
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
source("R/skim.R")
source("R/synpop.R")

options(tigris_use_cache = TRUE)
options(java.parameters = '-Xmx16G')

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "tigris", "r5r", "sf", "omxr", "tidycensus"))

pumas <- "4953001"
gtfs_url <- "http://www.fivecounty.utah.gov/transit/google_transit.zip"
geofabrik_url <- "https://download.geofabrik.de/north-america/us/utah-220426.osm.pbf"

# periods
periods <- c(
  "ea" = as.POSIXct("04-05-2022 04:00:00", format = "%d-%m-%Y %H:%M:%S"),
  "am" = as.POSIXct("04-05-2022 08:00:00", format = "%d-%m-%Y %H:%M:%S"),
  "md" = as.POSIXct("04-05-2022 12:00:00", format = "%d-%m-%Y %H:%M:%S"),
  "pm" = as.POSIXct("04-05-2022 17:00:00", format = "%d-%m-%Y %H:%M:%S"),
  "ev" = as.POSIXct("04-05-2022 22:00:00", format = "%d-%m-%Y %H:%M:%S")
)

# End this file with a list of target objects.
list(

  ## Geographic boundaries based on pumas attribute ============
  tar_target(bounding_box, get_bb(pumas)),
  tar_target(bg, get_bg(pumas)),
  tar_target(bg_from_0, renumber_bg(bg)),
  tar_target(tracts, unique(substr(bg$GEOID, 1, 11))),
  tar_target(counties, unique(substr(tracts, 1, 5))),

  ## Skims =====================================================
  # Compile the r5 folder
  tar_target(gtfs, get_gtfs(gtfs_url), format = "file"),
  tar_target(rawpbf, get_osmpbf(geofabrik_url, bounding_box), format = "file"),
  tar_target(r5dir, dirname(c(rawpbf, gtfs))),



  # calculate initial skims
  tar_target(skim_walk, get_walk_skim(r5dir[1], bg)),
  tar_target(skim_bike, get_bike_skim(r5dir[1], bg)),
  tar_target(skim_auto, get_auto_skim(r5dir[1], bg, periods)),
  tar_target(skim_tran, get_tran_skim(r5dir[1], bg, periods)),
  tar_target(skims, make_skims(bg_from_0, skim_walk, skim_bike, skim_auto, skim_tran),
             format = "file"),


  ## Synthetic Population =======================================
  # The only block-level control we get is the number of households in each
  # block
  # tar_target(bk_control, get_bk_control(se, crosswalk)),

  # tract controls include most demographic attributes:
  # The attributes available from the ACS include the following:
  # - Household size, derived from Table `B08202: HOUSEHOLD SIZE BY NUMBER OF WORKERS IN HOUSEHOLD`
  # - Household workers, derived from the same table
  # - Age, derived from Table `B01001: SEX BY AGE`
  # - Income, derived from Table `B19001:HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2018 INFLATION-ADJUSTED DOLLARS)`
  tar_target(acsvars, load_variables(2019, "acs5", cache = TRUE)),
  tar_target(sizes, get_sizework_controls(acsvars, counties)),
  tar_target(incs, get_income_controls(acsvars, counties)),
  tar_target(ages, get_age_controls(acsvars, counties)),
  tar_target(tract_controls, make_controls(tracts, ages, incs, sizes)),
  tar_target(meta, get_meta(tract_controls)),



  #' Seed files
  #' this function gets the relevant pums for all the pumas described,
  #' but only returns the first one in targets
  tar_target(pp_seed_file, get_pums(pumas, c("h", "p")), format = "file"),
  tar_target(seed, make_seed(pp_seed_file, pumas))



)
