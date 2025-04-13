# targets: quick access --------------------------------------------
# Sys.setenv(TAR_PROJECT = "data_prep")
# targets::tar_visnetwork(label = "branches") # view pipeline
# targets::tar_make() # run pipeline
#
# targets::tar_prune() # delete stored targets that are no longer part of the pipeline
# targets::tar_destroy() # delete saved pipeline outputs to be able to run the pipeline from scratch
# targets::tar_load() # load any pipeline output by name
# x <- targets::tar_read() # read any pipeline output into an R object
# read more at https://docs.ropensci.org/targets/ and https://books.ropensci.org/targets/

library(targets)

tar_option_set(
  format = "qs"
)

# source the R scripts
tar_source("R/data_prep/")

# parameters -------------------------------------------------------------
# you might want to change these according to the specs of the system you are using
options(global.max_mem_gb = 24)
options(global.max_n_cpu = parallelly::availableCores() - 1)
options(global.spanishoddata_data_path = "data/input/mitms-data/")
spanishoddata::spod_set_data_dir(getOption("global.spanishoddata_data_path"))
options(global.osm_path = "data/proc/osm")


list(
  # case study - Valencia

  tar_target(
    name = valencia_od_data_files,
    packages = c("spanishoddata"),
    command = case_study_valencia_predownload_od_data(
      data_dir = getOption("global.spanishoddata_data_path")
    )
  ),

  tar_target(
    name = valencia_osm,
    packages = c("osfr", "sf"),
    command = get_cached_osm_data_from_zenodo(
      osm_path = getOption("global.osm_path"),
      zenodo_url = "https://zenodo.org/records/15207223/files/geofabrik_valencia-latest.osm.pbf?download=1"
    ),
    format = "file"
  ),

  tar_target(
    name = valencia_od_and_zones,
    packages = c("spanishoddata", "dplyr", "sf"),
    command = case_study_valencia_preaggregate_od_data(
      data_dir = "data/input/mitms-data/",
      max_mem_gb = getOption("global.max_mem_gb"),
      max_n_cpu = getOption("global.max_n_cpu"),
      od_data_files = valencia_od_data_files
    )
  ),

  tar_target(
    name = valencia_data_for_plots,
    packages = c("tidyverse", "sf", "od", "osmactive"),
    command = case_study_valencia_biking_potential_prep(
      valencia_od_and_zones = valencia_od_and_zones,
      osm_path = getOption("global.osm_path"),
      cached_osm = valencia_osm
    )
  ),

  # case study - Madrid

  tar_target(
    name = madrid_od_data_files,
    packages = c("spanishoddata"),
    command = case_study_madrid_predownload_od_data(
      data_dir = getOption("global.spanishoddata_data_path")
    )
  ),

  tar_target(
    name = madrid_data_for_plots,
    packages = c("spanishoddata", "sf", "tidyverse"),
    command = case_study_madrid_preaggregate_od_data(
      data_dir = "data/input/mitms-data/",
      max_mem_gb = getOption("global.max_mem_gb"),
      max_n_cpu = getOption("global.max_n_cpu"),
      od_data_files = madrid_od_data_files
    )
  )
)
