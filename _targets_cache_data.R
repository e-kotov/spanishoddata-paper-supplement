# targets: quick access --------------------------------------------
# Sys.setenv(TAR_PROJECT = "cache_data")
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
tar_source("R/cache_data/")

# parameters -------------------------------------------------------------
options(global.osm_path = "data/proc/osm")


list(
  tar_target(
    name = cached_osm_data,
    packages = c("osmextract", "fs"),
    command = cache_osm_data(
      osm_path = getOption("global.osm_path")
    ),
    format = "file"
  )
)
