# targets: quick access --------------------------------------------
# Sys.setenv(TAR_PROJECT = "speed_test")
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

tar_source("R/speed_test/")


# parameters -------------------------------------------------------------
# you might want to change these according to the specs of the system you are using
# options for conversion process
options(global.max_mem_gb = 200) # adjust to available resources
options(global.max_n_cpu = 64) # adjust to available resources
options(global.spanishoddata_data_path = "data/input/mitms-data/")
spanishoddata::spod_set_data_dir(getOption("global.spanishoddata_data_path"))

# options for speed test
mem_to_test <- c(128, 64, 32, 16, 8, 4) # adjust to available resources
ncpu_to_test <- c(2, 4, 8, 16, 24, 32, 64) # adjust to available resources

# targets ----------------------------------------------------------------
list(
  # Download origin-destination data ---------------------------------------
  tar_target(
    name = od_data_csv,
    packages = c("spanishoddata", "fs"),
    command = spanishoddata::spod_download(
      type = "od",
      zones = "distr",
      dates = spanishoddata::spod_get_valid_dates(ver = 1),
      max_download_size_gb = 200,
      return_local_file_paths = TRUE,
      data_dir = getOption("global.spanishoddata_data_path")
    ),
    format = "file"
  ),

  # Convert downloaded data to parquet format ------------------------------
  tar_target(
    name = od_data_parquet,
    packages = c("spanishoddata", "fs"),
    command = {
      invisible(od_data_csv)
      spanishoddata::spod_convert(
        type = "od",
        zones = "distr",
        dates = "cached_v1",
        save_format = "parquet",
        data_dir = getOption("global.spanishoddata_data_path"),
        overwrite = TRUE,
        max_mem_gb = getOption("global.max_mem_gb"),
        max_n_cpu = getOption("global.max_n_cpu")
      )
    },
    format = "file"
  ),

  # # Convert downloaded data to duckdb format -------------------------------
  tar_target(
    name = od_data_duckdb,
    packages = c("spanishoddata", "fs"),
    command = {
      invisible(od_data_csv)
      spanishoddata::spod_convert(
        type = "od",
        zones = "distr",
        dates = "cached_v1",
        save_format = "duckdb",
        data_dir = getOption("global.spanishoddata_data_path"),
        overwrite = TRUE,
        max_mem_gb = getOption("global.max_mem_gb"),
        max_n_cpu = getOption("global.max_n_cpu")
      )
    }
  ),

  # Run speed test --------------------------------------------------------
  tar_target(
    name = speed_test_params_grid,
    command = expand.grid(
      mem = mem_to_test,
      ncpu = ncpu_to_test,
      data_type = c("duckdb", "parquet", "csv"),
      stringsAsFactors = FALSE
    ),
    iteration = "list"
  ),

  tar_target(
    name = parameters_list,
    command = split(
      speed_test_params_grid,
      seq_len(nrow(speed_test_params_grid))
    ),
    iteration = "list"
  ),

  tar_target(
    name = speed_test_result,
    packages = c(
      "spanishoddata",
      "tidyverse",
      "lubridate",
      "progress",
      "tictoc"
    ),
    command = run_speed_test(
      test_task = "mean houlry trips by weekday over 18 months for each OD pair",
      parameters_list = parameters_list,
      test_function = speed_test_mean_hourly_trips_by_weekday_od_pair,
      spanishoddata_data_path = getOption("global.spanishoddata_data_path"),
      od_data_csv,
      od_data_parquet,
      od_data_duckdb
    ),
    pattern = map(parameters_list),
    iteration = "list"
  ),

  tar_target(
    name = speed_test_summary,
    command = dplyr::bind_rows(speed_test_result)
  ),

  tar_target(
    name = speed_test_summary_csv,
    packages = c("readr", "fs"),
    command = {
      speed_test_summary_csv <- "supplement-data/speed_test_summary.csv"
      readr::write_csv(
        speed_test_summary,
        speed_test_summary_csv,
        append = FALSE
      )
      if (fs::file_exists(speed_test_summary_csv)) {
        speed_test_summary_csv
      } else {
        FALSE
      }
    },
    format = "file"
  )
)
