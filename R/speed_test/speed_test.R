# speed testing function
run_speed_test <- function(test_task, parameters_list, test_function, spanishoddata_data_path, ...) {
  
  mem <- parameters_list$mem
  ncpu <- parameters_list$ncpu
  data_type <- parameters_list$data_type


  start_time <- Sys.time()
  cat("Start time:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Running test:", test_task, "with", mem, "GB and", ncpu, "threads on", data_type, "\n")

  spanishoddata::spod_set_data_dir(spanishoddata_data_path)
  start_time <- Sys.time()
  tic(quiet = TRUE)
  
  result <- tryCatch({
    # Connect based on data_type
    db <- switch(
      data_type,
      "duckdb" = spod_connect(
        file.path(spanishoddata_data_path, "clean_data", "v1", "tabular", "duckdb", "od_distritos.duckdb"),
        max_mem_gb = mem, max_n_cpu = ncpu
      ),
      "parquet" = spod_connect(
        file.path(spanishoddata_data_path, "clean_data", "v1", "tabular", "parquet", "od_distritos"),
        max_mem_gb = mem, max_n_cpu = ncpu
      ),
      "csv" = spod_get(
        type = "od", zones = "distr", dates = "cached_v1",
        max_mem_gb = mem, max_n_cpu = ncpu
      ),
      stop("Unknown data_type")
    )
    
    time_result <- system.time({
      summary_table <- tryCatch({
        tmp <- test_function(db) |> collect()
      }, error = function(e) {
        message("Error during data collection: ", e$message)
        return(NULL)
      })
    })
    
    if (is.null(summary_table)) {
      return(data.frame(
        test_task = test_task,
        memory_gb = mem,
        n_threads = ncpu,
        data_type = data_type,
        start_time = format(start_time, "%Y-%m-%d %H:%M:%S"),
        end_time = NA,
        elapsed_time_sec = NA,
        user_time_sec = NA,
        system_time_sec = NA,
        tt_elapsed_time = NA
      ))
    }
    
    tt_time <- toc(quiet = TRUE)
    end_time <- Sys.time()
    spod_disconnect(db, free_mem = TRUE)
    
    data.frame(
      test_task = test_task,
      memory_gb = mem,
      n_threads = ncpu,
      data_type = data_type,
      start_time = format(start_time, "%Y-%m-%d %H:%M:%S"),
      end_time = format(end_time, "%Y-%m-%d %H:%M:%S"),
      elapsed_time_sec = time_result["elapsed"],
      user_time_sec = time_result["user.self"],
      system_time_sec = time_result["sys.self"],
      tt_elapsed_time = as.numeric(tt_time$toc - tt_time$tic)
    )
  }, error = function(e) {
    message("Error occurred: ", e$message)
    data.frame(
      test_task = test_task,
      memory_gb = mem,
      n_threads = ncpu,
      data_type = data_type,
      start_time = format(start_time, "%Y-%m-%d %H:%M:%S"),
      end_time = NA,
      elapsed_time_sec = NA,
      user_time_sec = NA,
      system_time_sec = NA,
      tt_elapsed_time = NA
    )
  })
  
  end_time <- Sys.time()
  cat("End time:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")

  result
}

speed_test_mean_hourly_trips_by_weekday_od_pair <- function(db) {
  db |> 
    mutate(weekday = wday(date, label = TRUE, week_start = 1)) |>
    group_by(date, weekday, id_origin, id_destination) |>
    summarise(n_trips = sum(n_trips, na.rm = TRUE), .groups = "drop") |> 
    group_by(weekday, id_origin, id_destination) |>
    summarise(mean_daily_trips = mean(n_trips, na.rm = TRUE), .groups = "drop")
}
