# Case Study 2: Exploring Work vs Non-work related trips in Madrid
case_study_madrid_predownload_od_data <- function(
  data_dir = getOption("global.spanishoddata_data_path")
) {
  spod_set_data_dir(data_dir)

  dates <- seq(as.Date("2023-02-06"), as.Date("2023-02-10"), by = "day")

  od_files <- spod_download(
    type = "origin-destination",
    zones = "districts",
    dates = dates,
    max_download_size_gb = 2,
    return_local_file_paths = TRUE
  )

  od_files <- stringr::str_extract(
    od_files[[1]],
    pattern = paste0(data_dir, ".*")
  )

  return(od_files)
}

case_study_madrid_preaggregate_od_data <- function(
  data_dir = getOption("global.spanishoddata_data_path"),
  max_mem_gb = getOption("global.max_mem_gb"),
  max_n_cpu = getOption("global.max_n_cpu"),
  od_data_files
) {
  spod_set_data_dir(data_dir)

  # set path to save DuckDB database
  duckdb_save_path <- file.path(
    spod_get_data_dir(),
    "/clean_data/v2/tabular/duckdb/od_flows_tmp.duckdb"
  )

  # set the dates
  dates <- seq(as.Date("2023-02-06"), as.Date("2023-02-10"), by = "day")

  # get and convert the flows do DuckDB database
  od_flows_db_file <- spod_convert(
    type = "od",
    zones = "distr",
    dates = dates,
    max_mem_gb = max_mem_gb,
    max_n_cpu = max_n_cpu,
    max_download_size_gb = 6,
    overwrite = TRUE,
    save_format = "duckdb",
    save_path = duckdb_save_path
  )

  # connect the database
  od_flows <- spanishoddata::spod_connect(
    data_path = duckdb_save_path,
    max_mem_gb = max_mem_gb,
    max_n_cpu = max_n_cpu
  )
  # spanishoddata::spod_disconnect(od_flows) # in case one needs to disconnect the database, e.g. to re-run the spod_convert() above

  # get the districts
  districts <- spod_get_zones("dist", ver = 2)

  # focus on Madrid
  zones_selected <- districts |>
    filter(grepl("Madrid distrito", name, ignore.case = TRUE))

  zones_selected_fua <- districts[st_buffer(zones_selected, dist = 5000), ]

  # prepare data for flowmapper
  nodes <- zones_selected_fua |>
    st_point_on_surface() |>
    st_transform(4326) |>
    st_coordinates() |>
    as.data.frame() |>
    mutate(name = zones_selected_fua$id) |>
    rename(x = X, y = Y) |>
    as_tibble()

  # preaggregate flows
  od_trip_types_morning <- od_flows |>
    filter(time_slot %in% c(7:11)) |>
    filter(id_origin %in% nodes$name & id_destination %in% nodes$name) |>
    group_by(
      o = id_origin,
      d = id_destination,
      date,
      activity_origin,
      activity_destination
    ) |>
    summarise(n_trips = sum(n_trips, na.rm = TRUE), .groups = "drop") |>
    group_by(o, d, activity_origin, activity_destination) |>
    summarise(value = sum(n_trips, na.rm = TRUE), .groups = "drop") |>
    collect() |>
    as_tibble()

  od_trip_types_evening <- od_flows |>
    filter(time_slot %in% c(17:21)) |>
    filter(id_origin %in% nodes$name & id_destination %in% nodes$name) |>
    group_by(
      o = id_origin,
      d = id_destination,
      date,
      activity_origin,
      activity_destination
    ) |>
    summarise(n_trips = sum(n_trips, na.rm = TRUE), .groups = "drop") |>
    group_by(o, d, activity_origin, activity_destination) |>
    summarise(value = sum(n_trips, na.rm = TRUE), .groups = "drop") |>
    collect() |>
    as_tibble()

  madrid_data_for_plots <- list(
    od_trip_types_morning = od_trip_types_morning,
    od_trip_types_evening = od_trip_types_evening,
    zones_selected_fua = zones_selected_fua,
    nodes = nodes
  )

  return(madrid_data_for_plots)
}
