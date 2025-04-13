# Case Study 1: Assessing Cycling Potential and Infrastructure Provision in Valencia
get_cached_osm_data_from_zenodo <- function(
  osm_path = getOption("global.osm_path"),
  zenodo_url = "https://zenodo.org/records/15207223/files/geofabrik_valencia-latest.osm.pbf?download=1"
) {
  file_save_path <- fs::path(osm_path, "geofabrik_valencia-latest.osm.pbf")
  if (fs::file_exists(file_save_path)) {
    return(file_save_path)
  } else {
    if (!fs::dir_exists(osm_path)) {
      fs::dir_create(osm_path, recurse = TRUE)
    }
    result <- download.file(zenodo_url, destfile = file_save_path, mode = "wb")
    if (result == 0) {
      return(file_save_path)
    } else {
      stop("Download failed")
    }
  }
}

case_study_valencia_predownload_od_data <- function(
  data_dir = getOption("global.spanishoddata_data_path")
) {
  spod_set_data_dir(data_dir)

  od_files <- spod_download(
    type = "origin-destination",
    zones = "districts",
    dates = c(start = "2024-05-05", end = "2024-05-11"),
    max_download_size_gb = 2,
    return_local_file_paths = TRUE
  )

  od_files <- stringr::str_extract(
    od_files[[1]],
    pattern = paste0(data_dir, ".*")
  )

  return(od_files)
}

case_study_valencia_preaggregate_od_data <- function(
  data_dir = getOption("global.spanishoddata_data_path"),
  max_mem_gb = getOption("global.max_mem_gb"),
  max_n_cpu = getOption("global.max_n_cpu"),
  od_data_files
) {
  spod_set_data_dir(data_dir)

  # Load OD data for potential weekly cycling trips
  od_db <- spod_get(
    type = "origin-destination",
    zones = "districts",
    dates = c(start = "2024-05-05", end = "2024-05-11"),
    max_download_size_gb = 2,
    max_mem_gb = max_mem_gb,
    max_n_cpu = max_n_cpu
  )

  # Aggregate trips by origin-destination
  od_national_aggregated <- od_db |>
    filter(distance %in% c("0.5-2", "2-10")) |> # Trips within 0.5-10 km
    group_by(id_origin, id_destination) |>
    summarise(trips = sum(n_trips, na.rm = TRUE), .groups = "drop") |>
    collect()

  districts_valencia <- spod_get_zones("dist", ver = 2) |>
    filter(grepl("Val\u00E8ncia", name, ignore.case = TRUE)) |>
    st_transform(districts_valencia, crs = 4326)
  st_geometry(districts_valencia) <- "geometry"

  valencia_od_and_zones <- list(
    od_national_aggregated = od_national_aggregated,
    districts_valencia = districts_valencia
  )
  return(valencia_od_and_zones)
}

case_study_valencia_biking_potential_prep <- function(
  valencia_od_and_zones = valencia_od_and_zones,
  osm_path = getOption("global.osm_path"),
  cached_osm
) {
  # Intra-zonal analysis: Focus on trips within the same zone (district)
  od_national_aggregated <- valencia_od_and_zones$od_national_aggregated
  districts_valencia <- valencia_od_and_zones$districts_valencia

  # Keep only intra-zonal trips
  national_intrazonal <- od_national_aggregated |>
    filter(id_origin == id_destination)

  # Extract district IDs for Valencia
  ids_valencia <- districts_valencia$id

  # Filter intra-zonal data for València
  valencia_intrazonal <- national_intrazonal |>
    filter(id_origin %in% ids_valencia, id_destination %in% ids_valencia)

  # Convert to spatial format
  valencia_intrazonal_sf <- left_join(
    districts_valencia[, c("id")],
    valencia_intrazonal,
    by = c("id" = "id_origin")
  )

  # Cycle net data extraction

  # Merge district geometries to get Valencia's boundary
  valencia_boundary <- st_union(districts_valencia)

  # Set storage for osm data
  if (!fs::dir_exists(osm_path)) {
    fs::dir_create(osm_path, recurse = TRUE)
  }
  Sys.setenv(OSMEXT_DOWNLOAD_DIRECTORY = osm_path)

  # Download and clip the OSM network within Valencia
  osm_full <- osmactive::get_travel_network(valencia_boundary)
  osm <- osm_full[valencia_boundary, ]

  # Extract road networks
  drive_net <- osmactive::get_driving_network(osm) # All roads
  drive_net_major <- osmactive::get_driving_network_major(osm) # Major roads

  # Extract and classify cycling infrastructure
  cycle_net <- osmactive::get_cycling_network(osm)
  cycle_net <- osmactive::distance_to_road(cycle_net, drive_net_major) # Distance to major roads
  cycle_net <- osmactive::classify_cycle_infrastructure(cycle_net) # Categorize cycle lanes

  # Visualize the cycling network
  # map_net <- osmactive::plot_osm_tmap(cycle_net)
  # map_net

  # ---- Data required for plotting ----
  # Define bounding box with appropriate CRS
  bbox_valencia_zoom <- st_bbox(
    c(
      xmin = -0.45,
      ymin = 39.44,
      xmax = -0.30,
      ymax = 39.51
    ),
    crs = st_crs(districts_valencia)
  )

  # Inter-zonal analysis: Focus on trips between different zones

  # Keep only inter-zonal trips
  od_national_interzonal <- od_national_aggregated |>
    filter(id_origin != id_destination)

  # Filter OD data for Valencia
  od_valencia <- od_national_interzonal |>
    filter(id_origin %in% ids_valencia, id_destination %in% ids_valencia)

  # Convert OD data to spatial format
  od_valencia_sf <- od::od_to_sf(od_valencia, z = districts_valencia)

  # Function to extract trips through intermediate districts (i.e., trips passing through multiple districts)
  extract_trips_through_intermediate_districts <- function(od_data, districts) {
    expanded_trips <- list()

    # Compute adjacency matrix once
    adjacent_districts <- st_touches(districts, sparse = FALSE)

    # Create a mapping between district IDs and their row indices
    district_id_to_index <- setNames(seq_len(nrow(districts)), districts$id)

    for (i in 1:nrow(od_data)) {
      trip <- od_data[i, ]
      route_geom <- trip$geometry

      # Identify all intersected districts
      intersected_districts <- st_intersection(districts, route_geom)

      # Order by the length of the intersection
      intersected_districts <- intersected_districts[
        order(st_length(intersected_districts$geometry)),
      ]

      if (nrow(intersected_districts) >= 2) {
        district_ids <- intersected_districts$id

        # Generate district-to-district steps, ensuring they are adjacent
        for (j in seq_along(district_ids)[-length(district_ids)]) {
          origin_idx <- district_id_to_index[district_ids[j]]
          destination_idx <- district_id_to_index[district_ids[j + 1]]

          # Ensure adjacency using numeric indices
          if (adjacent_districts[origin_idx, destination_idx]) {
            new_trip <- data.frame(
              id_origin = district_ids[j],
              id_destination = district_ids[j + 1],
              trips = trip$trips, # Keep original trip count
              stringsAsFactors = FALSE
            )
            expanded_trips <- append(expanded_trips, list(new_trip))
          }
        }
      }
    }

    # Convert list to data frame
    if (length(expanded_trips) > 0) {
      expanded_trips_df <- do.call(rbind, expanded_trips)

      # Sum trips for identical OD pairs
      expanded_trips_df <- expanded_trips_df |>
        group_by(id_origin, id_destination) |>
        summarise(trips = sum(trips), .groups = "drop")

      return(expanded_trips_df)
    } else {
      return(data.frame(
        id_origin = integer(),
        id_destination = integer(),
        trips = integer()
      ))
    }
  }

  # Run function on Valencia OD data
  adjacent_od_valencia <- extract_trips_through_intermediate_districts(
    od_valencia_sf,
    districts_valencia
  )

  # Join geometry of origin districts
  adjacent_od_valencia <- adjacent_od_valencia |>
    left_join(
      districts_valencia |> select(id, geometry),
      by = c("id_origin" = "id")
    ) |>
    rename(origin_geom = geometry)

  # Join geometry of destination districts
  adjacent_od_valencia <- adjacent_od_valencia |>
    left_join(
      districts_valencia |> select(id, geometry),
      by = c("id_destination" = "id")
    ) |>
    rename(dest_geom = geometry)

  # Create line geometries by connecting centroids of origin and destination districts
  adjacent_od_valencia_sf <- adjacent_od_valencia |>
    mutate(
      geometry = st_sfc(
        map2(
          origin_geom,
          dest_geom,
          ~ st_cast(st_union(c(st_centroid(.x), st_centroid(.y))), "LINESTRING")
        ),
        crs = st_crs(districts_valencia)
      )
    ) |>
    select(id_origin, id_destination, trips, geometry) |>
    st_as_sf()

  # Return the adjacent od data with geometries
  # adjacent_od_valencia_sf

  # Compute number of cycle network connections among adjacent districts (nominator of our connectivity ratio)

  # Extract start and end coordinates of cycle paths
  line_coords <- st_coordinates(cycle_net)

  start_points_sf <- cycle_net |>
    mutate(
      start_x = line_coords[match(1, line_coords[, "L1"]), "X"],
      start_y = line_coords[match(1, line_coords[, "L1"]), "Y"]
    ) |>
    st_as_sf(coords = c("start_x", "start_y"), crs = st_crs(cycle_net))

  end_points_sf <- cycle_net |>
    mutate(
      end_x = line_coords[
        match(max(line_coords[, "L1"]), line_coords[, "L1"]),
        "X"
      ],
      end_y = line_coords[
        match(max(line_coords[, "L1"]), line_coords[, "L1"]),
        "Y"
      ]
    ) |>
    st_as_sf(coords = c("end_x", "end_y"), crs = st_crs(cycle_net))

  # Assign start and end districts to cycle paths
  start_areas <- st_join(
    st_transform(start_points_sf, st_crs(districts_valencia)),
    districts_valencia
  ) |>
    select(osm_id, start_area = id) |>
    st_drop_geometry()

  end_areas <- st_join(
    st_transform(end_points_sf, st_crs(districts_valencia)),
    districts_valencia
  ) |>
    select(osm_id, end_area = id) |>
    st_drop_geometry()

  # Create edges between districts where cycle paths connect different areas
  edges <- start_areas |>
    inner_join(end_areas, by = "osm_id", relationship = "many-to-many") |>
    filter(!is.na(start_area) & !is.na(end_area) & start_area != end_area) |>
    select(start_area, end_area)

  # Count actual connections between adjacent districts
  edges_summary <- edges |>
    count(start_area, end_area, name = "connections") |>
    rename(o = start_area, d = end_area)

  # Check adjacency between areas using st_intersects() and filter adjacent areas
  edges_summary$are_adjacent <- mapply(
    function(o, d) {
      st_intersects(
        districts_valencia$geometry[districts_valencia$id == o],
        districts_valencia$geometry[districts_valencia$id == d],
        sparse = FALSE
      )
    },
    edges_summary$o,
    edges_summary$d
  )

  # Filter only adjacent areas
  edges_summary_adjacent <- edges_summary |>
    filter(are_adjacent) |>
    select(-are_adjacent) # Remove adjacency column if not needed

  # View result sorted by the number of connections
  # head(
  #   edges_summary_adjacent |>
  #     arrange(desc(connections))
  # )

  # Create a data frame with the length of the border shared by adjacent districts (denominator of our connectivity ratio)

  # Extract district boundaries
  districts_boundary <- st_boundary(districts_valencia[, "id"])

  # Identify adjacent districts
  adjacent_pairs <- which(
    st_touches(districts_valencia, sparse = FALSE),
    arr.ind = TRUE
  )

  # Function to calculate shared boundary length between two districts
  calculate_shared_boundary_length <- function(i, j) {
    # Access the i-th and j-th district boundaries
    boundary_i <- districts_boundary[i, ]
    boundary_j <- districts_boundary[j, ]
    # Calculate the shared boundary length
    shared_boundary <- st_intersection(boundary_i, boundary_j)
    return(st_length(shared_boundary))
  }

  # Apply the function to each pair of adjacent districts
  shared_lengths <- apply(adjacent_pairs, 1, function(pair) {
    calculate_shared_boundary_length(pair[1], pair[2])
  })

  # Create a data frame with origin-destination pairs and their shared border lengths
  border_lengths_df <- tibble(
    id_origin = districts_valencia$id[adjacent_pairs[, 1]],
    id_destination = districts_valencia$id[adjacent_pairs[, 2]],
    border_length = shared_lengths
  )

  # Verify the resulting data frame
  # border_lengths_df

  # Compute the ratio: connections/shared border length

  # Merge adjacency and border length data
  merged_table <- left_join(
    edges_summary_adjacent,
    border_lengths_df,
    by = c("o" = "id_origin", "d" = "id_destination")
  )

  # Merge with trips data
  merged_table <- left_join(
    merged_table,
    adjacent_od_valencia_sf,
    by = c("o" = "id_origin", "d" = "id_destination")
  )

  # Compute the ratio and filter out self-loops
  merged_table <- merged_table |>
    mutate(
      connections = replace_na(connections, 0),
      ratio = connections / border_length
    ) |>
    filter(o != d & border_length != 0) |>
    select(
      origin = o,
      destination = d,
      trips,
      border_length,
      connections,
      ratio
    )

  # Sort OD pairs by ratio
  # head(
  #   merged_table |>
  #     arrange(desc(ratio))
  # )

  # Final dataset! Convert OD pairs to spatial lines
  od_sf <- od_to_sf(merged_table, z = districts_valencia)

  # Return a list of objects used for plotting
  valencia_data_for_plots <- list(
    districts_valencia = districts_valencia,
    valencia_intrazonal_sf = valencia_intrazonal_sf,
    cycle_net = cycle_net,
    bbox_valencia_zoom = bbox_valencia_zoom,
    od_sf = od_sf
  )

  return(valencia_data_for_plots)
}
