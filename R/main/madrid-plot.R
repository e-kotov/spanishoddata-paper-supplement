case_study_madrid_work_v_nonwork_trips_plot <- function(
  madrid_data_for_plots,
  plots_output_dir = "plots/main-plots"
) {
  od_trip_types_morning <- madrid_data_for_plots$od_trip_types_morning
  od_trip_types_evening <- madrid_data_for_plots$od_trip_types_evening
  zones_selected_fua <- madrid_data_for_plots$zones_selected_fua
  nodes <- madrid_data_for_plots$nodes

  # Create base map
  base_plot_districts <- ggplot() +
    geom_sf(
      data = zones_selected_fua |> st_transform(4326),
      fill = NA,
      col = "grey40",
      linewidth = 0.1
    ) +
    labs(title = "", subtitle = "", fill = "", caption = "") +
    theme(
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white", color = NA),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    guides(fill = "none")

  # Group OD flows
  od_home_work_study_morning <- od_trip_types_morning |>
    filter(
      activity_origin %in%
        c("home", "work_or_study") &
        activity_destination %in% c("home", "work_or_study")
    ) |>
    group_by(o, d) |>
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

  od_frequent_morning <- od_trip_types_morning |>
    filter(
      activity_origin == "frequent_activity" |
        activity_destination == "frequent_activity"
    ) |>
    group_by(o, d) |>
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

  od_infrequent_morning <- od_trip_types_morning |>
    filter(
      activity_origin == "infrequent_activity" |
        activity_destination == "infrequent_activity"
    ) |>
    group_by(o, d) |>
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

  od_home_work_study_evening <- od_trip_types_evening |>
    filter(
      activity_origin %in%
        c("home", "work_or_study") &
        activity_destination %in% c("home", "work_or_study")
    ) |>
    group_by(o, d) |>
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

  od_frequent_evening <- od_trip_types_evening |>
    filter(
      activity_origin == "frequent_activity" |
        activity_destination == "frequent_activity"
    ) |>
    group_by(o, d) |>
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

  od_infrequent_evening <- od_trip_types_evening |>
    filter(
      activity_origin == "infrequent_activity" |
        activity_destination == "infrequent_activity"
    ) |>
    group_by(o, d) |>
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

  # Flowmap builder
  create_flowmap <- function(od, nodes, edge_width_factor = 1) {
    base_plot_districts |>
      flowmapper::add_flowmap(
        od = od,
        nodes = nodes,
        node_radius_factor = 1,
        edge_width_factor = edge_width_factor,
        arrow_point_angle = 65,
        node_buffer_factor = 1.5,
        outline_col = "grey80",
        add_legend = "bottom",
        legend_col = "gray20",
        legend_gradient = TRUE,
        k_node = 20
      ) +
      scale_fill_gradient(
        low = "#FABB29",
        high = "#AB061F",
        labels = scales::comma_format()
      ) +
      theme(
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 14, face = "bold")
      )
  }

  # Plots with adjusted width factors
  p_home_work_study_morning <- create_flowmap(
    od_home_work_study_morning,
    nodes,
    edge_width_factor = 1 / 2.77
  ) +
    labs(title = "A:\nHome <-> Work/Study", subtitle = "Morning (7–11) trips")

  p_frequent_morning <- create_flowmap(
    od_frequent_morning,
    nodes,
    edge_width_factor = 1
  ) +
    labs(
      title = "B:\nHome/Work/Study <-> Frequent activity",
      subtitle = "Morning (7–11) trips"
    )

  p_infrequent_morning <- create_flowmap(
    od_infrequent_morning,
    nodes,
    edge_width_factor = 1 / 2.42
  ) +
    labs(
      title = "C:\nHome/Work/Study <-> Infrequent activity",
      subtitle = "Morning (7–11) trips"
    )

  p_home_work_study_evening <- create_flowmap(
    od_home_work_study_evening,
    nodes,
    edge_width_factor = 1 / 4.13
  ) +
    labs(title = "D:\nHome <-> Work/Study", subtitle = "Evening (17–21) trips")

  p_frequent_evening <- create_flowmap(
    od_frequent_evening,
    nodes,
    edge_width_factor = 1 / 1.07
  ) +
    labs(
      title = "E:\nHome/Work/Study <-> Frequent activity",
      subtitle = "Evening (17–21) trips"
    )

  p_infrequent_evening <- create_flowmap(
    od_infrequent_evening,
    nodes,
    edge_width_factor = 1 / 1.83
  ) +
    labs(
      title = "F:\nHome/Work/Study <-> Infrequent activity",
      subtitle = "Evening (17–21) trips"
    )

  # Combine into 6-panel plot
  p_compare <- (p_home_work_study_morning +
    p_frequent_morning +
    p_infrequent_morning) /
    (p_home_work_study_evening + p_frequent_evening + p_infrequent_evening) +
    plot_annotation(
      caption = "Mobility flows in Madrid area\nMean over a week of February 6–10, 2023",
      theme = theme(plot.caption = element_text(size = 12))
    )

  # Save
  if (!fs::dir_exists(plots_output_dir)) {
    fs::dir_create(plots_output_dir)
  }

  plot_save_path <- paste0(plots_output_dir, "/fig-02-madrid.jpeg")

  ggsave(
    plot_save_path,
    plot = p_compare,
    width = 12,
    height = 14,
    units = "in",
    dpi = 300,
    create.dir = TRUE
  )

  return(plot_save_path)
}
