# Case Study 2: Exploring Work vs Non-work related trips in Madrid
case_study_madrid_work_v_nonwork_trips_plot <- function(
  madrid_data_for_plots,
  plots_output_dir = "plots/main-plots"
) {
  od_trip_types_morning <- madrid_data_for_plots$od_trip_types_morning
  od_trip_types_evening <- madrid_data_for_plots$od_trip_types_evening
  zones_selected_fua <- madrid_data_for_plots$zones_selected_fua
  nodes <- madrid_data_for_plots$nodes

  # create the base plot with Madrid districts
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
  base_plot_districts

  # aggregate flows for flowmapper
  od_home_work_study_morning <- od_trip_types_morning |>
    filter(
      activity_origin %in%
        c("home", "work_or_study") &
        activity_destination %in% c("home", "work_or_study")
    ) |>
    group_by(o, d) |>
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

  od_non_work_morning <- od_trip_types_morning |>
    filter(
      activity_origin %in%
        c("frequent_activityuent", "infrequent_activity") |
        activity_destination %in% c("frequent_activity", "infrequent_activity")
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

  od_non_work_evening <- od_trip_types_evening |>
    filter(
      activity_origin %in%
        c("frequent_activityuent", "infrequent_activity") |
        activity_destination %in% c("frequent_activity", "infrequent_activity")
    ) |>
    group_by(o, d) |>
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

  create_flowmap <- function(
    od,
    nodes,
    edge_width_factor = 1
  ) {
    # add the flows to the basemap
    flowmap_plot <- base_plot_districts |>
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
        k_node = 20 # play around with this parameter to aggregate nodes and flows
      )

    # customise colours for the fill
    flowmap_plot <- flowmap_plot +
      scale_fill_gradient(
        low = "#FABB29",
        high = "#AB061F",
        labels = scales::comma_format() # Real value labels
      ) +
      theme(
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 16, face = "bold"),
      )

    return(flowmap_plot)
  }

  # `edge_width_factor` is adjusted after first plot attempt to normalise the flows legends, as flowmapper does not allow to change the width of the edges natively and facet the plots automatically

  p_home_work_study_morning <- create_flowmap(
    od = od_home_work_study_morning,
    nodes = nodes,
    edge_width_factor = 1 / 3.11
  ) +
    labs(title = "A: Home-Work/Study Trips", subtitle = "Morning (7-11)")

  p_non_work_morning <- create_flowmap(
    od = od_non_work_morning,
    nodes = nodes,
    edge_width_factor = 1
  ) +
    labs(title = "B: Non-Work Trips", subtitle = "Morning (7-11)")

  p_home_work_study_evening <- create_flowmap(
    od = od_home_work_study_evening,
    nodes = nodes,
    edge_width_factor = 1 / 4.62
  ) +
    labs(title = "C: Home-Work/Study Trips", subtitle = "Evening (17-21)")

  p_non_work_evening <- create_flowmap(
    od = od_non_work_evening,
    nodes = nodes,
    edge_width_factor = 1 / 1.19
  ) +
    labs(title = "D: Non-Work Trips", subtitle = "Evening (17-21)")

  p_compare <- (p_home_work_study_morning + p_non_work_morning) /
    (p_home_work_study_evening + p_non_work_evening)

  p_compare <- p_compare +
    plot_annotation(
      caption = "Mobility flows in Madrid area\nMean over a week of February 6-10, 2023",
      theme = theme(
        plot.caption = element_text(size = 12),
      )
    )

  if (!fs::dir_exists(plots_output_dir)) {
    fs::dir_create(plots_output_dir)
  }

  plot_save_path <- paste0(plots_output_dir, "/fig-madrid.png")

  ggsave(
    plot_save_path,
    plot = p_compare,
    width = 9,
    height = 12,
    units = "in",
    dpi = 300,
    create.dir = TRUE
  )

  return(plot_save_path)
}
