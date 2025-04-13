# Case Study 1: Assessing Cycling Potential and Infrastructure Provision in Valencia

case_study_valencia_biking_potential_plot <- function(
  plot_objects,
  plots_output_dir = "plots/main-plots"
) {
  # Unpack prepared objects
  districts_valencia <- plot_objects$districts_valencia
  valencia_intrazonal_sf <- plot_objects$valencia_intrazonal_sf
  cycle_net <- plot_objects$cycle_net
  bbox_valencia_zoom <- plot_objects$bbox_valencia_zoom
  od_sf <- plot_objects$od_sf

  # Plot map 1

  # Define bounding box with appropriate CRS
  # Create the map object
  map1 <- tm_shape(valencia_intrazonal_sf, bbox_valencia_zoom) +
    tm_polygons(
      "trips",
      border.col = "grey70",
      palette = "YlGnBu",
      alpha = 0.7,
      title = "Potential Cycling Trips\n(Intra-Zone)",
      style = "fixed",
      breaks = c(0, 100000, 200000, 300000, 400000, 500000, Inf),
      textNA = "",
      na.value = "transparent"
    ) +
    tm_shape(cycle_net) +
    tm_lines(
      col = "grey20",
      lpha = 0.7,
      lwd = 1
    ) + # Sets the legend title
    tm_add_legend(
      type = "lines",
      labels = "Cycle network",
      col = "grey20",
      lpha = 0.7,
      lwd = 1
    ) +
    tm_layout(
      legend.outside = FALSE,
      frame = FALSE,
      legend.title.size = 1.3,
      legend.text.size = 1.3
    ) +
    tm_legend(frame = FALSE)

  # tmap_mode("plot")
  # map1

  # Plot map 2

  # Define connectivity categories
  breaks <- quantile(
    od_sf$ratio,
    probs = seq(0, 1, length.out = 5),
    na.rm = TRUE
  )
  labels <- c("Low", "Medium Low", "Medium High", "High")

  # Assign categories and adjust factor levels
  od_sf$connectivity_index <- factor(
    cut(od_sf$ratio, breaks = breaks, labels = labels, include.lowest = TRUE),
    levels = labels
  )

  # Define reversed color scale (blue = high, red = low)
  colors <- rev(RColorBrewer::brewer.pal(4, "RdYlBu")[4:1])

  # Generate map
  map2 <- tm_shape(districts_valencia, bbox_valencia_zoom) +
    tm_borders("grey70") +
    tm_shape(od_sf) +
    tm_lines(
      lwd = "trips",
      col = "connectivity_index",
      col_alpha = 0.7,
      col.scale = tm_scale(values = colors),
      col.legend = tm_legend(title = "Cycling Network\nConnectivity"),
      lwd.scale = tm_scale_continuous(
        values.scale = 10
      ),
      lwd.legend = tm_legend(
        title = "Potential Cycling Trips\n(Inter-Zone)",
        col = "gray70"
      )
    ) +
    # tm_shape(cycle_net) +
    # tm_lines(col = "red",
    #         lwd = 1) +  # Sets the legend title
    tm_layout(
      legend.outside = FALSE,
      frame = FALSE,
      legend.title.size = 1.3,
      legend.text.size = 1.3
    ) +
    tm_legend(frame = FALSE)

  # tmap_mode("plot")
  # map2

  # Create a facet with both images
  # Add bold titles to each map
  map1 <- map1 +
    tm_layout(
      main.title = "A",
      main.title.fontface = "bold",
      main.title.size = 2
    )
  map2 <- map2 +
    tm_layout(
      main.title = "B",
      main.title.fontface = "bold",
      main.title.size = 2
    )

  # Create the figure with both maps
  figure <- tmap_arrange(map1, map2, ncol = 2)

  if (!fs::dir_exists(plots_output_dir)) {
    fs::dir_create(plots_output_dir, recurse = TRUE)
  }

  plot_save_path <- paste0(plots_output_dir, "/fig-valencia.png")

  # Save map as an image
  tmap::tmap_save(
    tm = figure,
    filename = plot_save_path,
    dpi = 300,
    width = 10,
    height = 8,
    units = "in"
  )

  return(plot_save_path)
}
