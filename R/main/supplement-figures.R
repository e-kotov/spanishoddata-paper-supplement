import_gathered_articles_data_from_json <- function(
  input_json_path
) {
  articles <- input_json_path |>
    RcppSimdJson::fload(max_simplify_lvl = "data_frame") |>
    tibble::as_tibble()

  return(articles)
}

convert_gathered_articles_to_csv <- function(
  articles,
  output_csv_path
) {
  articles_flat_table <- articles |>
    dplyr::mutate(across(where(is.list), ~ sapply(., paste, collapse = ";")))

  readr::write_csv(
    x = articles_flat_table,
    file = output_csv_path,
    quote = "needed"
  )

  if (fs::file_exists(output_csv_path)) {
    return(output_csv_path)
  } else {
    stop(paste0(
      "Error writing ",
      output_csv_path,
      " to disk. Check write permissions."
    ))
  }
}

# Generate previous uses plots
generate_previous_uses_plots <- function(
  articles,
  plots_output_dir
) {
  if (fs::dir_exists(plots_output_dir) == FALSE) {
    fs::dir_create(plots_output_dir)
  }

  # fig_topics -------------------------------------------------------------
  fig_i <- 1
  fig_topics <- articles |>
    select(paper_short_citation, paper_doi_link, research_focus) |>
    unnest_longer(research_focus) |>
    group_by(research_focus) |>
    summarize(article_count = n(), .groups = 'drop') |>
    mutate(
      share = article_count / length(unique(articles$paper_doi_link)) * 100
    ) |>
    ggplot(aes(y = reorder(research_focus, share), x = share)) +
    ggalt::geom_lollipop(horizontal = TRUE) +
    geom_text(aes(label = article_count), hjust = -1) +
    labs(
      title = "Article Topics",
      x = "Share (%)",
      y = "",
      caption = "May not sum to 100%\nShare (on x axis) and Number (on plot) of Articles by Article Topic"
    ) +
    scale_y_discrete(labels = function(x) str_wrap(x, width = 50)) +
    scale_x_continuous(limits = c(0, 60)) +
    ggpubr::theme_pubr(base_size = 14) +
    theme(
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 12),
      axis.ticks = element_blank(),
      axis.line = element_blank()
    )

  save_supplement_plot(
    fig = fig_topics,
    fig_i = fig_i,
    plots_output_dir = plots_output_dir,
    width = 7,
    height = 5
  )

  # fig_data_years ---------------------------------------------------------
  fig_i <- fig_i + 1
  fig_data_years <- articles |>
    select(paper_short_citation, paper_doi_link, data_years_used) |>
    unnest_longer(data_years_used) |>
    group_by(data_years_used) |>
    summarize(article_count = n(), .groups = 'drop') |>
    mutate(
      share = article_count / length(unique(articles$paper_doi_link)) * 100
    ) |>
    ggplot(aes(y = reorder(data_years_used, share), x = share)) +
    ggalt::geom_lollipop(horizontal = TRUE) +
    geom_text(aes(label = article_count), hjust = -1) +
    labs(
      title = "Year of MITMS data used",
      x = "Share (%)",
      y = "",
      caption = "May not sum to 100%\nShare (on x axis) and Number (on plot) of Articles by Years of MITMS data"
    ) +
    scale_y_discrete(labels = function(x) str_wrap(x, width = 50)) +
    scale_x_continuous(limits = c(0, 100)) +
    ggpubr::theme_pubr(base_size = 14) +
    theme(
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 12),
      axis.ticks = element_blank(),
      axis.line = element_blank()
    )

  save_supplement_plot(
    fig = fig_data_years,
    fig_i = fig_i,
    plots_output_dir = plots_output_dir,
    width = 6,
    height = 3.5
  )

  # fig_data_time_resolution -----------------------------------------------

  fig_i <- fig_i + 1
  fig_data_time_resolution <- articles |>
    select(paper_short_citation, paper_doi_link, data_time_resolution_used) |>
    unnest_longer(data_time_resolution_used) |>
    group_by(data_time_resolution_used) |>
    summarize(article_count = n(), .groups = 'drop') |>
    mutate(
      share = article_count / length(unique(articles$paper_doi_link)) * 100
    ) |>
    ggplot(aes(y = reorder(data_time_resolution_used, share), x = share)) +
    ggalt::geom_lollipop(horizontal = TRUE) +
    geom_text(aes(label = article_count), hjust = -1) +
    labs(
      title = "MITMS data time resolution",
      x = "Share (%)",
      y = "",
      caption = "May not sum to 100%\nShare (on x axis) and Number (on plot) of Articles by MITMS Time Resolution"
    ) +
    scale_y_discrete(labels = function(x) str_wrap(x, width = 50)) +
    scale_x_continuous(limits = c(0, 100)) +
    ggpubr::theme_pubr(base_size = 14) +
    theme(
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 12),
      axis.ticks = element_blank(),
      axis.line = element_blank()
    )

  save_supplement_plot(
    fig = fig_data_time_resolution,
    fig_i = fig_i,
    plots_output_dir = plots_output_dir,
    width = 6,
    height = 3.5
  )

  # fig_data_spatial_resolution --------------------------------------------
  fig_i <- fig_i + 1
  fig_data_spatial_resolution <- articles |>
    select(
      paper_short_citation,
      paper_doi_link,
      data_spatial_resolution_used
    ) |>
    unnest_longer(data_spatial_resolution_used) |>
    group_by(data_spatial_resolution_used) |>
    summarize(article_count = n(), .groups = 'drop') |>
    mutate(
      share = article_count / length(unique(articles$paper_doi_link)) * 100
    ) |>
    ggplot(aes(y = reorder(data_spatial_resolution_used, share), x = share)) +
    ggalt::geom_lollipop(horizontal = TRUE) +
    geom_text(aes(label = article_count), hjust = -1) +
    labs(
      title = "MITMS data spatial resolution",
      x = "Share (%)",
      y = "",
      caption = "May not sum to 100%\nShare (on x axis) and Number (on plot) of Articles by MITMS Spatial Resolution"
    ) +
    scale_y_discrete(labels = function(x) str_wrap(x, width = 50)) +
    scale_x_continuous(limits = c(0, 100)) +
    ggpubr::theme_pubr(base_size = 14) +
    theme(
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 12),
      axis.ticks = element_blank(),
      axis.line = element_blank()
    )

  save_supplement_plot(
    fig = fig_data_spatial_resolution,
    fig_i = fig_i,
    plots_output_dir = plots_output_dir,
    width = 6,
    height = 3.5
  )

  # fig_data_pre_processing ------------------------------------------------
  fig_i <- fig_i + 1
  fig_data_pre_processing <- articles |>
    select(
      paper_short_citation,
      paper_doi_link,
      code_for_mitma_preprocessing
    ) |>
    group_by(code_for_mitma_preprocessing) |>
    summarize(article_count = n(), .groups = 'drop') |>
    mutate(
      share = article_count / length(unique(articles$paper_doi_link)) * 100
    ) |>
    ggplot(aes(y = reorder(code_for_mitma_preprocessing, share), x = share)) +
    ggalt::geom_lollipop(horizontal = TRUE) +
    geom_text(aes(label = article_count), hjust = -1) +
    labs(
      title = "MITMS data pre-processing code",
      x = "Share (%)",
      y = "",
      caption = "Share (on x axis) and Number (on plot) of Articles by Data Pre-Processing Reproducibility"
    ) +
    scale_y_discrete(labels = function(x) str_wrap(x, width = 50)) +
    scale_x_continuous(limits = c(0, 100)) +
    ggpubr::theme_pubr(base_size = 14) +
    theme(
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 12),
      axis.ticks = element_blank(),
      axis.line = element_blank()
    )

  save_supplement_plot(
    fig = fig_data_pre_processing,
    fig_i = fig_i,
    plots_output_dir = plots_output_dir,
    width = 6,
    height = 3.5
  )

  # fig_shared_pre_processed_data ------------------------------------------
  fig_i <- fig_i + 1
  fig_shared_pre_processed_data <- articles |>
    select(
      paper_short_citation,
      paper_doi_link,
      preprocessed_mitma_data_shared
    ) |>
    group_by(preprocessed_mitma_data_shared) |>
    summarize(article_count = n(), .groups = 'drop') |>
    mutate(
      share = article_count / length(unique(articles$paper_doi_link)) * 100
    ) |>
    ggplot(aes(y = reorder(preprocessed_mitma_data_shared, share), x = share)) +
    ggalt::geom_lollipop(horizontal = TRUE) +
    geom_text(aes(label = article_count), hjust = -1) +
    labs(
      title = "MITMS data pre-processing code",
      x = "Share (%)",
      y = "",
      caption = "Share (on x axis) and Number (on plot) of Articles by Shared Pre-processed MITMS Data"
    ) +
    scale_y_discrete(labels = function(x) str_wrap(x, width = 50)) +
    scale_x_continuous(limits = c(0, 100)) +
    ggpubr::theme_pubr(base_size = 14) +
    theme(
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 12),
      axis.ticks = element_blank(),
      axis.line = element_blank()
    )

  save_supplement_plot(
    fig = fig_shared_pre_processed_data,
    fig_i = fig_i,
    plots_output_dir = plots_output_dir,
    width = 6,
    height = 3.5
  )

  # fig_analysis_reproducibility -------------------------------------------
  fig_i <- fig_i + 1
  fig_analysis_reproducibility <- articles |>
    select(paper_short_citation, paper_doi_link, code_for_analysis) |>
    group_by(code_for_analysis) |>
    summarize(article_count = n(), .groups = 'drop') |>
    mutate(
      share = article_count / length(unique(articles$paper_doi_link)) * 100
    ) |>
    ggplot(aes(y = reorder(code_for_analysis, share), x = share)) +
    ggalt::geom_lollipop(horizontal = TRUE) +
    geom_text(aes(label = article_count), hjust = -1) +
    labs(
      title = "Data analysis code",
      x = "Share (%)",
      y = "",
      caption = "Share (on x axis) and Number (on plot) of Articles by Data Analysis Reproducibility"
    ) +
    scale_y_discrete(labels = function(x) str_wrap(x, width = 50)) +
    scale_x_continuous(limits = c(0, 100)) +
    ggpubr::theme_pubr(base_size = 14) +
    theme(
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 12),
      axis.ticks = element_blank(),
      axis.line = element_blank()
    )

  save_supplement_plot(
    fig = fig_analysis_reproducibility,
    fig_i = fig_i,
    plots_output_dir = plots_output_dir,
    width = 6,
    height = 3.5
  )

  # fig_vis_reproducibility ------------------------------------------------
  fig_i <- fig_i + 1
  fig_vis_reproducibility <- articles |>
    select(paper_short_citation, paper_doi_link, code_for_datavis) |>
    group_by(code_for_datavis) |>
    summarize(article_count = n(), .groups = 'drop') |>
    mutate(
      share = article_count / length(unique(articles$paper_doi_link)) * 100
    ) |>
    ggplot(aes(y = reorder(code_for_datavis, share), x = share)) +
    ggalt::geom_lollipop(horizontal = TRUE) +
    geom_text(aes(label = article_count), hjust = -1) +
    labs(
      title = "Data visualisation code",
      x = "Share (%)",
      y = "",
      caption = "Share (on x axis) and Number (on plot) of Articles by Data Visualisation Reproducibility"
    ) +
    scale_y_discrete(labels = function(x) str_wrap(x, width = 50)) +
    scale_x_continuous(limits = c(0, 100)) +
    ggpubr::theme_pubr(base_size = 14) +
    theme(
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 12),
      axis.ticks = element_blank(),
      axis.line = element_blank()
    )

  save_supplement_plot(
    fig = fig_vis_reproducibility,
    fig_i = fig_i,
    plots_output_dir = plots_output_dir,
    width = 6,
    height = 3.5
  )

  # fig_citation_approaches ------------------------------------------------
  fig_i <- fig_i + 1
  fig_citation_approaches <- articles |>
    select(paper_short_citation, paper_doi_link, data_citation_approaches) |>
    unnest_longer(data_citation_approaches) |>
    group_by(data_citation_approaches) |>
    summarize(article_count = n(), .groups = 'drop') |>
    mutate(
      share_approaches = article_count /
        length(unique(articles$paper_doi_link)) *
        100
    ) |>
    ggplot(aes(
      y = reorder(data_citation_approaches, share_approaches),
      x = share_approaches
    )) +
    ggalt::geom_lollipop(horizontal = TRUE) +
    geom_text(aes(label = article_count), hjust = -1) +
    labs(
      title = "Citation approaches",
      x = "Share (%)",
      y = "",
      caption = "May not sum to 100%\nShare (on x axis) and Number (on plot) of Articles by Data Citation Approach"
    ) +
    scale_y_discrete(labels = function(x) str_wrap(x, width = 50)) +
    scale_x_continuous(limits = c(0, 60)) +
    ggpubr::theme_pubr(base_size = 14) +
    theme(
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 12),
      axis.ticks = element_blank(),
      axis.line = element_blank()
    )

  save_supplement_plot(
    fig = fig_citation_approaches,
    fig_i = fig_i,
    plots_output_dir = plots_output_dir,
    width = 7,
    height = 5
  )

  # fig_language_software --------------------------------------------------
  fig_i <- fig_i + 1
  fig_language_software <- articles |>
    select(paper_short_citation, paper_doi_link, code_language) |>
    unnest_longer(code_language) |>
    group_by(code_language) |>
    summarize(article_count = n(), .groups = 'drop') |>
    mutate(
      share = article_count / length(unique(articles$paper_doi_link)) * 100
    ) |>
    ggplot(aes(y = reorder(code_language, share), x = share)) +
    ggalt::geom_lollipop(horizontal = TRUE) +
    geom_text(aes(label = article_count), hjust = -1) +
    labs(
      title = "Languages/Software used",
      x = "Share (%)",
      y = "",
      caption = "May not sum to 100%\nShare (on x axis) and Number (on plot) of Articles by Data Citation Approach"
    ) +
    scale_y_discrete(labels = function(x) str_wrap(x, width = 50)) +
    scale_x_continuous(limits = c(0, 60)) +
    ggpubr::theme_pubr(base_size = 14) +
    theme(
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 12),
      axis.ticks = element_blank(),
      axis.line = element_blank()
    )

  save_supplement_plot(
    fig = fig_language_software,
    fig_i = fig_i,
    plots_output_dir = plots_output_dir,
    width = 7,
    height = 5
  )

  file_list <- fs::dir_ls(plots_output_dir)
  return(file_list)
}


generate_speed_test_plot <- function(
  test_results_table,
  plots_output_dir
) {
  if (fs::dir_exists(plots_output_dir) == FALSE) {
    fs::dir_create(plots_output_dir)
  }

  # fig_analysis_speed -----------------------------------------------------

  # Create a complete grid of memory_gb, n_threads, and data_type
  complete_grid <- expand.grid(
    memory_gb = unique(test_results_table$memory_gb),
    n_threads = unique(test_results_table$n_threads),
    data_type = unique(test_results_table$data_type)
  )

  # Merge the grid with the actual data, introducing NAs for missing combinations
  data_complete <- complete_grid |>
    left_join(
      test_results_table,
      by = c("memory_gb", "n_threads", "data_type")
    ) |>
    mutate(tt_elapsed_time = tt_elapsed_time / 60) # Convert to minutes directly here

  # Plot the heatmap with NA values handled
  fig_analysis_speed <- data_complete |>
    mutate(
      memory_gb = as.factor(memory_gb),
      n_threads = as.factor(n_threads)
    ) |>
    ggplot(aes(x = n_threads, y = memory_gb, fill = tt_elapsed_time)) +
    geom_tile() +
    facet_wrap(~data_type, scales = "free_x") +
    scale_fill_viridis_c(
      name = "Elapsed Time (minutes)",
      direction = -1,
      breaks = scales::pretty_breaks(n = 5),
      na.value = "grey60" # Color for missing values
    ) +
    labs(
      title = "Processing Speed Comparison",
      subtitle = test_results_table[1, "test_task"],
      x = "Number of Threads",
      y = "Memory (GB)",
      caption = "Data processing speed comparison: DuckDB engine running on CSV.gz files \nvs DuckDB database vs a folder of Parquet files"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "right", # Keep the legend on the right
      legend.key.size = unit(2, "lines"), # Increase the size of the legend key
      legend.key.height = unit(2, "cm"), # Increase the height of the legend
      axis.text.x = element_text(angle = 0, hjust = 1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "lightgray"),
      strip.text = element_text(face = "bold"),
      plot.caption = element_text(hjust = 0)
    ) +
    guides(fill = guide_colorbar(barwidth = 1, barheight = 15))

  plot_save_path <- paste0(plots_output_dir, "/12_analysis-speed.pdf")
  ggplot2::ggsave(
    filename = plot_save_path,
    plot = fig_analysis_speed,
    width = 8,
    height = 6,
    dpi = 300
  )

  return(plot_save_path)
}

generate_package_workflow_plot <- function(
  flowchart_gv_dot_file,
  output_pdf_file
) {
  flowchart <- DiagrammeR::grViz(flowchart_gv_dot_file)

  plot_path <- fs::path_dir(output_pdf_file)
  if (fs::dir_exists(plot_path) == FALSE) {
    fs::dir_create(plot_path)
  }

  flowchart |>
    DiagrammeRsvg::export_svg() |>
    charToRaw() |>
    rsvg::rsvg_pdf(output_pdf_file)

  if (fs::file_exists(output_pdf_file)) {
    return(output_pdf_file)
  } else {
    stop(paste0(
      "Error writing ",
      output_pdf_file,
      " to disk. Check write permissions."
    ))
  }
}

save_supplement_plot <- function(fig, fig_i, plots_output_dir, width, height) {
  ggplot2::ggsave(
    filename = paste0(
      plots_output_dir,
      "/",
      fig_i,
      "_",
      deparse(substitute(fig)),
      ".pdf"
    ),
    plot = fig,
    width = 7,
    height = 5,
    dpi = 300
  )
}
