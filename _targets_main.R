# targets: quick access --------------------------------------------
# Sys.setenv(TAR_PROJECT = "main")
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
tar_source("R/main/")

# load speed test results from the "speed_test" pipeline
speed_test_summary <- tar_read(
  speed_test_summary,
  store = "_targets/speed_test"
)

# load data prep results from the "data_prep" pipeline
valencia_data_for_plots <- tar_read(
  valencia_data_for_plots,
  store = "_targets/data_prep"
)
madrid_data_for_plots <- tar_read(
  madrid_data_for_plots,
  store = "_targets/data_prep"
)

# parameters -------------------------------------------------------------
# you might want to change these according to the specs of the system you are using
options(global.max_mem_gb = 24)
options(global.max_n_cpu = parallelly::availableCores() - 1)
options(global.spanishoddata_data_path = "data/input/mitms-data/")
spanishoddata::spod_set_data_dir(getOption("global.spanishoddata_data_path"))
options(global.osm_path = "data/proc/osm")
# token to access the read only version of the OSF repository
options(global.osf_view_only_token = "2af02ae84922467c8b9a22f57d249295")


list(
  # Prepare gathered articles data -----------------------------------------
  tar_target(
    name = articles,
    packages = c("RcppSimdJson", "dplyr", "tibble"),
    command = import_gathered_articles_data_from_json(
      input_json_path = "supplement-data/articles-using-mitms-mobility-data.json"
    )
  ),

  tar_target(
    name = articles_csv,
    packages = c("RcppSimdJson", "tidyverse", "fs"),
    command = convert_gathered_articles_to_csv(
      articles = articles,
      output_csv_path = "supplement-data/articles-using-mitms-mobility-data.csv"
    ),
    format = "file"
  ),

  # Generate previous uses plots -------------------------------------------
  tar_target(
    name = previous_uses_plots,
    packages = c("tidyverse", "ggpubr", "ggalt"),
    command = generate_previous_uses_plots(
      articles = articles,
      plots_output_dir = "plots/supplement-plots"
    ),
    format = "file"
  ),

  # Create package workflow plot -------------------------------------------
  tar_target(
    name = fig_package_workflow,
    packages = c("DiagrammeR", "DiagrammeRsvg", "rsvg"),
    command = generate_package_workflow_plot(
      flowchart_gv_dot_file = "supplement-data/fig_package_workflow.gv",
      output_pdf_file = "plots/supplement-plots/11_fig_package_workflow.pdf"
    ),
    format = "file"
  ),

  # case study - Valencia
  tar_target(
    name = valencia_plot,
    packages = c("tidyverse", "sf", "tmap"),
    command = case_study_valencia_biking_potential_plot(
      plot_objects = valencia_data_for_plots,
      plots_output_dir = "plots/main-plots"
    ),
    format = "file"
  ),

  # case study - Madrid
  tar_target(
    name = madrid_plot,
    packages = c("tidyverse", "sf", "flowmapper", "patchwork"),
    command = case_study_madrid_work_v_nonwork_trips_plot(
      madrid_data_for_plots = madrid_data_for_plots,
      plots_output_dir = "plots/main-plots"
    ),
    format = "file"
  ),

  # Create analysis speed plot ---------------------------------------------
  tar_target(
    name = analysis_speed_plot,
    packages = c("tidyverse"),
    command = generate_speed_test_plot(
      test_results_table = speed_test_summary,
      plots_output_dir = "plots/supplement-plots"
    )
  )
)
