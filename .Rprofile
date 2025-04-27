# .Rprofile-binder — Startup profile for binder

if (interactive()) {
  # Detect container vs. local environment
  is_container <- function() {
    # Docker checks
    docker_env <- file.exists("/.dockerenv")
    docker_cgroup <- FALSE
    if (file.exists("/proc/1/cgroup")) {
      cg <- try(readLines("/proc/1/cgroup"), silent = TRUE)
      if (!inherits(cg, "try-error")) {
        docker_cgroup <- any(grepl("docker", cg))
      }
    }

    # Apptainer/Singularity checks
    singularity_dir <- dir.exists("/.singularity.d")
    container_env <- any(grepl(
      "^APPTAINER_|^SINGULARITY_",
      names(Sys.getenv())
    ))

    # Binder (mybinder.org) checks
    binder_env <- nzchar(Sys.getenv("BINDER_REPO_URL"))
    browser_env <- Sys.getenv("BROWSER") ==
      "/usr/lib/rstudio-server/bin/postback/rpostback-browser"

    # Determine container status and type
    is_cont <- docker_env ||
      docker_cgroup ||
      singularity_dir ||
      container_env ||
      binder_env ||
      browser_env
    ctype <- NA_character_
    if (docker_env || docker_cgroup) ctype <- "docker"
    if (singularity_dir || container_env) ctype <- "apptainer/singularity"
    if (binder_env || browser_env) ctype <- "binder"

    list(
      is_container = is_cont,
      container_type = ctype
    )
  }

  # Gather environment info and package availability
  container_info <- is_container()
  have_pkgs <- requireNamespace("targets", quietly = TRUE) &&
    requireNamespace("spanishoddata", quietly = TRUE)

  if (container_info$is_container && have_pkgs) {
    # Container environment with required packages: show startup messages
    if (requireNamespace("cli", quietly = TRUE)) {
      cli::cli_text(
        "\n\nWelcome to the computational environment for the \"spanishoddata: A package for accessing and working with Spanish Open Mobility Big Data\" article. Running in: {container_info$container_type}.\n"
      )
      cli::cli_text(
        "Run {.run targets::tar_visnetwork()} in R console to visualise the pipeline of actions that generate the figures."
      )
      cli::cli_text(
        "Run {.run targets::tar_destroy()} in R console to delete the pipeline snapshots."
      )
      cli::cli_text(
        "Run {.run targets::tar_make()} in R console to regenerate all figures."
      )
      cli::cli_text("You will find the updated figures in the `plots/` folder.")
      cli::cli_text(
        "For more information, see {.file README.md}/{.file README.qmd}."
      )
    } else {
      message(
        "\n\nWelcome to the computational environment for the 'spanishoddata: A package for accessing and working with Spanish Open Mobility Big Data' article. Running in: ",
        container_info$container_type,
        ".\n"
      )
      message(
        "Run `targets::tar_visnetwork()` in R console to visualise the pipeline of actions that generate the figures."
      )
      message(
        "Run `targets::tar_destroy()` in R console to delete the pipeline snapshots."
      )
      message(
        "Run `targets::tar_make()` in R console to regenerate all figures."
      )
      message("You will find the updated figures in the `plots/` folder.")
      message("For more information, see the README.md file in this directory.")
    }
    # Open key files in RStudio, if available
    if (
      requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::isAvailable()
    ) {
      invisible(try(
        rstudioapi::navigateToFile("_targets_main.R"),
        silent = TRUE
      ))
      invisible(try(
        rstudioapi::navigateToFile("README.md", line = 373),
        silent = TRUE
      ))
    }
  } else {
    # Not in container or missing packages: activate renv if available
    renv_activate <- file.path(getwd(), "renv", "activate.R")
    if (file.exists(renv_activate)) {
      source(renv_activate)
    }
    rm(renv_activate)
  }

  # Cleanup variables left in environment
  rm(is_container, container_info, have_pkgs)
}
