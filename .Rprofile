# .Rprofile-binder — Startup profile for binder

# Simple container detection: Docker via USER == "rstudio" or Singularity via /.singularity.d or APPTAINER_/SINGULARITY_ env vars
is_container <- function() {
  docker_env <- identical(Sys.getenv("USER"), "rstudio")
  singularity_env <- dir.exists("/.singularity.d") ||
    any(grepl("^APPTAINER_|^SINGULARITY_", names(Sys.getenv())))
  docker_env || singularity_env
}

# Check for required packages
have_pkgs <- requireNamespace("targets", quietly = TRUE) &&
  requireNamespace("spanishoddata", quietly = TRUE)

if (is_container() && have_pkgs) {
  # Container detected: show startup messages
  if (interactive() && requireNamespace("cli", quietly = TRUE)) {
    cli::cli_text(
      "\n\nWelcome to the computational environment for the 'spanishoddata: A package for accessing and working with Spanish Open Mobility Big Data' article."
    )
    cli::cli_text(
      "Detected container environment (USER='rstudio' or Singularity)."
    )
    cli::cli_text(
      "Run {.run targets::tar_visnetwork()} to visualise the pipeline of actions that generate the figures."
    )
    cli::cli_text(
      "Run {.run targets::tar_destroy()} to delete the pipeline snapshots."
    )
    cli::cli_text("Run {.run targets::tar_make()} to regenerate all figures.")
    cli::cli_text("Updated figures are in the `plots/` folder.")
    cli::cli_text("See {.file README.md} for more details.")
  } else if (interactive()) {
    message(
      "\n\nWelcome to the computational environment for the 'spanishoddata' article (container detected)."
    )
    message("Detected container environment (USER='rstudio' or Singularity).")
    message("Run `targets::tar_visnetwork()` to visualise the pipeline.")
    message("Run `targets::tar_destroy()` to delete pipeline snapshots.")
    message("Run `targets::tar_make()` to regenerate figures.")
    message("Figures are in the 'plots/' folder.")
    message("See README.md for details.")
  }

  # Use RStudio sessionInit hook to open key files in IDE's source pane
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    setHook(
      "rstudio.sessionInit",
      function(newSession) {
        if (newSession && rstudioapi::isAvailable()) {
          try(
            rstudioapi::navigateToFile("_targets_main.R", line = 1),
            silent = TRUE
          )
          try(
            rstudioapi::navigateToFile("README.md", line = 360),
            silent = TRUE
          )
        }
      },
      action = "append"
    )
  }
} else {
  # Not in container or missing packages: activate renv if available
  renv_activate <- file.path(getwd(), "renv", "activate.R")
  if (file.exists(renv_activate)) {
    try(source(renv_activate), silent = TRUE)
  }
  rm(renv_activate)
}

# Cleanup helper objects
rm(is_container, have_pkgs)
