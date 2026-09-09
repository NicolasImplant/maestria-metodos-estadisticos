#!/usr/bin/env Rscript
# Prepare a fresh clone for work: ensure renv is available and restore the
# project library from renv.lock. Safe to run repeatedly.
#
#   Rscript scripts/bootstrap.R

options(warn = 1L, repos = c(CRAN = "https://cloud.r-project.org"))

if (!requireNamespace("renv", quietly = TRUE)) {
  message("Installing renv ...")
  install.packages("renv")
}

if (file.exists("renv.lock")) {
  message("Restoring the project library from renv.lock ...")
  renv::restore(prompt = FALSE)
} else {
  message("No renv.lock yet. Initializing renv for this project ...")
  renv::init(bare = FALSE)
}

message("Bootstrap complete.")
