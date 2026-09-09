#!/usr/bin/env Rscript
# Format every R / Quarto file in place with the tidyverse style.
#
#   Rscript scripts/style.R

targets <- Filter(dir.exists, c("app", "tests", "scripts", "actividades"))

invisible(lapply(targets, function(dir) {
  styler::style_dir(
    path = dir,
    recursive = TRUE,
    filetype = c("R", "Rmd", "qmd"),
    exclude_dirs = c("renv", "packrat", ".git")
  )
}))
