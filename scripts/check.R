#!/usr/bin/env Rscript
# One-shot quality gate: style check (no writes) + lint + unit tests.
# Exits non-zero on the first failing stage so it can be wired into CI or a
# pre-push hook.
#
#   Rscript scripts/check.R

options(warn = 1L)

abort_stage <- function(msg) {
  message("\n[FAIL] ", msg)
  quit(status = 1L, save = "no")
}

targets <- Filter(dir.exists, c("app", "tests", "scripts", "actividades"))

# --- 1. Style: verify files are already formatted ------------------------
message("== styler (check only) ==")
styled <- do.call(rbind, lapply(targets, function(dir) {
  styler::style_dir(
    path = dir,
    recursive = TRUE,
    filetype = c("R", "Rmd", "qmd"),
    exclude_dirs = c("renv", "packrat", ".git"),
    dry = "on"
  )
}))
if (any(styled$changed, na.rm = TRUE)) {
  message("Files not formatted:")
  print(styled[styled$changed, "file"])
  abort_stage("Run `Rscript scripts/style.R` and commit the result.")
}

# --- 2. Lint -----------------------------------------------------------
message("== lintr ==")
lints <- lintr::lint_dir(path = ".")
if (length(lints) > 0L) {
  print(lints)
  abort_stage(sprintf("%d lint issue(s).", length(lints)))
}

# --- 3. Tests ---------------------------------------------------------------
message("== testthat ==")
results <- testthat::test_dir(
  "tests/testthat",
  reporter = testthat::default_reporter(),
  stop_on_failure = FALSE
)
df <- as.data.frame(results)
if (sum(df$failed) > 0L || any(df$error)) {
  abort_stage("Unit tests failed.")
}

message("\n[OK] All checks passed.")
