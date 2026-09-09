# Make `box::use(app/...)` resolve against the project root when the test
# suite is run from anywhere (RStudio/Positron, `scripts/check.R`, CI).
local({
  project_root <- normalizePath(
    file.path(testthat::test_path(), "..", ".."),
    mustWork = TRUE
  )
  current <- getOption("box.path")
  options(box.path = union(current, project_root))
})
