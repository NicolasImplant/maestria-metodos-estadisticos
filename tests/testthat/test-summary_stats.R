# styler: off
box::use(
  testthat[test_that, expect_equal, expect_error, expect_named],
  app/pipelines/summary_stats[numeric_summary],
)
# styler: on

test_that("numeric_summary returns one row per requested column", {
  out <- numeric_summary(mtcars, c("mpg", "hp"))

  expect_equal(nrow(out), 2L)
  expect_equal(out$variable, c("mpg", "hp"))
  expect_named(
    out,
    c("variable", "n", "n_missing", "mean", "sd", "min", "median", "max")
  )
})

test_that("numeric_summary computes the expected statistics", {
  df <- data.frame(x = c(1, 2, 3, NA))
  out <- numeric_summary(df, "x")

  expect_equal(out$n, 4L)
  expect_equal(out$n_missing, 1L)
  expect_equal(out$mean, 2)
  expect_equal(out$min, 1)
  expect_equal(out$max, 3)
})

test_that("numeric_summary rejects non-numeric and unknown columns", {
  df <- data.frame(label = c("a", "b"))

  expect_error(numeric_summary(df, "label"), "not numeric")
  expect_error(numeric_summary(df, "missing"), regexp = "subset|missing")
})
