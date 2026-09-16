# styler: off
box::use(
  testthat[test_that, expect_equal, expect_error, expect_named, expect_true],
  app/pipelines/summary_stats[
    numeric_summary, categorical_summary, get_mode, iqr_outliers, skewness,
  ],
)
# styler: on

test_that("get_mode calculates statistical mode deterministically", {
  # Simple case
  expect_equal(get_mode(c(1, 2, 2, 3)), 2)

  # Tie-breaking for numeric (selects smallest)
  expect_equal(get_mode(c(1, 1, 2, 2, 3)), 1)

  # Tie-breaking for alphabetical characters
  expect_equal(get_mode(c("b", "b", "a", "a", "c")), "a")

  # Handles NA cleanly
  expect_equal(get_mode(c(5, 5, 2, NA, NA)), 5)
})

test_that("numeric_summary returns expected expanded columns", {
  df <- data.frame(x = c(1, 2, 2, 3, NA))
  out <- numeric_summary(df, "x")

  expect_equal(nrow(out), 1L)
  expect_equal(out$variable, "x")
  expect_equal(out$n, 5L)
  expect_equal(out$n_missing, 1L)
  expect_equal(out$mean, 2)
  expect_equal(out$median, 2)
  expect_equal(out$mode, 2)
  expect_equal(out$min, 1)
  expect_equal(out$max, 3)
  expect_equal(out$sd, 0.816496580927726)
  expect_equal(out$var, 0.666666666666667)
  expect_equal(out$range, 2)

  expect_named(
    out,
    c(
      "variable", "n", "n_missing", "mean", "median", "mode", "min", "max",
      "sd", "var", "range"
    )
  )
})

test_that("numeric_summary rejects non-numeric and unknown columns", {
  df <- data.frame(label = c("a", "b"))

  expect_error(numeric_summary(df, "label"), "not numeric")
  expect_error(numeric_summary(df, "missing"), regexp = "subset|missing")
})

test_that("categorical_summary returns correct counts and proportions", {
  df <- data.frame(
    status = c("U", "U", "R", "U", NA),
    stringsAsFactors = FALSE
  )

  out <- categorical_summary(df, "status")

  expect_equal(nrow(out), 3L) # U, R, and Missing (NA)
  expect_named(out, c("variable", "value", "count", "proportion"))

  # Check frequencies
  u_row <- out[out$value == "U", ]
  r_row <- out[out$value == "R", ]
  miss_row <- out[out$value == "Missing", ]

  expect_equal(u_row$count, 3L)
  expect_equal(r_row$count, 1L)
  expect_equal(miss_row$count, 1L)

  expect_equal(sum(out$proportion), 1.0)
})

test_that("categorical_summary rejects non-categorical columns", {
  df <- data.frame(x = c(1, 2, 3))
  expect_error(categorical_summary(df, "x"), "not character or factor")
})

test_that("iqr_outliers flags a single extreme value via the Tukey fence", {
  df <- data.frame(x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 100))
  out <- iqr_outliers(df, "x")

  expect_equal(out$variable, "x")
  expect_equal(out$q1, 3.25)
  expect_equal(out$q3, 7.75)
  expect_equal(out$iqr, 4.5)
  expect_equal(out$lower_bound, -3.5)
  expect_equal(out$upper_bound, 14.5)
  expect_equal(out$n_outliers, 1L)
  expect_equal(out$outlier_values, 100)
})

test_that("iqr_outliers reports zero outliers for a uniform sample", {
  df <- data.frame(x = 1:10)
  out <- iqr_outliers(df, "x")

  expect_equal(out$n_outliers, 0L)
  expect_equal(out$outlier_values, numeric(0))
})

test_that("iqr_outliers rejects non-numeric and unknown columns", {
  df <- data.frame(label = c("a", "b"))

  expect_error(iqr_outliers(df, "label"), "not numeric")
  expect_error(iqr_outliers(df, "missing"), regexp = "subset|missing")
})

test_that("skewness returns 0 for a perfectly symmetric sample", {
  expect_equal(skewness(c(1, 2, 3, 4, 5)), 0)
})

test_that("skewness is positive for a right-skewed sample", {
  expect_true(skewness(c(1, 2, 2, 2, 2, 2, 2, 2, 2, 100)) > 0)
})

test_that("skewness returns NA for degenerate samples", {
  expect_equal(skewness(c(1, 2)), NA_real_) # fewer than 3 observations
  expect_equal(skewness(c(5, 5, 5)), NA_real_) # zero variance
})
