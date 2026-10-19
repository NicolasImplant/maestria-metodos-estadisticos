# styler: off
box::use(
  testthat[
    test_that, expect_equal, expect_error, expect_named, expect_true,
  ],
  app/pipelines/confidence_intervals[
    ci_mean_t, ci_mean_bootstrap, ci_diff_means, ci_proportion,
    ci_diff_proportions, ci_var_ratio, ci_summary_table,
    ci_diff_proportions_paired, ci_var_ratio_paired_boot, boot_bound_mc_se,
  ],
)
# styler: on

test_that("ci_mean_t matches stats::t.test exactly", {
  x <- c(10, 12, 14, 15, 18, 20, NA)
  res <- ci_mean_t(x, conf_level = 0.95)
  ref <- stats::t.test(x, conf.level = 0.95)

  expect_equal(res$n, 6L)
  expect_equal(res$mean, mean(x, na.rm = TRUE))
  expect_equal(res$ci_low, ref$conf.int[[1]])
  expect_equal(res$ci_high, ref$conf.int[[2]])
  expect_equal(res$width, ref$conf.int[[2]] - ref$conf.int[[1]])
})

test_that("ci_mean_bootstrap is deterministic under fixed seed", {
  x <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)
  b1 <- ci_mean_bootstrap(
    x,
    n_boot = 500L,
    seed = 2026L,
    keep_replicates = TRUE
  )
  b2 <- ci_mean_bootstrap(x, n_boot = 500L, seed = 2026L)

  expect_equal(b1$ci_low, b2$ci_low)
  expect_equal(b1$ci_high, b2$ci_high)
  expect_equal(length(b1$replicates), 500L)
  expect_true(b1$ci_low < b1$mean && b1$mean < b1$ci_high)
})

test_that("ci_diff_means computes Welch and two-sample Bootstrap bounds", {
  g1 <- c(10, 11, 12, 13, 14, 15)
  g2 <- c(14, 15, 16, 17, 18, 19)
  res <- ci_diff_means(g1, g2, conf_level = 0.95, n_boot = 500L, seed = 2026L)
  ref <- stats::t.test(g1, g2, var.equal = FALSE)

  expect_equal(res$diff, -4)
  expect_equal(res$welch_low, ref$conf.int[[1]])
  expect_equal(res$welch_high, ref$conf.int[[2]])
  expect_true(res$boot_low < 0 && res$boot_high < 0)
})

test_that("ci_proportion and ci_diff_proportions match expected formulas", {
  p_res <- ci_proportion(30L, 50L, conf_level = 0.95)
  expect_equal(p_res$prop, 0.6)
  expect_true(p_res$wald_low < 0.6 && p_res$wald_high > 0.6)
  expect_true(p_res$wilson_low >= 0 && p_res$wilson_high <= 1)

  d_res <- ci_diff_proportions(30L, 50L, 40L, 60L, conf_level = 0.95)
  expect_equal(round(d_res$ci_low, 2), -0.25)
  expect_equal(round(d_res$ci_high, 2), 0.11)
})

test_that("ci_var_ratio matches F-distribution variance ratio interval", {
  x1 <- c(2, 4, 6, 8, 10, 12)
  x2 <- c(3, 4, 5, 6, 7, 8)
  res <- ci_var_ratio(x1, x2, conf_level = 0.95)
  ref <- stats::var.test(x1, x2, conf.level = 0.95)

  expect_equal(res$ratio, unname(ref$estimate))
  expect_equal(res$ci_low, ref$conf.int[[1]])
  expect_equal(res$ci_high, ref$conf.int[[2]])
})

test_that("ci_summary_table returns expected columns and rejects non-numeric", {
  df <- data.frame(a = 1:20, b = (1:20)^2, label = rep("x", 20))
  tbl <- ci_summary_table(df, c("a", "b"), n_boot = 200L, seed = 2026L)

  expect_equal(nrow(tbl), 2L)
  expect_named(
    tbl,
    c(
      "variable", "n", "mean", "median", "sd", "se", "skewness",
      "t_low", "t_high", "t_width", "boot_se", "boot_low",
      "boot_high", "boot_width"
    )
  )
  expect_error(ci_summary_table(df, "label"), "not numeric")
})

test_that("ci_diff_proportions_paired uses discordant-cell variance", {
  y1 <- c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE)
  y2 <- c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE)
  res <- ci_diff_proportions_paired(y1, y2, conf_level = 0.95)

  d <- as.numeric(y1) - as.numeric(y2)
  se_ref <- sqrt(mean((d - mean(d))^2) / length(d))
  expect_equal(res$diff, mean(y1) - mean(y2))
  expect_equal(res$se, se_ref)
  expect_equal(res$ci_low, res$diff - stats::qnorm(0.975) * se_ref)
  expect_equal(c(res$n10, res$n01), c(2L, 3L))
  expect_error(ci_diff_proportions_paired(y1, y2[-1]))
})

test_that("ci_var_ratio_paired_boot is reproducible and brackets the ratio", {
  set.seed(1)
  x1 <- stats::rnorm(60, sd = 2)
  x2 <- 0.5 * x1 + stats::rnorm(60)
  r1 <- ci_var_ratio_paired_boot(x1, x2, n_boot = 500L, seed = 2026L)
  r2 <- ci_var_ratio_paired_boot(x1, x2, n_boot = 500L, seed = 2026L)

  expect_equal(r1, r2)
  expect_equal(r1$ratio, stats::var(x1) / stats::var(x2))
  expect_true(r1$ci_low < r1$ratio && r1$ratio < r1$ci_high)
  expect_error(ci_var_ratio_paired_boot(x1, x2[-1]))
})

test_that("boot_bound_mc_se shrinks with more replicates", {
  se_small <- boot_bound_mc_se(0.39, 1000L)
  se_large <- boot_bound_mc_se(0.39, 4000L)

  expect_equal(se_small / se_large, 2)
  expect_equal(
    boot_bound_mc_se(1, 5000L),
    sqrt(0.025 * 0.975 / 5000) / stats::dnorm(stats::qnorm(0.025))
  )
})
