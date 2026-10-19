#' @title Parametric and Non-Parametric Confidence Intervals Pipeline
#'
#' @description
#' Pure functional pipeline for computing classical parametric confidence
#' intervals (Student's t, Welch two-sample t, Wald/Wilson binomial proportion,
#' F variance ratio) alongside non-parametric Percentile Bootstrap confidence
#' intervals.
#'
#' @details
#' \preformatted{
#' box::use(app/pipelines/confidence_intervals[
#'   ci_mean_t, ci_mean_bootstrap, ci_diff_means, ci_proportion, ci_var_ratio,
#'   ci_diff_proportions_paired, ci_var_ratio_paired_boot, boot_bound_mc_se,
#'   ci_summary_table
#' ])
#' }

# styler: off
box::use(
  checkmate[
    assert_count, assert_data_frame, assert_int, assert_logical,
    assert_number, assert_numeric, assert_subset,
  ],
  stats[dnorm, median, qf, qnorm, qt, quantile, sd, t.test, var],
  app/pipelines/summary_stats[skewness],
)
# styler: on

clean_numeric <- function(x, min_len = 2L) {
  assert_numeric(x, min.len = min_len)
  vals <- x[!is.na(x)]
  if (length(vals) < min_len) {
    stop(
      sprintf("At least %d non-missing observations are required.", min_len),
      call. = FALSE
    )
  }
  vals
}

#' Parametric t-Student confidence interval for the population mean
#'
#' @param x Numeric vector with at least 2 non-missing values.
#' @param conf_level Confidence level in `(0, 1)`. Default is `0.95`.
#' @return A named list with `n`, `mean`, `median`, `sd`, `se`, `df`,
#'   `t_crit`, `margin_error`, `ci_low`, `ci_high`, and `width`.
#' @export
ci_mean_t <- function(x, conf_level = 0.95) {
  vals <- clean_numeric(x, min_len = 2L)
  assert_number(conf_level, lower = 0.5, upper = 0.9999)

  n <- length(vals)
  m <- mean(vals)
  s <- sd(vals)
  se <- s / sqrt(n)
  df <- n - 1L
  alpha <- 1 - conf_level
  t_crit <- qt(1 - alpha / 2, df = df)
  me <- t_crit * se

  list(
    n = n,
    mean = m,
    median = median(vals),
    sd = s,
    se = se,
    df = df,
    t_crit = t_crit,
    margin_error = me,
    ci_low = m - me,
    ci_high = m + me,
    width = 2 * me
  )
}

#' Non-parametric Percentile Bootstrap confidence interval for the mean
#'
#' @param x Numeric vector with at least 2 non-missing values.
#' @param conf_level Confidence level in `(0, 1)`. Default is `0.95`.
#' @param n_boot Number of bootstrap replicates. Default is `5000L`.
#' @param seed Integer random seed for reproducibility. Default is `2026L`.
#' @param keep_replicates Logical; whether to include the vector of bootstrap
#'   sample means in the returned list.
#' @return A named list with `n`, `mean`, `boot_mean`, `boot_se`, `boot_bias`,
#'   `ci_low`, `ci_high`, `width`, `n_boot`, and optionally `replicates`.
#' @export
ci_mean_bootstrap <- function(
  x,
  conf_level = 0.95,
  n_boot = 5000L,
  seed = 2026L,
  keep_replicates = FALSE
) {
  vals <- clean_numeric(x, min_len = 2L)
  assert_number(conf_level, lower = 0.5, upper = 0.9999)
  assert_int(n_boot, lower = 100L)
  assert_int(seed)

  set.seed(seed)
  n <- length(vals)
  alpha <- 1 - conf_level

  mat <- matrix(
    sample(vals, size = n * n_boot, replace = TRUE),
    nrow = n_boot,
    ncol = n
  )
  boot_means <- rowMeans(mat)
  bounds <- unname(
    quantile(boot_means, probs = c(alpha / 2, 1 - alpha / 2))
  )
  obs_mean <- mean(vals)

  out <- list(
    n = n,
    mean = obs_mean,
    boot_mean = mean(boot_means),
    boot_se = sd(boot_means),
    boot_bias = mean(boot_means) - obs_mean,
    ci_low = bounds[[1]],
    ci_high = bounds[[2]],
    width = bounds[[2]] - bounds[[1]],
    n_boot = as.integer(n_boot)
  )
  if (isTRUE(keep_replicates)) {
    out$replicates <- boot_means
  }
  out
}

#' Two-sample confidence interval for the difference of means (Welch & Boot)
#'
#' Computes the confidence interval for `mean(x1) - mean(x2)` using both
#' Welch's parametric t-test (unequal variances) and two-sample independent
#' Percentile Bootstrap.
#'
#' @param x1 Numeric vector for group 1.
#' @param x2 Numeric vector for group 2.
#' @param conf_level Confidence level in `(0, 1)`. Default is `0.95`.
#' @param n_boot Number of bootstrap replicates. Default is `5000L`.
#' @param seed Integer random seed. Default is `2026L`.
#' @return A named list with group summaries, `diff`, Welch CI bounds,
#'   Bootstrap CI bounds, degrees of freedom, and `p_value`.
#' @export
ci_diff_means <- function(
  x1,
  x2,
  conf_level = 0.95,
  n_boot = 5000L,
  seed = 2026L
) {
  v1 <- clean_numeric(x1, min_len = 2L)
  v2 <- clean_numeric(x2, min_len = 2L)
  assert_number(conf_level, lower = 0.5, upper = 0.9999)
  assert_int(n_boot, lower = 100L)
  assert_int(seed)

  wt <- t.test(v1, v2, var.equal = FALSE, conf.level = conf_level)

  set.seed(seed)
  n1 <- length(v1)
  n2 <- length(v2)
  m1 <- rowMeans(
    matrix(sample(v1, size = n1 * n_boot, replace = TRUE), nrow = n_boot)
  )
  m2 <- rowMeans(
    matrix(sample(v2, size = n2 * n_boot, replace = TRUE), nrow = n_boot)
  )
  boot_diffs <- m1 - m2
  alpha <- 1 - conf_level
  b_bounds <- unname(
    quantile(boot_diffs, probs = c(alpha / 2, 1 - alpha / 2))
  )

  list(
    n1 = n1,
    n2 = n2,
    mean1 = mean(v1),
    mean2 = mean(v2),
    sd1 = sd(v1),
    sd2 = sd(v2),
    diff = mean(v1) - mean(v2),
    df = unname(wt$parameter),
    welch_low = wt$conf.int[[1]],
    welch_high = wt$conf.int[[2]],
    p_value = wt$p.value,
    boot_se = sd(boot_diffs),
    boot_low = b_bounds[[1]],
    boot_high = b_bounds[[2]]
  )
}

#' Confidence interval for a binomial proportion (Wald & Wilson score)
#'
#' @param k Integer count of successes (`0 <= k <= n`).
#' @param n Positive integer sample size.
#' @param conf_level Confidence level in `(0, 1)`. Default is `0.95`.
#' @return A named list with `k`, `n`, `prop`, `se`, `wald_low`, `wald_high`,
#'   `wilson_low`, and `wilson_high`.
#' @export
ci_proportion <- function(k, n, conf_level = 0.95) {
  assert_count(k)
  assert_int(n, lower = 1L)
  if (k > n) {
    stop("Success count 'k' cannot exceed sample size 'n'.", call. = FALSE)
  }
  assert_number(conf_level, lower = 0.5, upper = 0.9999)

  p_hat <- k / n
  alpha <- 1 - conf_level
  z <- qnorm(1 - alpha / 2)
  se <- sqrt(p_hat * (1 - p_hat) / n)

  denom <- 1 + (z^2) / n
  center <- (p_hat + (z^2) / (2 * n)) / denom
  half <- (z / denom) * sqrt((p_hat * (1 - p_hat) / n) + (z^2) / (4 * n^2))

  list(
    k = as.integer(k),
    n = as.integer(n),
    prop = p_hat,
    se = se,
    wald_low = max(0, p_hat - z * se),
    wald_high = min(1, p_hat + z * se),
    wilson_low = max(0, center - half),
    wilson_high = min(1, center + half)
  )
}

#' Confidence interval for the difference of two proportions (Wald)
#'
#' @param k1 Success count in group 1.
#' @param n1 Sample size in group 1.
#' @param k2 Success count in group 2.
#' @param n2 Sample size in group 2.
#' @param conf_level Confidence level in `(0, 1)`. Default is `0.95`.
#' @return A named list with `p1`, `p2`, `diff`, `se`, `ci_low`, `ci_high`.
#' @export
ci_diff_proportions <- function(k1, n1, k2, n2, conf_level = 0.95) {
  r1 <- ci_proportion(k1, n1, conf_level)
  r2 <- ci_proportion(k2, n2, conf_level)

  diff_p <- r1$prop - r2$prop
  se_diff <- sqrt(r1$se^2 + r2$se^2)
  z <- qnorm(1 - (1 - conf_level) / 2)

  list(
    p1 = r1$prop,
    p2 = r2$prop,
    diff = diff_p,
    se = se_diff,
    ci_low = diff_p - z * se_diff,
    ci_high = diff_p + z * se_diff
  )
}

#' Confidence interval for the ratio of two population variances (F-test)
#'
#' Computes the exact F-distribution interval for `var(x1) / var(x2)`.
#'
#' @param x1 Numeric vector for sample 1.
#' @param x2 Numeric vector for sample 2.
#' @param conf_level Confidence level in `(0, 1)`. Default is `0.95`.
#' @return A named list with `var1`, `var2`, `ratio`, `df1`, `df2`,
#'   `ci_low`, and `ci_high`.
#' @export
ci_var_ratio <- function(x1, x2, conf_level = 0.95) {
  v1 <- clean_numeric(x1, min_len = 2L)
  v2 <- clean_numeric(x2, min_len = 2L)
  assert_number(conf_level, lower = 0.5, upper = 0.9999)

  s2_1 <- var(v1)
  s2_2 <- var(v2)
  if (s2_2 == 0) {
    stop("Second sample has zero variance.", call. = FALSE)
  }

  df1 <- length(v1) - 1L
  df2 <- length(v2) - 1L
  alpha <- 1 - conf_level
  ratio <- s2_1 / s2_2

  f_low <- qf(alpha / 2, df1 = df1, df2 = df2)
  f_high <- qf(1 - alpha / 2, df1 = df1, df2 = df2)

  list(
    var1 = s2_1,
    var2 = s2_2,
    ratio = ratio,
    df1 = df1,
    df2 = df2,
    ci_low = ratio / f_high,
    ci_high = ratio / f_low
  )
}

#' Confidence interval for the difference of two paired proportions (Wald)
#'
#' Both binary outcomes are measured on the same subjects, so the variance
#' uses the discordant cells: `Var(d) = (p10 + p01 - d^2) / n`.
#'
#' @param y1 Logical vector of outcome 1 (e.g. passed Math).
#' @param y2 Logical vector of outcome 2, same length and subjects as `y1`.
#' @param conf_level Confidence level in `(0, 1)`. Default is `0.95`.
#' @return A named list with `n`, `n10`, `n01`, `p1`, `p2`, `diff`, `se`,
#'   `ci_low`, `ci_high`.
#' @export
ci_diff_proportions_paired <- function(y1, y2, conf_level = 0.95) {
  assert_logical(y1, any.missing = FALSE, min.len = 2L)
  assert_logical(y2, any.missing = FALSE, len = length(y1))
  assert_number(conf_level, lower = 0.5, upper = 0.9999)

  n <- length(y1)
  n10 <- sum(y1 & !y2)
  n01 <- sum(!y1 & y2)
  diff_p <- (n10 - n01) / n
  se <- sqrt(((n10 + n01) / n - diff_p^2) / n)
  z <- qnorm(1 - (1 - conf_level) / 2)

  list(
    n = n,
    n10 = n10,
    n01 = n01,
    p1 = mean(y1),
    p2 = mean(y2),
    diff = diff_p,
    se = se,
    ci_low = diff_p - z * se,
    ci_high = diff_p + z * se
  )
}

#' Paired Percentile Bootstrap CI for the ratio of two variances
#'
#' Resamples subjects (rows) jointly, so the dependence between `x1` and `x2`
#' is preserved and no normality is assumed (unlike the F interval).
#'
#' @param x1 Numeric vector for measurement 1.
#' @param x2 Numeric vector for measurement 2, same subjects as `x1`.
#' @param conf_level Confidence level in `(0, 1)`. Default is `0.95`.
#' @param n_boot Number of bootstrap replicates. Default is `5000L`.
#' @param seed Integer random seed. Default is `2026L`.
#' @return A named list with `var1`, `var2`, `ratio`, `ci_low`, `ci_high`,
#'   and `n_boot`.
#' @export
ci_var_ratio_paired_boot <- function(
  x1,
  x2,
  conf_level = 0.95,
  n_boot = 5000L,
  seed = 2026L
) {
  assert_numeric(x1, any.missing = FALSE, min.len = 2L)
  assert_numeric(x2, any.missing = FALSE, len = length(x1))
  assert_number(conf_level, lower = 0.5, upper = 0.9999)
  assert_int(n_boot, lower = 100L)
  assert_int(seed)
  if (var(x2) == 0) {
    stop("Second sample has zero variance.", call. = FALSE)
  }

  set.seed(seed)
  n <- length(x1)
  idx <- matrix(
    sample.int(n, size = n * n_boot, replace = TRUE),
    nrow = n_boot
  )
  ratios <- vapply(
    seq_len(n_boot),
    function(b) var(x1[idx[b, ]]) / var(x2[idx[b, ]]),
    numeric(1)
  )
  alpha <- 1 - conf_level
  bounds <- unname(
    quantile(ratios, probs = c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
  )

  list(
    var1 = var(x1),
    var2 = var(x2),
    ratio = var(x1) / var(x2),
    ci_low = bounds[[1]],
    ci_high = bounds[[2]],
    n_boot = as.integer(n_boot)
  )
}

#' Monte Carlo standard error of a bootstrap percentile bound
#'
#' Asymptotic approximation `sqrt(p (1 - p) / B) / f(q_p)`, with the density
#' `f` taken from a normal approximation of the bootstrap distribution.
#'
#' @param boot_se Standard deviation of the bootstrap replicates.
#' @param n_boot Number of bootstrap replicates.
#' @param p Percentile probability. Default is `0.025`.
#' @return Numeric scalar: simulation noise expected in that bound.
#' @export
boot_bound_mc_se <- function(boot_se, n_boot, p = 0.025) {
  assert_number(boot_se, lower = 0)
  assert_int(n_boot, lower = 1L)
  assert_number(p, lower = 0.001, upper = 0.999)
  density_q <- dnorm(qnorm(p)) / boot_se
  sqrt(p * (1 - p) / n_boot) / density_q
}

#' Compare parametric t-Student vs. Bootstrap CIs across columns
#'
#' @param data Non-empty `data.frame`.
#' @param cols Character vector of numeric column names in `data`.
#' @param conf_level Confidence level in `(0, 1)`. Default is `0.95`.
#' @param n_boot Number of bootstrap replicates. Default is `5000L`.
#' @param seed Integer random seed. Default is `2026L`.
#' @return A `data.frame` with one row per column comparing `t_low`, `t_high`,
#'   ` t_width` vs. `boot_low`, `boot_high`, `boot_width`, and `skewness`.
#' @export
ci_summary_table <- function(
  data,
  cols,
  conf_level = 0.95,
  n_boot = 5000L,
  seed = 2026L
) {
  assert_data_frame(data, min.rows = 2L)
  assert_subset(cols, choices = names(data))

  rows <- lapply(cols, function(col) {
    vals <- data[[col]]
    if (!is.numeric(vals)) {
      stop(sprintf("Column '%s' is not numeric.", col), call. = FALSE)
    }
    t_res <- ci_mean_t(vals, conf_level = conf_level)
    b_res <- ci_mean_bootstrap(
      vals,
      conf_level = conf_level,
      n_boot = n_boot,
      seed = seed
    )
    data.frame(
      variable = col,
      n = t_res$n,
      mean = t_res$mean,
      median = t_res$median,
      sd = t_res$sd,
      se = t_res$se,
      skewness = skewness(vals),
      t_low = t_res$ci_low,
      t_high = t_res$ci_high,
      t_width = t_res$width,
      boot_se = b_res$boot_se,
      boot_low = b_res$ci_low,
      boot_high = b_res$ci_high,
      boot_width = b_res$width,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}
