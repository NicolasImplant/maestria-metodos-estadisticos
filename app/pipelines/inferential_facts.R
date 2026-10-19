#' @title Pure inferential facts for the Unit 3 Confidence Intervals activity
#'
#' @description
#' Computes all parametric and Bootstrap confidence interval facts for the
#' merged Student Performance cohort (N = 382), ensuring that prose, tables,
#' charts, and slides in Unit 3 cite identical, dynamically verified numbers.
#'
#' @details
#' \preformatted{
#' box::use(app/pipelines/inferential_facts[inferential_facts, fmt_num, fmt_ci])
#' facts <- inferential_facts(merged_df)
#' }

# styler: off
box::use(
  checkmate[assert_data_frame, assert_int, assert_names],
  app/pipelines/confidence_intervals[
    boot_bound_mc_se, ci_diff_means, ci_diff_proportions,
    ci_diff_proportions_paired, ci_mean_bootstrap, ci_mean_t, ci_proportion,
    ci_summary_table, ci_var_ratio, ci_var_ratio_paired_boot,
  ],
  app/pipelines/report_facts[fmt_num, report_facts],
)
# styler: on

required_cols <- c(
  "sex", "address", "age", "traveltime.mat",
  "G3.mat", "G3.por", "Walc.mat", "Walc.por",
  "absences.mat", "absences.por"
)

#' Re-export fmt_num for convenience in Unit 3 documents
#'
#' @param x Numeric scalar or vector.
#' @param digits Number of decimal places. Default is `2`.
#' @return Formatted character vector.
#' @export
fmt_num <- fmt_num

#' Format a confidence interval as `[low, high]`
#'
#' @param low Lower bound.
#' @param high Upper bound.
#' @param digits Decimal places. Default is `2`.
#' @return Character scalar formatted as `"[low, high]"`.
#' @export
fmt_ci <- function(low, high, digits = 2) {
  sprintf("[%s, %s]", fmt_num(low, digits), fmt_num(high, digits))
}

walc_contrasts <- function(df, n_boot, seed) {
  subjects <- c("mat", "por")
  rows <- lapply(subjects, function(s) {
    g_col <- paste0("G3.", s)
    a_col <- paste0("absences.", s)
    w_col <- paste0("Walc.", s)

    is_low <- df[[w_col]] <= 2
    is_high <- df[[w_col]] >= 4

    ci_g <- ci_diff_means(
      df[[g_col]][is_high],
      df[[g_col]][is_low],
      n_boot = n_boot,
      seed = seed
    )
    ci_a <- ci_diff_means(
      df[[a_col]][is_high],
      df[[a_col]][is_low],
      n_boot = n_boot,
      seed = seed
    )

    data.frame(
      subject = s,
      n_high = ci_g$n1,
      n_low = ci_g$n2,
      grade_high = ci_g$mean1,
      grade_low = ci_g$mean2,
      grade_diff = ci_g$diff,
      grade_welch_low = ci_g$welch_low,
      grade_welch_high = ci_g$welch_high,
      grade_boot_low = ci_g$boot_low,
      grade_boot_high = ci_g$boot_high,
      grade_p = ci_g$p_value,
      abs_high = ci_a$mean1,
      abs_low = ci_a$mean2,
      abs_diff = ci_a$diff,
      abs_welch_low = ci_a$welch_low,
      abs_welch_high = ci_a$welch_high,
      abs_boot_low = ci_a$boot_low,
      abs_boot_high = ci_a$boot_high,
      abs_p = ci_a$p_value,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

pass_rate_facts <- function(df) {
  pass_mat <- sum(df$G3.mat >= 10)
  pass_por <- sum(df$G3.por >= 10)
  n_tot <- nrow(df)

  high_walc_mat <- df$Walc.mat >= 4
  low_walc_mat <- df$Walc.mat <= 2
  high_walc_por <- df$Walc.por >= 4
  low_walc_por <- df$Walc.por <= 2

  list(
    mat = ci_proportion(pass_mat, n_tot),
    por = ci_proportion(pass_por, n_tot),
    diff_por_mat = ci_diff_proportions_paired(df$G3.por >= 10, df$G3.mat >= 10),
    diff_mat_por = ci_diff_proportions_paired(df$G3.mat >= 10, df$G3.por >= 10),
    walc_diff_mat = ci_diff_proportions(
      sum(df$G3.mat[high_walc_mat] >= 10),
      sum(high_walc_mat),
      sum(df$G3.mat[low_walc_mat] >= 10),
      sum(low_walc_mat)
    ),
    walc_diff_por = ci_diff_proportions(
      sum(df$G3.por[high_walc_por] >= 10),
      sum(high_walc_por),
      sum(df$G3.por[low_walc_por] >= 10),
      sum(low_walc_por)
    )
  )
}

extract_uni <- function(uni_tbl, var_name) {
  r <- uni_tbl[uni_tbl$variable == var_name, ]
  list(
    n = r$n,
    mean = r$mean,
    median = r$median,
    sd = r$sd,
    se = r$se,
    skewness = r$skewness,
    t_low = r$t_low,
    t_high = r$t_high,
    t_half = r$t_width / 2,
    boot_low = r$boot_low,
    boot_high = r$boot_high,
    boot_half = r$boot_width / 2
  )
}

contrast_item <- function(x1, x2, n_boot, seed) {
  res <- ci_diff_means(x1, x2, n_boot = n_boot, seed = seed)
  res$excludes_zero <- (res$welch_low > 0) || (res$welch_high < 0)
  res
}

build_contrasts <- function(df, n_boot, seed) {
  h_mat <- df$Walc.mat >= 4
  l_mat <- df$Walc.mat <= 2
  h_por <- df$Walc.por >= 4
  l_por <- df$Walc.por <= 2
  is_r <- df$address == "R"
  is_u <- df$address == "U"

  list(
    alcohol = list(
      g3_por = contrast_item(
        df$G3.por[h_por], df$G3.por[l_por], n_boot, seed
      ),
      g3_mat = contrast_item(
        df$G3.mat[h_mat], df$G3.mat[l_mat], n_boot, seed
      ),
      abs_mat = contrast_item(
        df$absences.mat[h_mat], df$absences.mat[l_mat], n_boot, seed
      ),
      abs_por = contrast_item(
        df$absences.por[h_por], df$absences.por[l_por], n_boot, seed
      )
    ),
    address = list(
      g3_por = contrast_item(
        df$G3.por[is_r], df$G3.por[is_u], n_boot, seed
      ),
      g3_mat = contrast_item(
        df$G3.mat[is_r], df$G3.mat[is_u], n_boot, seed
      )
    )
  )
}

#' Compute all inferential facts for Unit 3
#'
#' @param df Merged Math/Portuguese data frame (`N = 382`).
#' @param n_boot Number of bootstrap replicates. Default is `5000L`.
#' @param seed Random seed. Default is `2026L`.
#' @return A nested list containing all parametric and Bootstrap facts.
#' @export
inferential_facts <- function(df, n_boot = 5000L, seed = 2026L) {
  assert_data_frame(df, min.rows = 2L)
  assert_names(names(df), must.include = required_cols)
  assert_int(n_boot, lower = 100L)
  assert_int(seed)

  cols <- c(
    "G3.mat", "G3.por", "absences.mat", "absences.por", "Walc.mat", "Walc.por"
  )
  uni_tbl <- ci_summary_table(df, cols, n_boot = n_boot, seed = seed)

  boot_dists <- lapply(
    c(
      G3.mat = "G3.mat", G3.por = "G3.por",
      absences.mat = "absences.mat", absences.por = "absences.por"
    ),
    function(col) {
      list(
        t_ci = ci_mean_t(df[[col]]),
        boot_ci = ci_mean_bootstrap(
          df[[col]],
          n_boot = n_boot,
          seed = seed,
          keep_replicates = TRUE
        )
      )
    }
  )

  d_grade <- df$G3.por - df$G3.mat
  paired_t <- ci_mean_t(d_grade)
  paired_b <- ci_mean_bootstrap(d_grade, n_boot = n_boot, seed = seed)
  pr <- pass_rate_facts(df)
  # Same students in both subjects: paired bootstrap, not the F interval
  vr_g3 <- ci_var_ratio_paired_boot(
    df$G3.mat, df$G3.por,
    n_boot = n_boot, seed = seed
  )
  vr_abs <- ci_var_ratio_paired_boot(
    df$absences.mat, df$absences.por,
    n_boot = n_boot, seed = seed
  )
  vr_g3_f <- ci_var_ratio(df$G3.mat, df$G3.por)
  abs_mat_row <- uni_tbl[uni_tbl$variable == "absences.mat", ]
  desc <- report_facts(df)
  contrasts <- build_contrasts(df, n_boot = n_boot, seed = seed)

  list(
    descriptive = desc,
    cohort = list(
      n = nrow(df),
      conf_level = 0.95,
      n_boot = n_boot,
      seed = seed
    ),
    univariate_table = uni_tbl,
    univariate = list(
      g3_mat = extract_uni(uni_tbl, "G3.mat"),
      g3_por = extract_uni(uni_tbl, "G3.por"),
      abs_mat = extract_uni(uni_tbl, "absences.mat"),
      abs_por = extract_uni(uni_tbl, "absences.por"),
      walc = extract_uni(uni_tbl, "Walc.mat")
    ),
    boot_distributions = boot_dists,
    walc_contrasts = walc_contrasts(df, n_boot = n_boot, seed = seed),
    contrasts = contrasts,
    paired_grade = list(
      mean_diff = paired_t$mean,
      sd_diff = paired_t$sd,
      t_low = paired_t$ci_low,
      t_high = paired_t$ci_high,
      boot_low = paired_b$ci_low,
      boot_high = paired_b$ci_high
    ),
    paired_g3 = list(
      mean = paired_t$mean,
      sd = paired_t$sd,
      t_low = paired_t$ci_low,
      t_high = paired_t$ci_high,
      boot_low = paired_b$ci_low,
      boot_high = paired_b$ci_high
    ),
    pass_rates = pr,
    proportions = list(
      pass_mat = pr$mat,
      pass_por = pr$por,
      diff_pass = pr$diff_mat_por
    ),
    var_ratios = list(
      grades_mat_por = vr_g3,
      absences_mat_por = vr_abs,
      g3 = vr_g3,
      absences = vr_abs,
      g3_f_independent = vr_g3_f
    ),
    mc_error = list(
      abs_mat_lower = boot_bound_mc_se(abs_mat_row$boot_se, n_boot)
    ),
    alcohol_max_p = max(
      vapply(
        c("g3_por", "abs_mat", "abs_por"),
        function(k) contrasts$alcohol[[k]]$p_value,
        numeric(1)
      )
    )
  )
}
