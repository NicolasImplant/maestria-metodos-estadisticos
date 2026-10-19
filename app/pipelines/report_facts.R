#' @title Pure report facts for the student-performance activity
#'
#' @description
#' Side-effect-free helpers that compute every number quoted in the report,
#' the slides and the chart annotations from the merged Math/Portuguese data
#' frame. Keeping them in one place guarantees that text and data never drift
#' apart (no hard-coded figures in captions).
#'
#' @details
#' \preformatted{
#' box::use(app/pipelines/report_facts[report_facts, fmt_num])
#' facts <- report_facts(merged_df)
#' fmt_num(facts$corr$subjects$pearson, 2)
#' }

# styler: off
box::use(
  checkmate[
    assert_data_frame, assert_names, assert_number, assert_numeric,
  ],
  stats[cor, cor.test, median, sd, setNames, t.test],
  app/pipelines/summary_stats[iqr_outliers],
)
# styler: on

stratum_levels <- c("Bajo (1-2)", "Moderado (3)", "Alto (4-5)")

required_columns <- c(
  "sex", "address", "age", "traveltime.mat",
  "G3.mat", "G3.por", "Walc.mat", "Walc.por",
  "absences.mat", "absences.por"
)

#' Group the 1-5 weekend alcohol scale into three balanced strata
#'
#' Level 5 alone has very few students (e.g. only 4 women), so the scale is
#' grouped as low (1-2), moderate (3) and high (4-5).
#'
#' @param x Numeric vector with values between 1 and 5.
#' @return An ordered-level factor with levels `Bajo (1-2)`,
#'   `Moderado (3)` and `Alto (4-5)`.
#' @export
walc_stratum <- function(x) {
  assert_numeric(x, lower = 1, upper = 5)
  cut(x, breaks = c(0, 2, 3, 5), labels = stratum_levels)
}

#' Format a number with a fixed number of decimals (dot as separator)
#'
#' @param x Numeric vector.
#' @param digits Number of decimals.
#' @return A character vector.
#' @export
fmt_num <- function(x, digits = 1L) {
  assert_numeric(x)
  assert_number(digits, lower = 0)
  formatC(x, format = "f", digits = digits)
}

#' Format a p-value for prose
#'
#' @param p A single p-value between 0 and 1.
#' @return `"< 0.001"` for very small values, otherwise the value rounded to
#'   three decimals.
#' @export
fmt_p <- function(p) {
  assert_number(p, lower = 0, upper = 1)
  if (p < 0.001) "< 0.001" else formatC(p, format = "f", digits = 3L)
}

# Column of one subject, e.g. .col(df, "G3", "mat") -> df$G3.mat
.col <- function(df, base, subject) {
  df[[paste0(base, ".", subject)]]
}

.association <- function(x, y) {
  sp <- cor.test(x, y, method = "spearman", exact = FALSE)
  list(
    pearson = cor(x, y),
    spearman = unname(sp$estimate),
    p_spearman = sp$p.value
  )
}

# Welch difference of means (a - b) with its 95% confidence interval.
.welch <- function(a, b) {
  if (length(a) < 2L || length(b) < 2L) {
    return(list(diff = NA_real_, low = NA_real_, high = NA_real_, p = NA_real_))
  }
  tt <- t.test(a, b)
  list(
    diff = mean(a) - mean(b),
    low = tt$conf.int[[1L]],
    high = tt$conf.int[[2L]],
    p = tt$p.value
  )
}

.high_alcohol <- function(df, subject, good_grade) {
  high <- .col(df, "Walc", subject) >= 4
  good <- high & .col(df, "G3", subject) >= good_grade
  list(
    n_high = sum(high),
    n_good = sum(good),
    pct_good = sum(good) / sum(high)
  )
}

.strata_table <- function(df, subject) {
  stratum <- walc_stratum(.col(df, "Walc", subject))
  g3 <- .col(df, "G3", subject)
  cells <- expand.grid(
    sex = c("F", "M"), stratum = stratum_levels, stringsAsFactors = FALSE
  )
  rows <- lapply(seq_len(nrow(cells)), function(i) {
    y <- g3[df$sex == cells$sex[i] & stratum == cells$stratum[i]]
    data.frame(
      subject = subject,
      sex = cells$sex[i],
      stratum = factor(cells$stratum[i], levels = stratum_levels),
      n = length(y),
      mean = mean(y),
      se = sd(y) / sqrt(length(y))
    )
  })
  do.call(rbind, rows)
}

.address_gap <- function(df, subject) {
  g3 <- .col(df, "G3", subject)
  urban <- g3[df$address == "U"]
  rural <- g3[df$address == "R"]
  gap <- .welch(rural, urban)
  data.frame(
    subject = subject,
    n_urban = length(urban),
    n_rural = length(rural),
    mean_urban = mean(urban),
    mean_rural = mean(rural),
    diff = gap$diff,
    ci_low = gap$low,
    ci_high = gap$high,
    p = gap$p,
    rural_above_urban = sum(rural > mean(urban))
  )
}

.absence_profile <- function(df, subject) {
  absences <- .col(df, "absences", subject)
  g3 <- .col(df, "G3", subject)
  fences <- iqr_outliers(df, paste0("absences.", subject))
  flag <- absences < fences$lower_bound | absences > fences$upper_bound
  zero <- g3 == 0
  gap <- .welch(g3[flag], g3[!flag])
  data.frame(
    subject = subject,
    n_out = sum(flag),
    pct_out = mean(flag),
    upper = fences$upper_bound,
    max_absences = max(absences),
    mean_out = mean(g3[flag]),
    mean_rest = mean(g3[!flag]),
    median_out = median(g3[flag]),
    median_rest = median(g3[!flag]),
    diff = gap$diff,
    ci_low = gap$low,
    ci_high = gap$high,
    p = gap$p,
    n_zero = sum(zero),
    n_zero_no_absences = sum(zero & absences == 0),
    mean_rest_nonzero = mean(g3[!flag & !zero])
  )
}

#' Compute every figure quoted in the report and slides
#'
#' @param merged_df Merged Math/Portuguese data frame (one row per student)
#'   with the columns `sex`, `address`, `age`, `traveltime.mat`, and the
#'   `G3`, `Walc` and `absences` columns suffixed `.mat` / `.por`.
#' @param good_grade Grade (0-20) that counts as "a grade of 14 or more".
#' @return A named list: `cohort`, `corr`, `high_alcohol`, `strata`,
#'   `walc5_by_sex`, `address`, `long_travel`, `absences`.
#'   `traveltime` is an ordinal code (1: <15 min, 2: 15-30, 3: 30-60,
#'   4: >60), so `long_travel` is the share with code 3 or 4 (30 min or more),
#'   taken from the Math questionnaire.
#' @export
report_facts <- function(merged_df, good_grade = 14) {
  assert_data_frame(merged_df, min.rows = 3L)
  assert_names(names(merged_df), must.include = required_columns)
  assert_number(good_grade, lower = 0, upper = 20)

  subjects <- c("mat", "por")
  by_subject <- function(fn) {
    setNames(lapply(subjects, fn), subjects)
  }
  bind <- function(rows) do.call(rbind, unname(rows))

  travel_long <- merged_df$traveltime.mat >= 3L
  women_por <- merged_df$sex == "F"
  stratum_por <- walc_stratum(merged_df$Walc.por)

  list(
    cohort = list(
      n = nrow(merged_df),
      pct_urban = mean(merged_df$address == "U"),
      pct_female = mean(women_por),
      mean_age = mean(merged_df$age)
    ),
    corr = list(
      subjects = .association(merged_df$G3.por, merged_df$G3.mat),
      alcohol_mat = .association(merged_df$Walc.mat, merged_df$G3.mat),
      alcohol_por = .association(merged_df$Walc.por, merged_df$G3.por)
    ),
    high_alcohol = by_subject(function(s) {
      .high_alcohol(merged_df, s, good_grade)
    }),
    strata = bind(by_subject(function(s) .strata_table(merged_df, s))),
    walc5_by_sex = c(
      F = sum(merged_df$Walc.por == 5 & women_por),
      M = sum(merged_df$Walc.por == 5 & !women_por)
    ),
    women_low_moderate = mean(stratum_por[women_por] != "Alto (4-5)"),
    address = bind(by_subject(function(s) .address_gap(merged_df, s))),
    long_travel = c(
      urban = mean(travel_long[merged_df$address == "U"]),
      rural = mean(travel_long[merged_df$address == "R"])
    ),
    absences = bind(by_subject(function(s) .absence_profile(merged_df, s)))
  )
}
