# styler: off
box::use(
  testthat[
    test_that, expect_equal, expect_error, expect_identical, expect_true,
    expect_named, expect_lt,
  ],
  stats[cor, sd, t.test],
  app/pipelines/report_facts[report_facts, walc_stratum, fmt_num, fmt_p],
)
# styler: on

# Deterministic 40-student fixture: 20 urban / 20 rural, 20 F / 20 M.
make_students <- function() {
  n <- 40L
  i <- seq_len(n)
  data.frame(
    sex = rep(c("F", "M"), each = 20L),
    address = rep(c("U", "R"), times = 20L),
    age = 15L + (i %% 4L),
    traveltime.mat = rep(c(1L, 3L), times = 20L),
    G3.mat = (i * 7L) %% 21L,
    G3.por = 8L + (i * 3L) %% 11L,
    Walc.mat = 1L + (i %% 5L),
    Walc.por = 1L + ((i + 1L) %% 5L),
    absences.mat = c(rep(0L, 36L), 30L, 40L, 50L, 60L),
    absences.por = c(rep(2L, 37L), 25L, 30L, 35L)
  )
}

test_that("walc_stratum groups the 1-5 scale into three ordered strata", {
  out <- walc_stratum(c(1, 2, 3, 4, 5))

  expect_identical(
    as.character(out),
    c("Bajo (1-2)", "Bajo (1-2)", "Moderado (3)", "Alto (4-5)", "Alto (4-5)")
  )
  expect_identical(
    levels(out), c("Bajo (1-2)", "Moderado (3)", "Alto (4-5)")
  )
  expect_error(walc_stratum(6), "<= 5")
})

test_that("fmt_num and fmt_p format numbers for prose", {
  expect_identical(fmt_num(3.14159, 2), "3.14")
  expect_identical(fmt_num(c(1, 2.5)), c("1.0", "2.5"))
  expect_identical(fmt_p(0.0004), "< 0.001")
  expect_identical(fmt_p(0.0688), "0.069")
  expect_error(fmt_p(1.5))
})

test_that("report_facts returns every block with consistent counts", {
  df <- make_students()
  facts <- report_facts(df)

  expect_named(
    facts,
    c(
      "cohort", "corr", "high_alcohol", "strata", "walc5_by_sex",
      "women_low_moderate", "address", "long_travel", "absences"
    )
  )
  expect_equal(facts$cohort$n, 40L)
  expect_equal(facts$cohort$pct_urban, 0.5)
  expect_equal(facts$cohort$pct_female, 0.5)

  # Strata cells add up to the whole sample for each subject.
  expect_equal(sum(facts$strata$n[facts$strata$subject == "mat"]), 40L)
  expect_equal(sum(facts$strata$n[facts$strata$subject == "por"]), 40L)
  expect_equal(nrow(facts$strata), 12L)
})

test_that("report_facts matches direct base-R computations", {
  df <- make_students()
  facts <- report_facts(df)

  expect_equal(
    facts$corr$subjects$pearson, cor(df$G3.por, df$G3.mat)
  )
  expect_equal(
    facts$corr$alcohol_mat$spearman,
    cor(df$Walc.mat, df$G3.mat, method = "spearman")
  )

  high <- df$Walc.mat >= 4
  expect_equal(facts$high_alcohol$mat$n_high, sum(high))
  expect_equal(
    facts$high_alcohol$mat$n_good, sum(high & df$G3.mat >= 14)
  )

  cell <- facts$strata[
    facts$strata$subject == "mat" & facts$strata$sex == "M" &
      facts$strata$stratum == "Bajo (1-2)",
  ]
  y <- df$G3.mat[df$sex == "M" & df$Walc.mat <= 2]
  expect_equal(cell$n, length(y))
  expect_equal(cell$mean, mean(y))
  expect_equal(cell$se, sd(y) / sqrt(length(y)))
})

test_that("address gap is rural minus urban with a Welch interval", {
  df <- make_students()
  gap <- report_facts(df)$address
  mat <- gap[gap$subject == "mat", ]
  rural <- df$G3.mat[df$address == "R"]
  urban <- df$G3.mat[df$address == "U"]
  tt <- t.test(rural, urban)

  expect_equal(mat$diff, mean(rural) - mean(urban))
  expect_equal(mat$ci_low, tt$conf.int[[1L]])
  expect_equal(mat$ci_high, tt$conf.int[[2L]])
  expect_equal(mat$rural_above_urban, sum(rural > mean(urban)))
  expect_equal(report_facts(df)$long_travel[["rural"]], 1)
  expect_equal(report_facts(df)$long_travel[["urban"]], 0)
})

test_that("absence profile flags Tukey outliers and reports zero grades", {
  df <- make_students()
  df$G3.mat[df$absences.mat == 0][1:3] <- 0L
  prof <- report_facts(df)$absences
  mat <- prof[prof$subject == "mat", ]

  expect_equal(mat$n_out, 4L)
  expect_equal(mat$max_absences, 60L)
  expect_equal(
    mat$n_zero_no_absences, sum(df$G3.mat == 0 & df$absences.mat == 0)
  )
  expect_true(mat$mean_rest_nonzero > mat$mean_rest)
  expect_lt(mat$pct_out, 0.11)
})

test_that("report_facts validates its input", {
  df <- make_students()

  expect_error(report_facts(df[, setdiff(names(df), "G3.mat")]), "G3.mat")
  expect_error(report_facts("not a data frame"))
  expect_error(report_facts(df, good_grade = 25))
})
