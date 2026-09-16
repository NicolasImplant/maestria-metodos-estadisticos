# styler: off
box::use(
  testthat[
    test_that, expect_true, expect_error, expect_identical, expect_equal
  ],
  app/pipelines/preprocessing[clean_student_data, merge_student_data],
)
# styler: on

test_that("clean_student_data cleans quoted string numbers in G1 and G2", {
  # Test df with quotes
  raw_df <- data.frame(
    school = c("GP", "MS"),
    G1 = c('"5"', '"15"'),
    G2 = c('"6"', '"14"'),
    G3 = c(6L, 14L),
    stringsAsFactors = FALSE
  )

  cleaned <- clean_student_data(raw_df)

  expect_identical(cleaned$G1, c(5L, 15L))
  expect_identical(cleaned$G2, c(6L, 14L))
  expect_true(is.integer(cleaned$G1))
  expect_true(is.integer(cleaned$G2))
})

test_that("clean_student_data leaves numeric grades untouched", {
  raw_df <- data.frame(
    school = c("GP", "MS"),
    G1 = c(5L, 15L),
    G2 = c(6, 14), # numeric double
    stringsAsFactors = FALSE
  )

  cleaned <- clean_student_data(raw_df)

  expect_identical(cleaned$G1, c(5L, 15L))
  expect_identical(cleaned$G2, c(6L, 14L)) # cast to integer
})

test_that("merge_student_data merges cohort using 13 keys and suffixes", {
  # Create mock math df
  df_mat <- data.frame(
    school = "GP", sex = "F", age = 15L, address = "U",
    famsize = "GT3", Pstatus = "T", Medu = 4L, Fedu = 4L,
    Mjob = "teacher", Fjob = "other", reason = "home",
    nursery = "yes", internet = "yes", absences = 4L,
    G1 = 12L, G2 = 13L, G3 = 14L,
    stringsAsFactors = FALSE
  )

  # Create mock portuguese df
  df_por <- data.frame(
    school = "GP", sex = "F", age = 15L, address = "U",
    famsize = "GT3", Pstatus = "T", Medu = 4L, Fedu = 4L,
    Mjob = "teacher", Fjob = "other", reason = "home",
    nursery = "yes", internet = "yes", absences = 2L,
    G1 = 15L, G2 = 16L, G3 = 17L,
    stringsAsFactors = FALSE
  )

  merged <- merge_student_data(df_mat, df_por)

  expect_equal(nrow(merged), 1L)
  # Verify suffixes
  expect_true("absences.mat" %in% colnames(merged))
  expect_true("absences.por" %in% colnames(merged))
  expect_true("G3.mat" %in% colnames(merged))
  expect_true("G3.por" %in% colnames(merged))

  expect_equal(merged$absences.mat, 4L)
  expect_equal(merged$absences.por, 2L)
})
