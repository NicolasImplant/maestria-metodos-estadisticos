# styler: off
box::use(
  testthat[
    test_that, expect_true, expect_error, expect_s3_class, expect_identical,
  ],
  app/core/factory[data_source_factory, supported_source_kinds],
)
# styler: on

test_that("the factory advertises its supported kinds", {
  expect_true("csv" %in% supported_source_kinds())
})

test_that("the factory builds a CsvSource for kind = 'csv'", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  write.csv(data.frame(a = 1:3, b = 4:6), tmp, row.names = FALSE)

  src <- data_source_factory("csv", path = tmp)

  expect_s3_class(src, "CsvSource")
  expect_s3_class(src, "DataSource")
  expect_identical(nrow(src$connect()$read()), 3L)
})

test_that("the factory rejects unknown kinds", {
  expect_error(data_source_factory("parquet"), regexp = "choice|parquet")
})
