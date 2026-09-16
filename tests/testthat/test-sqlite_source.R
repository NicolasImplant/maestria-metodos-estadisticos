# styler: off
box::use(
  testthat[
    test_that, expect_true, expect_error, expect_s3_class, expect_identical
  ],
  DBI[dbConnect, dbDisconnect, dbWriteTable],
  RSQLite[SQLite],
  app/services/sqlite_source[SqliteSource],
)
# styler: on

test_that("SqliteSource constructor enforces checkmate rules", {
  expect_error(
    SqliteSource$new(db_path = "non_existent.db", table_name = "test")
  )
  expect_error(
    SqliteSource$new(db_path = tempfile(fileext = ".db"), table_name = "")
  )
})

test_that("SqliteSource can connect, read, and disconnect successfully", {
  # Setup temporary database
  tmp_db <- tempfile(fileext = ".db")
  on.exit(unlink(tmp_db), add = TRUE)

  con <- dbConnect(SQLite(), tmp_db)
  mock_data <- data.frame(
    id = 1:5,
    value = letters[1:5],
    stringsAsFactors = FALSE
  )
  dbWriteTable(con, "test_table", mock_data)
  dbDisconnect(con)

  # Use SqliteSource
  src <- SqliteSource$new(db_path = tmp_db, table_name = "test_table")
  expect_s3_class(src, "SqliteSource")
  expect_s3_class(src, "DataSource")

  # Reading before connect should fail
  expect_error(src$read(), regexp = "not connected")

  # Connect and read
  src$connect()
  res <- src$read()
  expect_identical(nrow(res), 5L)
  expect_identical(ncol(res), 2L)
  expect_identical(res$value, mock_data$value)

  # Disconnect
  src$disconnect()
  expect_error(src$read(), regexp = "not connected")
})
