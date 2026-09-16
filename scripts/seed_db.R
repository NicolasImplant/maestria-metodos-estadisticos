#!/usr/bin/env Rscript
# Seed the local SQLite database from raw CSV files for Unit 2.
#
#   Rscript scripts/seed_db.R

message("Seeding database...")

# Paths
db_dir <- file.path("actividades", "02-estadistica-descriptiva", "datos")
db_path <- file.path(db_dir, "student_performance.db")
csv_mat <- file.path(db_dir, "student-mat.csv")
csv_por <- file.path(db_dir, "student-por.csv")

if (!file.exists(csv_mat) || !file.exists(csv_por)) {
  stop(
    "Raw CSV files not found in activities directory.\n",
    "Please make sure you copied them from Modulo 2/",
    call. = FALSE
  )
}

# Read CSVs using semicolon separator
df_mat <- read.table(
  csv_mat,
  sep = ";",
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
df_por <- read.table(
  csv_por,
  sep = ";",
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Connect to SQLite
con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

# Write tables
DBI::dbWriteTable(con, "student_mat", df_mat, overwrite = TRUE)
DBI::dbWriteTable(con, "student_por", df_por, overwrite = TRUE)

# Verify
tables <- DBI::dbListTables(con)
message("Database created successfully at: ", db_path)
message("Tables: ", paste(tables, collapse = ", "))

cnt_mat <- DBI::dbGetQuery(con, "SELECT COUNT(*) FROM student_mat")[[1]]
cnt_por <- DBI::dbGetQuery(con, "SELECT COUNT(*) FROM student_por")[[1]]
message("student_mat: ", cnt_mat, " rows")
message("student_por: ", cnt_por, " rows")

# Disconnect
DBI::dbDisconnect(con)
