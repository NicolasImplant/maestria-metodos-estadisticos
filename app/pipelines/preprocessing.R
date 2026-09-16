#' @title Data Preprocessing Pipeline
#'
#' @description
#' Pure functional pipeline to clean and merge the Student Performance datasets
#' (Math and Portuguese).
#'
#' @details
#' \preformatted{
#' box::use(app/pipelines/preprocessing[clean_student_data, merge_student_data])
#' df_clean <- clean_student_data(df_raw)
#' df_merged <- merge_student_data(df_mat_clean, df_por_clean)
#' }

# styler: off
box::use(
  checkmate[assert_data_frame],
)
# styler: on

#' Clean G1 and G2 grade columns from string to integer
#'
#' Removes double quotes from G1 and G2 and explicitly casts them to integers.
#'
#' @param df A data frame containing G1 and G2 columns as strings.
#' @return A cleaned data frame with G1 and G2 as integers.
#' @export
clean_student_data <- function(df) {
  assert_data_frame(df, min.rows = 1L)

  # Resolve G1
  if ("G1" %in% colnames(df)) {
    # Strip quotes if character
    if (is.character(df$G1) || is.factor(df$G1)) {
      df$G1 <- as.integer(gsub('"', "", as.character(df$G1)))
    } else {
      df$G1 <- as.integer(df$G1)
    }
  }

  # Resolve G2
  if ("G2" %in% colnames(df)) {
    # Strip quotes if character
    if (is.character(df$G2) || is.factor(df$G2)) {
      df$G2 <- as.integer(gsub('"', "", as.character(df$G2)))
    } else {
      df$G2 <- as.integer(df$G2)
    }
  }

  # Cast G3 to integer just in case
  if ("G3" %in% colnames(df)) {
    df$G3 <- as.integer(df$G3)
  }

  df
}

#' Merge Math and Portuguese datasets on common demographic keys
#'
#' Merges both datasets using the 13-key demographic mapping to obtain the
#' common student cohort, using .mat and .por suffixes.
#'
#' @param df_mat Cleaned Math data frame.
#' @param df_por Cleaned Portuguese data frame.
#' @return A merged data frame of exactly 382 rows.
#' @export
merge_student_data <- function(df_mat, df_por) {
  assert_data_frame(df_mat, min.rows = 1L)
  assert_data_frame(df_por, min.rows = 1L)

  keys <- c(
    "school", "sex", "age", "address", "famsize", "Pstatus",
    "Medu", "Fedu", "Mjob", "Fjob", "reason", "nursery", "internet"
  )

  # Check that keys exist in both
  if (!all(keys %in% colnames(df_mat)) || !all(keys %in% colnames(df_por))) {
    stop("Not all 13 keys are present in both datasets.", call. = FALSE)
  }

  merged <- merge(df_mat, df_por, by = keys, suffixes = c(".mat", ".por"))

  merged
}
