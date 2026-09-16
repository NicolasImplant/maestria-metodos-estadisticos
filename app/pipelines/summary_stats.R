#' @title Pure summary-statistics transforms
#'
#' @description
#' Side-effect-free functions that turn a `data.frame` into descriptive
#' summaries. Analytical logic is kept separate from service/infrastructure
#' code so it can be unit-tested in isolation and composed with the native
#' pipe (`|>`).
#'
#' @details
#' \preformatted{
#' box::use(app/pipelines/summary_stats[numeric_summary, categorical_summary])
#' numeric_summary(mtcars, c("mpg", "hp"))
#' }

# styler: off
box::use(
  checkmate[assert_data_frame, assert_numeric, assert_string, assert_subset],
  stats[median, quantile, sd, var],
)
# styler: on

#' Helper function to calculate statistical mode deterministically
#'
#' Finds the most frequent value. If there are ties, selects the smallest
#' numeric or alphabetical value.
#'
#' @param x Vector of values.
#' @return The mode value of the same type as x.
#' @export
get_mode <- function(x) {
  # Strip NA
  x <- x[!is.na(x)]
  if (length(x) == 0L) {
    return(NA)
  }
  tbl <- table(x)
  modes <- names(tbl)[tbl == max(tbl)]

  # Attempt numeric conversion to sort/select min
  val_num <- suppressWarnings(as.numeric(modes))
  if (any(is.na(val_num))) {
    # Fallback to character alphabetical sorting
    sorted_modes <- sort(modes)
    sorted_modes[[1]]
  } else {
    # Numeric sorting
    min(val_num)
  }
}

#' Describe the numeric columns of a data frame
#'
#' @param data A non-empty `data.frame`.
#' @param cols Character vector of column names to describe. All must be
#'   numeric columns present in `data`.
#' @return A `data.frame` with one row per requested column and the columns:
#'   `variable`, `n`, `n_missing`, `mean`, `median`, `mode`, `min`, `max`,
#'   `sd`, `var`, `range`.
#' @export
numeric_summary <- function(data, cols) {
  assert_data_frame(data, min.rows = 1L)
  assert_subset(cols, choices = names(data))

  rows <- lapply(cols, function(col) {
    values <- data[[col]]
    if (!is.numeric(values)) {
      stop(sprintf("Column '%s' is not numeric.", col), call. = FALSE)
    }
    complete <- values[!is.na(values)]

    n_val <- length(values)
    n_miss <- sum(is.na(values))

    if (length(complete) == 0L) {
      return(data.frame(
        variable = col,
        n = n_val,
        n_missing = n_miss,
        mean = NA_real_,
        median = NA_real_,
        mode = NA_real_,
        min = NA_real_,
        max = NA_real_,
        sd = NA_real_,
        var = NA_real_,
        range = NA_real_,
        stringsAsFactors = FALSE
      ))
    }

    min_val <- min(complete)
    max_val <- max(complete)

    data.frame(
      variable = col,
      n = n_val,
      n_missing = n_miss,
      mean = mean(complete),
      median = median(complete),
      mode = get_mode(complete),
      min = min_val,
      max = max_val,
      sd = sd(complete),
      var = var(complete),
      range = max_val - min_val,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

#' Describe the categorical columns of a data frame
#'
#' Generates frequency tables showing counts and relative frequencies
#' (proportions) for the specified categorical columns.
#'
#' @param data A non-empty `data.frame`.
#' @param cols Character vector of column names to describe. All must be
#'   character or factor columns present in `data`.
#' @return A `data.frame` with columns `variable`, `value`, `count`,
#'   `proportion`.
#' @export
categorical_summary <- function(data, cols) {
  assert_data_frame(data, min.rows = 1L)
  assert_subset(cols, choices = names(data))

  rows <- lapply(cols, function(col) {
    values <- data[[col]]
    if (!is.character(values) && !is.factor(values)) {
      stop(
        sprintf("Column '%s' is not character or factor.", col),
        call. = FALSE
      )
    }

    # Make table, including NA
    tbl <- table(values, useNA = "always")
    df <- as.data.frame(tbl, responseName = "count", stringsAsFactors = FALSE)
    colnames(df)[[1]] <- "value"

    # Convert NA to readable string "Missing" or keep as NA
    df$value[is.na(df$value)] <- "Missing"

    # Exclude rows with 0 count (which table can produce for factors with unused
    # levels, or NA when there are no NAs)
    df <- df[df$count > 0L, ]

    df$variable <- col
    df$proportion <- df$count / sum(df$count)

    # Reorder columns
    df[, c("variable", "value", "count", "proportion")]
  })

  do.call(rbind, rows)
}

#' Compute the sample skewness of a numeric vector
#'
#' Uses the Fisher-Pearson standardized third moment
#' (\eqn{g_1 = \frac{1}{n}\sum(x_i - \bar{x})^3 / s^3}, with `s` the sample
#' standard deviation). Values near `0` indicate a symmetric distribution;
#' positive values indicate a right tail, negative values a left tail.
#'
#' @param x Numeric vector. `NA` values are dropped before computing.
#' @return A single numeric skewness value, or `NA_real_` when fewer than 3
#'   complete observations remain or the sample has zero variance.
#' @export
skewness <- function(x) {
  assert_numeric(x, min.len = 1L)

  complete <- x[!is.na(x)]
  n <- length(complete)
  if (n < 3L) {
    return(NA_real_)
  }

  m <- mean(complete)
  s <- sd(complete)
  if (s == 0) {
    return(NA_real_)
  }

  (sum((complete - m)^3) / n) / (s^3)
}

#' Detect outliers in a numeric column using the IQR (Tukey fence) rule
#'
#' Flags values below `Q1 - 1.5 * IQR` or above `Q3 + 1.5 * IQR` as outliers,
#' the standard criterion referenced by the course guide for identifying
#' atypical values. Also reports the sample skewness of the column so that
#' asymmetry claims in the report can cite a concrete statistic.
#'
#' @param data A non-empty `data.frame`.
#' @param col Character scalar naming a numeric column present in `data`.
#' @return A named list with `variable`, `q1`, `q3`, `iqr`, `lower_bound`,
#'   `upper_bound`, `n_outliers`, `outlier_values` and `skewness`.
#' @export
iqr_outliers <- function(data, col) {
  assert_data_frame(data, min.rows = 1L)
  assert_string(col)
  assert_subset(col, choices = names(data))

  values <- data[[col]]
  if (!is.numeric(values)) {
    stop(sprintf("Column '%s' is not numeric.", col), call. = FALSE)
  }
  complete <- values[!is.na(values)]

  q1 <- unname(quantile(complete, probs = 0.25))
  q3 <- unname(quantile(complete, probs = 0.75))
  iqr_val <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr_val
  upper_bound <- q3 + 1.5 * iqr_val

  is_outlier <- complete < lower_bound | complete > upper_bound
  outlier_values <- sort(complete[is_outlier])

  list(
    variable = col,
    q1 = q1,
    q3 = q3,
    iqr = iqr_val,
    lower_bound = lower_bound,
    upper_bound = upper_bound,
    n_outliers = sum(is_outlier),
    outlier_values = outlier_values,
    skewness = skewness(complete)
  )
}
