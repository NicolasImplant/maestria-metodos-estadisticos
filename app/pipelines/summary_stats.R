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
#' box::use(app/pipelines/summary_stats[numeric_summary])
#' numeric_summary(mtcars, c("mpg", "hp"))
#' }

# styler: off
box::use(
  checkmate[assert_data_frame, assert_subset],
  stats[median, sd],
)
# styler: on

#' Describe the numeric columns of a data frame
#'
#' @param data A non-empty `data.frame`.
#' @param cols Character vector of column names to describe. All must be
#'   numeric columns present in `data`.
#' @return A `data.frame` with one row per requested column and the columns
#'   `variable`, `n`, `n_missing`, `mean`, `sd`, `min`, `median`, `max`.
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
    data.frame(
      variable = col,
      n = length(values),
      n_missing = sum(is.na(values)),
      mean = mean(complete),
      sd = sd(complete),
      min = min(complete),
      median = median(complete),
      max = max(complete),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}
