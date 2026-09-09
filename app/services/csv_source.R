#' @title CSV data source
#'
#' @description
#' Concrete `DataSource` backed by a delimited text file. Serves as the
#' reference implementation of the `app/core/interfaces[DataSource]` contract.
#'
#' @details
#' \preformatted{
#' box::use(app/services/csv_source[CsvSource])
#' src <- CsvSource$new(path = "data/raw.csv")
#' df  <- src$connect()$read()
#' src$disconnect()
#' }

# styler: off
box::use(
  R6[R6Class],
  checkmate[assert_file_exists, assert_flag, assert_string],
  utils[read.csv],
  app/core/interfaces[DataSource],
)
# styler: on

#' @export
CsvSource <- R6Class(
  "CsvSource",
  inherit = DataSource,
  public = list(
    #' @description Create the CSV source.
    #' @param path Path to an existing, readable `.csv` file.
    #' @param header Whether the first row holds column names.
    #' @param sep Field separator.
    initialize = function(path, header = TRUE, sep = ",") {
      assert_file_exists(path, access = "r", extension = "csv")
      assert_flag(header)
      assert_string(sep, min.chars = 1L)
      super$initialize(name = basename(path))
      private$path <- path
      private$header <- header
      private$sep <- sep
    },

    #' @description No-op for a file source; kept for contract symmetry.
    #' @return `invisible(self)`.
    connect = function() {
      invisible(self)
    },

    #' @description Read the file into memory.
    #' @return A `data.frame` with the file contents.
    read = function() {
      read.csv(
        private$path,
        header = private$header,
        sep = private$sep,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    },

    #' @description No-op for a file source; kept for contract symmetry.
    #' @return `invisible(self)`.
    disconnect = function() {
      invisible(self)
    }
  ),
  private = list(
    path = NULL,
    header = NULL,
    sep = NULL
  )
)
