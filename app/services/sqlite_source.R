#' @title SQLite data source
#'
#' @description
#' Concrete `DataSource` backed by a table in a local SQLite database file.
#' Fully conforms to the `app/core/interfaces[DataSource]` contract.
#'
#' @details
#' \preformatted{
#' box::use(app/services/sqlite_source[SqliteSource])
#' src <- SqliteSource$new(db_path = "data/db.db", table_name = "students")
#' df  <- src$connect()$read()
#' src$disconnect()
#' }

# styler: off
box::use(
  R6[R6Class],
  checkmate[assert_file_exists, assert_string],
  DBI[dbConnect, dbDisconnect, dbReadTable, dbIsValid],
  RSQLite[SQLite],
  app/core/interfaces[DataSource],
)
# styler: on

#' @export
SqliteSource <- R6Class(
  "SqliteSource",
  inherit = DataSource,
  public = list(
    #' @description Create the SQLite source.
    #' @param db_path Path to an existing, readable SQLite database file.
    #' @param table_name Name of the target table. Non-empty string.
    initialize = function(db_path, table_name) {
      assert_file_exists(db_path, access = "r")
      assert_string(table_name, min.chars = 1L)
      super$initialize(name = sprintf("%s::%s", basename(db_path), table_name))
      private$db_path <- db_path
      private$table_name <- table_name
    },

    #' @description Establish a connection to the SQLite database.
    #' @return `invisible(self)`.
    connect = function() {
      if (is.null(private$con) || !dbIsValid(private$con)) {
        private$con <- dbConnect(SQLite(), private$db_path)
      }
      invisible(self)
    },

    #' @description Read the entire table from SQLite.
    #' @return A `data.frame` with the table rows.
    read = function() {
      if (is.null(private$con) || !dbIsValid(private$con)) {
        stop("Database is not connected. Call $connect() first.", call. = FALSE)
      }
      dbReadTable(private$con, private$table_name)
    },

    #' @description Terminate the active SQLite connection.
    #' @return `invisible(self)`.
    disconnect = function() {
      if (!is.null(private$con) && dbIsValid(private$con)) {
        dbDisconnect(private$con)
      }
      private$con <- NULL
      invisible(self)
    }
  ),
  private = list(
    db_path = NULL,
    table_name = NULL,
    con = NULL
  )
)
