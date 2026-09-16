#' @title Data source factory
#'
#' @description
#' Factory Method that centralizes the creation of `DataSource` instances.
#' Callers ask for a source by kind and receive a ready-to-use object without
#' knowing the concrete class. Adding a new source type only requires
#' registering it here.
#'
#' @details
#' \preformatted{
#' box::use(app/core/factory[data_source_factory])
#' src <- data_source_factory("csv", path = "actividades/01/datos/raw.csv")
#' }

# styler: off
box::use(
  checkmate[assert_choice, assert_list],
)
# styler: on

# Registry of supported source kinds. Each entry is a zero-argument function
# that returns the constructor for that kind. The lazy `box::use()` inside
# avoids a load-time dependency cycle with the service modules.
.registry <- list(
  csv = function() {
    # styler: off
    box::use(app/services/csv_source[CsvSource])
    # styler: on
    CsvSource
  },
  sqlite = function() {
    # styler: off
    box::use(app/services/sqlite_source[SqliteSource])
    # styler: on
    SqliteSource
  }
)

#' Build a data source
#'
#' @param kind Source kind. One of `names(supported_source_kinds())`.
#' @param ... Named arguments forwarded to the concrete constructor.
#' @return An initialized object whose class inherits from `DataSource`.
#' @export
data_source_factory <- function(kind, ...) {
  assert_choice(kind, choices = names(.registry))
  args <- list(...)
  assert_list(args, names = "named", .var.name = "...")

  constructor <- .registry[[kind]]()
  do.call(constructor$new, args)
}

#' List the source kinds the factory can build
#'
#' @return A character vector of registered kinds.
#' @export
supported_source_kinds <- function() {
  names(.registry)
}
