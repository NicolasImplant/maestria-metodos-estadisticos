#' @title Abstract domain contracts
#'
#' @description
#' `R6` base classes that define contracts (interfaces) for the rest of the
#' application. They enforce the Dependency Inversion Principle: high-level
#' modules depend on these abstractions, never on concrete implementations.
#' Every abstract method calls `stop()` so that subclasses are forced to
#' implement it (fail-fast).
#'
#' @details
#' Intended usage from other modules:
#' \preformatted{
#' box::use(app/core/interfaces[DataSource])
#' }

# styler: off
box::use(
  R6[R6Class],
  checkmate[assert_string],
)
# styler: on

# Raised by every not-yet-implemented method of an abstract class.
abstract_method <- function(name) {
  stop(
    sprintf("Abstract method '%s()' must be implemented by a subclass.", name),
    call. = FALSE
  )
}

#' Abstract data source
#'
#' Minimal contract for any data source (file, database, API). Concrete
#' implementations live under `app/services/`.
#'
#' @export
DataSource <- R6Class(
  "DataSource",
  public = list(
    #' @description Create the data source.
    #' @param name Human-readable identifier. Non-empty string.
    initialize = function(name) {
      assert_string(name, min.chars = 1L)
      private$name <- name
    },

    #' @description Open the underlying connection or resource.
    #' @return `invisible(self)` to allow method chaining.
    connect = function() abstract_method("connect"),

    #' @description Read the whole source.
    #' @return A `data.frame` with the source records.
    read = function() abstract_method("read"),

    #' @description Release the underlying connection or resource.
    #' @return `invisible(self)`.
    disconnect = function() abstract_method("disconnect")
  ),
  private = list(
    name = NULL
  )
)
