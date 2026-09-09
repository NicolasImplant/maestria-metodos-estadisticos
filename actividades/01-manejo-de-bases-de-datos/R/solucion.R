#' @title Solución — código de análisis
#'
#' Actividad 01 — Manejo de bases de datos en R.
#'
#' Coloca aquí las funciones puras del análisis y llámalas desde
#' `informe.qmd`. Mantén la lógica de infraestructura (conexiones, lectura de
#' archivos) detrás de `app/core/factory` y `app/services/`.

# styler: off
box::use(
  checkmate[assert_data_frame],
  app/pipelines/summary_stats[numeric_summary],
)
# styler: on

#' Ejemplo: resumen numérico de una base cargada
#'
#' @param data `data.frame` ya importado.
#' @param cols Columnas numéricas a describir.
#' @return `data.frame` con el resumen descriptivo.
#' @export
describe_base <- function(data, cols) {
  assert_data_frame(data, min.rows = 1L)
  numeric_summary(data, cols)
}
