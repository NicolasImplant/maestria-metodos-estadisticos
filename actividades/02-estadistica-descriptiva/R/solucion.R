#' @title Solución — código de análisis
#'
#' Actividad 02 — Estadistica Descriptiva

# styler: off
box::use(
  ggplot2[
    ggplot, aes, geom_jitter, geom_smooth, geom_line, geom_point,
    geom_violin, geom_boxplot, geom_hline, theme_minimal, labs,
    scale_color_viridis_d, scale_fill_manual, facet_wrap, theme,
    element_text, ggsave, scale_color_manual, stat_summary,
  ],
  patchwork[wrap_plots],
  here[here],
  app/core/factory[data_source_factory],
  app/pipelines/preprocessing[clean_student_data, merge_student_data],
  app/pipelines/summary_stats[
    numeric_summary, categorical_summary, iqr_outliers,
  ],
)
# styler: on

#' Load data from SQLite or CSV
#'
#' Demonstrates the Dependency Inversion Principle (DIP) by accepting any
#' source kind.
#'
#' @param kind Character. 'csv' or 'sqlite'.
#' @return A list with df_mat and df_por.
#' @export
load_inputs <- function(kind = "csv") {
  if (kind == "sqlite") {
    db_path <- here(
      "actividades", "02-estadistica-descriptiva", "datos",
      "student_performance.db"
    )
    src_mat <- data_source_factory(
      "sqlite",
      db_path = db_path,
      table_name = "student_mat"
    )
    src_por <- data_source_factory(
      "sqlite",
      db_path = db_path,
      table_name = "student_por"
    )
  } else {
    csv_mat <- here(
      "actividades", "02-estadistica-descriptiva", "datos",
      "student-mat.csv"
    )
    csv_por <- here(
      "actividades", "02-estadistica-descriptiva", "datos",
      "student-por.csv"
    )
    src_mat <- data_source_factory("csv", path = csv_mat, sep = ";")
    src_por <- data_source_factory("csv", path = csv_por, sep = ";")
  }

  df_mat_raw <- src_mat$connect()$read()
  df_por_raw <- src_por$connect()$read()

  src_mat$disconnect()
  src_por$disconnect()

  list(
    mat = df_mat_raw,
    por = df_por_raw
  )
}

#' Run full descriptive pipeline
#'
#' @param source_kind Source database 'csv' or 'sqlite'.
#' @return List of clean data frames and summary tables.
#' @export
run_descriptive_pipeline <- function(source_kind = "csv") {
  raw_data <- load_inputs(source_kind)

  df_mat_clean <- clean_student_data(raw_data$mat)
  df_por_clean <- clean_student_data(raw_data$por)

  merged_df <- merge_student_data(df_mat_clean, df_por_clean)

  # Quantitative columns of interest
  cols_num <- c("age", "absences.mat", "absences.por", "G3.mat", "G3.por")
  sum_num <- numeric_summary(merged_df, cols_num)

  # Qualitative columns
  cols_cat <- c("sex", "address", "internet", "Walc.mat")
  merged_df_cat <- merged_df
  merged_df_cat$Walc.mat <- as.character(merged_df_cat$Walc.mat)
  sum_cat <- categorical_summary(merged_df_cat, cols_cat)

  # Formal outlier/asymmetry diagnostics (IQR rule) for the absences columns,
  # replacing the previous purely narrative "atípicos extremos" claim.
  outliers_mat <- iqr_outliers(merged_df, "absences.mat")
  outliers_por <- iqr_outliers(merged_df, "absences.por")

  list(
    mat_clean = df_mat_clean,
    por_clean = df_por_clean,
    merged = merged_df,
    numeric_summary = sum_num,
    categorical_summary = sum_cat,
    outliers_mat = outliers_mat,
    outliers_por = outliers_por
  )
}

#' Generate and export high-resolution PNG charts
#'
#' @param merged_df Merged student performance data frame (N=382).
#' @export
generate_and_export_charts <- function(merged_df) {
  # Create directory
  export_dir <- here(
    "actividades", "02-estadistica-descriptiva", "export_graficos"
  )
  dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)

  # 1. Correlación Cruzada (Math vs Portuguese)
  p1 <- ggplot(
    merged_df,
    aes(x = G3.por, y = G3.mat, color = as.factor(Walc.mat))
  ) +
    geom_jitter(width = 0.5, height = 0.5, alpha = 0.7, size = 2) +
    geom_smooth(
      method = "lm",
      se = FALSE,
      color = "#2c3e50",
      linetype = "dashed",
      size = 1
    ) +
    scale_color_viridis_d(name = "Alcohol Fin de Sem.") +
    theme_minimal() +
    labs(
      title = "Alineamiento Académico: Matemáticas vs. Portugués",
      subtitle = "Cohorte de estudiantes común (N = 382)",
      x = "Nota Final Portugués (G3.por)",
      y = "Nota Final Matemáticas (G3.mat)"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 10, color = "#7f8c8d")
    )

  ggsave(
    file.path(export_dir, "correlacion_cruzada.png"),
    plot = p1,
    width = 7,
    height = 5,
    dpi = 300
  )

  # 2. Gráfico de Interacción Género y Alcohol
  # Compute interaction averages for Math
  p2_mat <- ggplot(
    merged_df,
    aes(x = as.factor(Walc.mat), y = G3.mat, group = sex, color = sex)
  ) +
    stat_summary(fun = mean, geom = "line", size = 1.2) +
    stat_summary(fun = mean, geom = "point", size = 3) +
    scale_color_manual(values = c("F" = "#e74c3c", "M" = "#3498db")) +
    theme_minimal() +
    labs(
      title = "Matemáticas (G3.mat)",
      x = "Consumo Alcohol (1: Muy bajo, 5: Muy alto)",
      y = "Promedio Calificación"
    ) +
    theme(plot.title = element_text(face = "bold", size = 11))

  # Compute interaction averages for Portuguese
  p2_por <- ggplot(
    merged_df,
    aes(x = as.factor(Walc.por), y = G3.por, group = sex, color = sex)
  ) +
    stat_summary(fun = mean, geom = "line", size = 1.2) +
    stat_summary(fun = mean, geom = "point", size = 3) +
    scale_color_manual(values = c("F" = "#e74c3c", "M" = "#3498db")) +
    theme_minimal() +
    labs(
      title = "Portugués (G3.por)",
      x = "Consumo Alcohol (1: Muy bajo, 5: Muy alto)",
      y = "Promedio Calificación"
    ) +
    theme(plot.title = element_text(face = "bold", size = 11))

  p2 <- wrap_plots(p2_mat, p2_por, ncol = 2, guides = "collect") +
    labs(
      title = "Efecto de Interacción: Género, Alcohol y Calificaciones",
      subtitle = "Comparativa de medias según nivel de alcohol"
    )

  ggsave(
    file.path(export_dir, "interaccion_genero_alcohol.png"),
    plot = p2,
    width = 9,
    height = 5,
    dpi = 300
  )

  # 3. Gráfico de Brecha Geográfica (Urbano vs Rural)
  p3_mat <- ggplot(merged_df, aes(x = address, y = G3.mat, fill = address)) +
    geom_violin(alpha = 0.5, trim = FALSE) +
    geom_boxplot(width = 0.2, color = "black", outlier.shape = NA) +
    scale_fill_manual(values = c("R" = "#e67e22", "U" = "#2ecc71")) +
    theme_minimal() +
    labs(
      title = "Calificación Matemáticas",
      x = "Residencia (R: Rural, U: Urbano)",
      y = "Nota Final G3.mat"
    ) +
    theme(legend.position = "none", plot.title = element_text(face = "bold"))

  p3_por <- ggplot(merged_df, aes(x = address, y = G3.por, fill = address)) +
    geom_violin(alpha = 0.5, trim = FALSE) +
    geom_boxplot(width = 0.2, color = "black", outlier.shape = NA) +
    scale_fill_manual(values = c("R" = "#e67e22", "U" = "#2ecc71")) +
    theme_minimal() +
    labs(
      title = "Calificación Portugués",
      x = "Residencia (R: Rural, U: Urbano)",
      y = "Nota Final G3.por"
    ) +
    theme(legend.position = "none", plot.title = element_text(face = "bold"))

  p3 <- wrap_plots(p3_mat, p3_por, ncol = 2) +
    labs(
      title = "Brecha Geográfica de Rendimiento",
      subtitle = "Distribuciones híbridas según zona residencial"
    )

  ggsave(
    file.path(export_dir, "brecha_urbano_rural.png"),
    plot = p3,
    width = 9,
    height = 5,
    dpi = 300
  )

  # 4. Detección Formal de Atípicos en Inasistencias (Regla IQR)
  # Unlike p3, outliers are shown explicitly (no `outlier.shape = NA`), and
  # the upper Tukey fence is drawn so the "milla extra" claim is visual, not
  # just narrative.
  outliers_mat <- iqr_outliers(merged_df, "absences.mat")
  outliers_por <- iqr_outliers(merged_df, "absences.por")

  p4_mat <- ggplot(merged_df, aes(x = "Matemáticas", y = absences.mat)) +
    geom_boxplot(
      fill = "#e74c3c", alpha = 0.5,
      outlier.color = "#c0392b", outlier.size = 2
    ) +
    geom_hline(
      yintercept = outliers_mat$upper_bound,
      linetype = "dashed", color = "#2c3e50"
    ) +
    theme_minimal() +
    labs(
      title = sprintf("Matemáticas (%d atípicos)", outliers_mat$n_outliers),
      x = NULL,
      y = "Inasistencias"
    ) +
    theme(plot.title = element_text(face = "bold", size = 11))

  p4_por <- ggplot(merged_df, aes(x = "Portugués", y = absences.por)) +
    geom_boxplot(
      fill = "#2ecc71", alpha = 0.5,
      outlier.color = "#27ae60", outlier.size = 2
    ) +
    geom_hline(
      yintercept = outliers_por$upper_bound,
      linetype = "dashed", color = "#2c3e50"
    ) +
    theme_minimal() +
    labs(
      title = sprintf("Portugués (%d atípicos)", outliers_por$n_outliers),
      x = NULL,
      y = "Inasistencias"
    ) +
    theme(plot.title = element_text(face = "bold", size = 11))

  p4 <- wrap_plots(p4_mat, p4_por, ncol = 2) +
    labs(
      title = "Detección Formal de Atípicos en Inasistencias (Regla IQR)",
      subtitle = "Línea punteada = límite superior de Tukey (Q3 + 1.5·IQR)"
    )

  ggsave(
    file.path(export_dir, "atipicos_inasistencias.png"),
    plot = p4,
    width = 9,
    height = 5,
    dpi = 300
  )

  invisible(list(p1 = p1, p2 = p2, p3 = p3, p4 = p4))
}
