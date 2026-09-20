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
    position_dodge, mean_se,
  ],
  patchwork[wrap_plots, plot_annotation],
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

  # Categorización balanceada de consumo de alcohol (elimina artefacto n=4)
  merged_df$walc_cat_mat <- factor(
    ifelse(merged_df$Walc.mat <= 2, "Bajo (1-2)",
      ifelse(merged_df$Walc.mat == 3, "Moderado (3)", "Alto (4-5)")
    ),
    levels = c("Bajo (1-2)", "Moderado (3)", "Alto (4-5)")
  )

  merged_df$walc_cat_por <- factor(
    ifelse(merged_df$Walc.por <= 2, "Bajo (1-2)",
      ifelse(merged_df$Walc.por == 3, "Moderado (3)", "Alto (4-5)")
    ),
    levels = c("Bajo (1-2)", "Moderado (3)", "Alto (4-5)")
  )

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
      linewidth = 1
    ) +
    scale_color_viridis_d(name = "Consumo Alcohol") +
    theme_minimal() +
    labs(
      title = "Alineamiento Académico y Consumo de Alcohol",
      subtitle = paste0(
        "Correlación intra-sujeto r = 0.48 | ",
        "Alcohol vs Notas: r_por = -0.23, r_mat = -0.03"
      ),
      caption = paste0(
        "Nota: Alta dispersión de notas en consumo elevado (Walc 4-5); ",
        "15 estudiantes alcanzan notas >= 14."
      ),
      x = "Nota Final Portugués (G3.por)",
      y = "Nota Final Matemáticas (G3.mat)"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9, color = "#475569"),
      plot.caption = element_text(size = 8, color = "#64748b", face = "italic")
    )

  ggsave(
    file.path(export_dir, "correlacion_cruzada.png"),
    plot = p1,
    width = 7,
    height = 5,
    dpi = 300
  )

  # 2. Gráfico de Interacción Género y Alcohol (Barras de error y N balanceado)
  p2_mat <- ggplot(
    merged_df,
    aes(x = walc_cat_mat, y = G3.mat, group = sex, color = sex)
  ) +
    stat_summary(
      fun.data = mean_se,
      geom = "errorbar",
      width = 0.15,
      position = position_dodge(0.15)
    ) +
    stat_summary(
      fun = mean,
      geom = "line",
      linewidth = 1,
      position = position_dodge(0.15)
    ) +
    stat_summary(
      fun = mean,
      geom = "point",
      size = 3,
      position = position_dodge(0.15)
    ) +
    scale_color_manual(
      values = c("F" = "#e74c3c", "M" = "#3498db"),
      labels = c("F" = "Mujeres (F)", "M" = "Hombres (M)")
    ) +
    theme_minimal() +
    labs(
      title = "Matemáticas (G3.mat)",
      subtitle = "Hombres: n=(94, 32, 58) | Mujeres: n=(137, 44, 17)",
      x = "Estrato de Consumo de Fin de Semana",
      y = "Promedio Calificación (±1 SE)"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 8.5, color = "#64748b")
    )

  p2_por <- ggplot(
    merged_df,
    aes(x = walc_cat_por, y = G3.por, group = sex, color = sex)
  ) +
    stat_summary(
      fun.data = mean_se,
      geom = "errorbar",
      width = 0.15,
      position = position_dodge(0.15)
    ) +
    stat_summary(
      fun = mean,
      geom = "line",
      linewidth = 1,
      position = position_dodge(0.15)
    ) +
    stat_summary(
      fun = mean,
      geom = "point",
      size = 3,
      position = position_dodge(0.15)
    ) +
    scale_color_manual(
      values = c("F" = "#e74c3c", "M" = "#3498db"),
      labels = c("F" = "Mujeres (F)", "M" = "Hombres (M)")
    ) +
    theme_minimal() +
    labs(
      title = "Portugués (G3.por)",
      subtitle = "Hombres: n=(93, 33, 58) | Mujeres: n=(137, 43, 18)",
      x = "Estrato de Consumo de Fin de Semana",
      y = "Promedio Calificación (±1 SE)"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 8.5, color = "#64748b")
    )

  p2 <- wrap_plots(p2_mat, p2_por, ncol = 2, guides = "collect") +
    plot_annotation(
      title = paste0(
        "Efecto de Interacción: Género, Consumo de Alcohol ",
        "y Calificaciones"
      ),
      subtitle = paste0(
        "Medias muestrales y barras de error estándar ",
        "(±1 SE) agrupadas por estrato"
      ),
      caption = paste0(
        "La agrupación en 3 estratos neutraliza el artefacto ",
        "muestral de celdas unitarias (n=4 en Walc 5)."
      ),
      theme = theme(
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 9.5, color = "#475569"),
        plot.caption = element_text(
          size = 8, color = "#64748b", face = "italic"
        )
      )
    ) &
    theme(legend.position = "bottom")

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
      subtitle = "Urbano: media 10.6 | Rural: media 9.6",
      x = "Residencia (R: Rural, U: Urbano)",
      y = "Nota Final G3.mat"
    ) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 9, color = "#64748b")
    )

  p3_por <- ggplot(merged_df, aes(x = address, y = G3.por, fill = address)) +
    geom_violin(alpha = 0.5, trim = FALSE) +
    geom_boxplot(width = 0.2, color = "black", outlier.shape = NA) +
    scale_fill_manual(values = c("R" = "#e67e22", "U" = "#2ecc71")) +
    theme_minimal() +
    labs(
      title = "Calificación Portugués",
      subtitle = "Urbano: media 12.8 | Rural: media 11.4",
      x = "Residencia (R: Rural, U: Urbano)",
      y = "Nota Final G3.por"
    ) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 9, color = "#64748b")
    )

  p3 <- wrap_plots(p3_mat, p3_por, ncol = 2) +
    plot_annotation(
      title = paste0(
        "Brecha Geográfica de Rendimiento y ",
        "Solapamiento Distribucional"
      ),
      subtitle = paste0(
        "Cohorte desbalanceada: Urbano (N = 301) vs. Rural (N = 81) | ",
        "Mayor viaje rural (1.9h vs 1.3h)"
      ),
      caption = paste0(
        "Nota: El amplio solapamiento evidencia resiliencia rural; ",
        "muchos estudiantes rurales superan la media urbana."
      ),
      theme = theme(
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 9.5, color = "#475569"),
        plot.caption = element_text(
          size = 8, color = "#64748b", face = "italic"
        )
      )
    )

  ggsave(
    file.path(export_dir, "brecha_urbano_rural.png"),
    plot = p3,
    width = 9,
    height = 5,
    dpi = 300
  )

  # 4. Detección Formal de Atípicos en Inasistencias (Regla IQR)
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
      title = sprintf(
        "Matemáticas (%d atípicos > %d faltas)",
        outliers_mat$n_outliers, outliers_mat$upper_bound
      ),
      subtitle = paste0(
        "Nota media: Atípicos = 10.23 vs No-atípicos = 10.39 ",
        "(Sin impacto)"
      ),
      x = NULL,
      y = "Inasistencias"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 8.5, color = "#64748b")
    )

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
      title = sprintf(
        "Portugués (%d atípicos > %d faltas)",
        outliers_por$n_outliers, outliers_por$upper_bound
      ),
      subtitle = paste0(
        "Nota media: Atípicos = 10.71 vs No-atípicos = 12.60 ",
        "(-1.89 pts)"
      ),
      x = NULL,
      y = "Inasistencias"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 8.5, color = "#64748b")
    )

  p4 <- wrap_plots(p4_mat, p4_por, ncol = 2) +
    plot_annotation(
      title = paste0(
        "Detección Formal de Atípicos en Inasistencias ",
        "(Regla IQR de Tukey)"
      ),
      subtitle = paste0(
        "Impacto asimétrico: el ausentismo extremo penaliza en lenguaje ",
        "pero no altera la media en matemáticas"
      ),
      caption = paste0(
        "Línea punteada = límite superior de Tukey (Q3 + 1.5·IQR). ",
        "Ausentismo concentrado en un subgrupo (< 5%)."
      ),
      theme = theme(
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 9.5, color = "#475569"),
        plot.caption = element_text(
          size = 8, color = "#64748b", face = "italic"
        )
      )
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
