#' @title Solución — código de análisis
#'
#' Actividad 02 — Estadistica Descriptiva

# styler: off
box::use(
  ggplot2[
    ggplot, aes, geom_jitter, geom_smooth, geom_violin, geom_boxplot,
    geom_hline, theme_minimal, labs, scale_color_viridis_d, scale_fill_manual,
    theme, element_text, ggsave, scale_color_manual, stat_summary,
    position_dodge, mean_se,
  ],
  patchwork[wrap_plots, plot_annotation],
  here[here],
  app/core/factory[data_source_factory],
  app/pipelines/preprocessing[clean_student_data, merge_student_data],
  app/pipelines/report_facts[report_facts, walc_stratum, fmt_num],
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
    outliers_por = outliers_por,
    facts = report_facts(merged_df)
  )
}

subject_names <- c(mat = "Matemáticas", por = "Portugués")

# Shared look for the caption block of the combined figures.
annotation_theme <- function() {
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9.5, color = "#475569"),
    plot.caption = element_text(size = 8, color = "#64748b", face = "italic")
  )
}

# "94 / 32 / 58"-style counts for one sex across the three alcohol strata.
strata_counts <- function(strata, subject, sex) {
  rows <- strata[strata$subject == subject & strata$sex == sex, ]
  paste(rows$n, collapse = " / ")
}

# 1. Cross-subject scatter (Portuguese vs Math), coloured by weekend alcohol.
chart_alignment <- function(df, facts) {
  corr <- facts$corr
  high <- facts$high_alcohol$mat

  ggplot(df, aes(x = G3.por, y = G3.mat, color = as.factor(Walc.mat))) +
    geom_jitter(width = 0.5, height = 0.5, alpha = 0.7, size = 2) +
    geom_smooth(
      method = "lm", se = FALSE, color = "#2c3e50",
      linetype = "dashed", linewidth = 1
    ) +
    scale_color_viridis_d(name = "Alcohol en fin\nde semana (1-5)") +
    theme_minimal() +
    labs(
      title = "Notas de Portugués y Matemáticas, según consumo de alcohol",
      subtitle = paste0(
        "Entre materias: r = ", fmt_num(corr$subjects$pearson, 2), "\n",
        "Alcohol y nota: Portugués r = ", fmt_num(corr$alcohol_por$pearson, 2),
        " | Matemáticas r = ", fmt_num(corr$alcohol_mat$pearson, 2)
      ),
      caption = sprintf(
        paste0(
          "Con consumo alto (4-5) en Matemáticas, %d de %d estudiantes ",
          "tienen nota de 14 o más.\nLínea punteada: tendencia lineal."
        ),
        high$n_good, high$n_high
      ),
      x = "Nota final de Portugués (G3.por)",
      y = "Nota final de Matemáticas (G3.mat)"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 9, color = "#475569"),
      plot.caption = element_text(size = 8, color = "#64748b", face = "italic")
    )
}

# 2. Mean grade by alcohol stratum and sex, with +/- 1 standard error.
interaction_panel <- function(df, subject, facts) {
  df$stratum <- walc_stratum(df[[paste0("Walc.", subject)]])
  df$grade <- df[[paste0("G3.", subject)]]
  dodge <- position_dodge(0.15)

  ggplot(df, aes(x = stratum, y = grade, group = sex, color = sex)) +
    stat_summary(
      fun.data = mean_se, geom = "errorbar", width = 0.15, position = dodge
    ) +
    stat_summary(fun = mean, geom = "line", linewidth = 1, position = dodge) +
    stat_summary(fun = mean, geom = "point", size = 3, position = dodge) +
    scale_color_manual(
      values = c("F" = "#e74c3c", "M" = "#3498db"),
      labels = c("F" = "Mujeres", "M" = "Hombres"),
      name = NULL
    ) +
    theme_minimal() +
    labs(
      title = subject_names[[subject]],
      subtitle = sprintf(
        "Hombres n = %s | Mujeres n = %s",
        strata_counts(facts$strata, subject, "M"),
        strata_counts(facts$strata, subject, "F")
      ),
      x = "Consumo de alcohol en el fin de semana",
      y = "Nota promedio (± 1 SE)"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 8.5, color = "#64748b")
    )
}

chart_interaction <- function(df, facts) {
  five <- facts$walc5_by_sex

  wrap_plots(
    interaction_panel(df, "mat", facts),
    interaction_panel(df, "por", facts),
    ncol = 2, guides = "collect"
  ) +
    plot_annotation(
      title = "Nota promedio según consumo de alcohol y sexo",
      subtitle = paste0(
        "Puntos = promedio del grupo; barras = ± 1 error estándar (SE). ",
        "n por nivel: bajo / moderado / alto."
      ),
      caption = sprintf(
        paste0(
          "Se agruparon 3 niveles de alcohol porque en el nivel 5 solo ",
          "había %d mujeres y %d hombres."
        ),
        five[["F"]], five[["M"]]
      ),
      theme = annotation_theme()
    ) &
    theme(legend.position = "bottom")
}

# 3. Violin + boxplot of grades by residence zone.
geography_panel <- function(df, subject, gap) {
  df$grade <- df[[paste0("G3.", subject)]]

  ggplot(df, aes(x = address, y = grade, fill = address)) +
    geom_violin(alpha = 0.5, trim = FALSE) +
    geom_boxplot(width = 0.2, color = "black", outlier.shape = NA) +
    scale_fill_manual(values = c("R" = "#e67e22", "U" = "#2ecc71")) +
    theme_minimal() +
    labs(
      title = subject_names[[subject]],
      subtitle = paste0(
        "Media urbana ", fmt_num(gap$mean_urban), " | rural ",
        fmt_num(gap$mean_rural)
      ),
      x = "Zona de residencia (R = rural, U = urbana)",
      y = paste0("Nota final (G3.", subject, ")")
    ) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 9, color = "#64748b")
    )
}

chart_geography <- function(df, facts) {
  gap <- facts$address
  gap_mat <- gap[gap$subject == "mat", ]
  gap_por <- gap[gap$subject == "por", ]
  travel <- facts$long_travel * 100

  wrap_plots(
    geography_panel(df, "mat", gap_mat),
    geography_panel(df, "por", gap_por),
    ncol = 2
  ) +
    plot_annotation(
      title = "Notas según zona de residencia",
      subtitle = sprintf(
        paste0(
          "Urbano N = %d | Rural N = %d | Viajan 30 min o más: ",
          "%s%% urbano vs %s%% rural"
        ),
        gap_mat$n_urban, gap_mat$n_rural,
        fmt_num(travel[["urban"]], 0), fmt_num(travel[["rural"]], 0)
      ),
      caption = sprintf(
        paste0(
          "Las distribuciones se solapan: %d de %d estudiantes rurales ",
          "superan la media urbana en Matemáticas y %d en Portugués."
        ),
        gap_mat$rural_above_urban, gap_mat$n_rural, gap_por$rural_above_urban
      ),
      theme = annotation_theme()
    )
}

# 4. Boxplot of absences with the upper Tukey fence drawn as a dashed line.
absence_panel <- function(df, subject, profile, fill) {
  df$absences <- df[[paste0("absences.", subject)]]
  df$label <- subject_names[[subject]]

  ggplot(df, aes(x = label, y = absences)) +
    geom_boxplot(
      fill = fill, alpha = 0.5, outlier.color = "#c0392b", outlier.size = 2
    ) +
    geom_hline(
      yintercept = profile$upper, linetype = "dashed", color = "#2c3e50"
    ) +
    theme_minimal() +
    labs(
      title = sprintf(
        "%s: %d atípicos (más de %d faltas)",
        subject_names[[subject]], profile$n_out, profile$upper
      ),
      subtitle = sprintf(
        "Nota media: atípicos %s | resto %s (%s sin las notas 0)",
        fmt_num(profile$mean_out), fmt_num(profile$mean_rest),
        fmt_num(profile$mean_rest_nonzero)
      ),
      x = NULL,
      y = "Inasistencias"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 8.5, color = "#64748b")
    )
}

chart_absences <- function(df, facts) {
  prof <- facts$absences
  prof_mat <- prof[prof$subject == "mat", ]
  prof_por <- prof[prof$subject == "por", ]

  wrap_plots(
    absence_panel(df, "mat", prof_mat, "#e74c3c"),
    absence_panel(df, "por", prof_por, "#2ecc71"),
    ncol = 2
  ) +
    plot_annotation(
      title = "Inasistencias muy alejadas del resto (regla de Tukey)",
      subtitle = sprintf(
        "Menos del %d%% del alumnado: %s%% en Matemáticas y %s%% en Portugués",
        ceiling(max(prof$pct_out) * 100),
        fmt_num(prof_mat$pct_out * 100), fmt_num(prof_por$pct_out * 100)
      ),
      caption = sprintf(
        paste0(
          "Línea punteada = límite de Tukey (Q3 + 1.5·IQR). En Matemáticas, ",
          "%d estudiantes tienen nota 0 y ninguna falta."
        ),
        prof_mat$n_zero_no_absences
      ),
      theme = annotation_theme()
    )
}

#' Generate and export high-resolution PNG charts
#'
#' Every figure quoted in a title, subtitle or caption is computed from the
#' data through [report_facts()], so the charts cannot drift from the report.
#'
#' @param merged_df Merged student performance data frame (N=382).
#' @return Invisibly, a list with the four ggplot objects `p1` to `p4`.
#' @export
generate_and_export_charts <- function(merged_df) {
  export_dir <- here(
    "actividades", "02-estadistica-descriptiva", "export_graficos"
  )
  dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)

  facts <- report_facts(merged_df)
  plots <- list(
    p1 = chart_alignment(merged_df, facts),
    p2 = chart_interaction(merged_df, facts),
    p3 = chart_geography(merged_df, facts),
    p4 = chart_absences(merged_df, facts)
  )

  exports <- list(
    p1 = c("correlacion_cruzada.png", 7),
    p2 = c("interaccion_genero_alcohol.png", 9),
    p3 = c("brecha_urbano_rural.png", 9),
    p4 = c("atipicos_inasistencias.png", 9)
  )
  for (name in names(exports)) {
    ggsave(
      file.path(export_dir, exports[[name]][[1L]]),
      plot = plots[[name]],
      width = as.numeric(exports[[name]][[2L]]),
      height = 5,
      dpi = 300
    )
  }

  invisible(plots)
}
