#' @title Solución — Análisis Inferencial con Intervalos de Confianza
#'
#' Actividad 03 — Introducción a la inferencia estadística: IC y Bootstrap

# styler: off
box::use(
  ggplot2[
    aes, annotate, coord_flip, element_text, geom_density, geom_errorbar,
    geom_histogram, geom_hline, geom_point, geom_vline, ggplot, ggsave, labs,
    position_dodge, scale_color_manual, scale_fill_manual, scale_x_continuous,
    theme, theme_minimal,
  ],
  here[here],
  patchwork[plot_annotation, wrap_plots],
  app/core/factory[data_source_factory],
  app/pipelines/confidence_intervals[ci_proportion],
  app/pipelines/inferential_facts[inferential_facts],
  app/pipelines/preprocessing[clean_student_data, merge_student_data],
  app/pipelines/report_facts[fmt_num, walc_stratum],
  app/pipelines/summary_stats[
    categorical_summary, iqr_outliers, numeric_summary,
  ],
)
# styler: on

resolve_data_dir <- function() {
  local_dir <- here("actividades", "03-intervalos-confianza", "datos")
  if (file.exists(file.path(local_dir, "student_performance.db"))) {
    return(local_dir)
  }
  here("actividades", "02-estadistica-descriptiva", "datos")
}

#' Load raw datasets via SQLite or CSV DataSource
#'
#' @param kind Character scalar: `"sqlite"` or `"csv"`.
#' @return A named list with `mat` and `por` raw data frames.
#' @export
load_inputs <- function(kind = "sqlite") {
  data_dir <- resolve_data_dir()
  if (kind == "sqlite") {
    db_path <- file.path(data_dir, "student_performance.db")
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
    csv_mat <- file.path(data_dir, "student-mat.csv")
    csv_por <- file.path(data_dir, "student-por.csv")
    src_mat <- data_source_factory("csv", path = csv_mat, sep = ";")
    src_por <- data_source_factory("csv", path = csv_por, sep = ";")
  }

  df_mat_raw <- src_mat$connect()$read()
  df_por_raw <- src_por$connect()$read()
  src_mat$disconnect()
  src_por$disconnect()

  list(mat = df_mat_raw, por = df_por_raw)
}

#' Execute the complete Unit 3 inferential pipeline
#'
#' @param source_kind Character scalar: `"sqlite"` or `"csv"`.
#' @param n_boot Number of bootstrap replicates. Default is `5000L`.
#' @param seed Random seed. Default is `2026L`.
#' @return A named list with `merged`, `numeric_summary`, `categorical_summary`,
#'   `outliers_mat`, `outliers_por`, and `facts`.
#' @export
run_inferential_pipeline <- function(
  source_kind = "sqlite",
  n_boot = 5000L,
  seed = 2026L
) {
  raw_data <- load_inputs(source_kind)
  df_mat_clean <- clean_student_data(raw_data$mat)
  df_por_clean <- clean_student_data(raw_data$por)
  merged_df <- merge_student_data(df_mat_clean, df_por_clean)

  cols_num <- c(
    "age", "absences.mat", "absences.por",
    "G3.mat", "G3.por", "Walc.mat", "Walc.por"
  )
  sum_num <- numeric_summary(merged_df, cols_num)

  merged_cat <- merged_df
  merged_cat$Walc.mat <- as.character(merged_cat$Walc.mat)
  sum_cat <- categorical_summary(
    merged_cat,
    c("sex", "address", "internet", "Walc.mat")
  )

  list(
    merged = merged_df,
    numeric_summary = sum_num,
    categorical_summary = sum_cat,
    outliers_mat = iqr_outliers(merged_df, "absences.mat"),
    outliers_por = iqr_outliers(merged_df, "absences.por"),
    facts = inferential_facts(merged_df, n_boot = n_boot, seed = seed)
  )
}

annotation_theme <- function() {
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9.5, color = "#475569"),
    plot.caption = element_text(size = 8, color = "#64748b", face = "italic")
  )
}

boot_density_panel <- function(dist_obj, title_lbl, x_lbl, fill_col) {
  t_ci <- dist_obj$t_ci
  b_ci <- dist_obj$boot_ci
  df_rep <- data.frame(mean_star = b_ci$replicates)

  ggplot(df_rep, aes(x = mean_star)) +
    geom_histogram(
      aes(y = ..density..),
      bins = 38,
      fill = fill_col,
      alpha = 0.35,
      color = "white"
    ) +
    geom_density(color = "#0f172a", linewidth = 0.9) +
    geom_vline(
      xintercept = t_ci$mean,
      color = "#0f172a",
      linewidth = 1
    ) +
    geom_vline(
      xintercept = c(t_ci$ci_low, t_ci$ci_high),
      color = "#0284c7",
      linetype = "dashed",
      linewidth = 0.95
    ) +
    geom_vline(
      xintercept = c(b_ci$ci_low, b_ci$ci_high),
      color = "#e11d48",
      linetype = "dotted",
      linewidth = 1.05
    ) +
    theme_minimal() +
    labs(
      title = title_lbl,
      subtitle = sprintf(
        "Media = %s | t-Student [%s, %s] | Bootstrap [%s, %s]",
        fmt_num(t_ci$mean, 2),
        fmt_num(t_ci$ci_low, 2), fmt_num(t_ci$ci_high, 2),
        fmt_num(b_ci$ci_low, 2), fmt_num(b_ci$ci_high, 2)
      ),
      x = x_lbl,
      y = "Densidad Bootstrap"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 10.5),
      plot.subtitle = element_text(size = 8.5, color = "#475569")
    )
}

# 1. Parametric t-Student vs Bootstrap 95% CIs for G3 grades
chart_grades_ci <- function(facts) {
  p_mat <- boot_density_panel(
    facts$boot_distributions$G3.mat,
    "Matemáticas (G3.mat)",
    "Media muestral Bootstrap (escala 0 a 20)",
    "#38bdf8"
  )
  p_por <- boot_density_panel(
    facts$boot_distributions$G3.por,
    "Lengua Portuguesa (G3.por)",
    "Media muestral Bootstrap (escala 0 a 20)",
    "#10b981"
  )

  wrap_plots(p_mat, p_por, ncol = 2) +
    plot_annotation(
      title = "Intervalos de confianza del 95% para la nota final media (G3)",
      subtitle = paste0(
        "5,000 réplicas Bootstrap (N = 382). Línea azul discontinua: ",
        "IC t-Student; línea roja punteada: IC Bootstrap Percentil."
      ),
      caption = sprintf(
        paste0(
          "Diferencia pareada intra-sujeto (Portugués − Matemáticas): ",
          "+%s puntos (IC 95%% t: [%s, %s] | Bootstrap: [%s, %s])."
        ),
        fmt_num(facts$paired_grade$mean_diff, 2),
        fmt_num(facts$paired_grade$t_low, 2),
        fmt_num(facts$paired_grade$t_high, 2),
        fmt_num(facts$paired_grade$boot_low, 2),
        fmt_num(facts$paired_grade$boot_high, 2)
      ),
      theme = annotation_theme()
    )
}

# 2. Skewed absences: Raw distribution vs Bootstrap CI of the mean
chart_absences_ci <- function(df, facts) {
  p_raw <- ggplot(df, aes(x = absences.mat)) +
    geom_histogram(bins = 30, fill = "#f59e0b", alpha = 0.75, color = "white") +
    geom_vline(
      xintercept = mean(df$absences.mat),
      color = "#e11d48",
      linewidth = 1
    ) +
    geom_vline(
      xintercept = stats::median(df$absences.mat),
      color = "#0284c7",
      linetype = "dashed",
      linewidth = 1
    ) +
    theme_minimal() +
    labs(
      title = "A. Datos originales de faltas en Matemáticas",
      subtitle = sprintf(
        paste0(
          "Fuerte asimetría derecha (g1 = %s): ",
          "Mediana = 3 (azul) vs Media = %s (roja)"
        ),
        fmt_num(
          facts$univariate_table$skewness[
            facts$univariate_table$variable == "absences.mat"
          ],
          2
        ),
        fmt_num(mean(df$absences.mat), 2)
      ),
      x = "Inasistencias registradas (absences.mat)",
      y = "Frecuencia de estudiantes"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 10.5),
      plot.subtitle = element_text(size = 8.5, color = "#475569")
    )

  p_boot <- boot_density_panel(
    facts$boot_distributions$absences.mat,
    "B. Distribución Bootstrap de la media (B = 5,000)",
    "Promedio de inasistencias en cada remuestreo",
    "#f59e0b"
  )

  wrap_plots(p_raw, p_boot, ncol = 2) +
    plot_annotation(
      title = paste0(
        "Por qué usamos Bootstrap en variables ",
        "asimétricas (Inasistencias)"
      ),
      subtitle = paste0(
        "Aunque los datos individuales tienen cola larga (hasta 75 faltas), ",
        "el remuestreo estima la incertidumbre real del promedio."
      ),
      caption = sprintf(
        paste0(
          "En Portugués (absences.por): Media = %s | IC 95%% t-Student ",
          "[%s, %s] | IC 95%% Bootstrap [%s, %s]."
        ),
        fmt_num(facts$boot_distributions$absences.por$t_ci$mean, 2),
        fmt_num(facts$boot_distributions$absences.por$t_ci$ci_low, 2),
        fmt_num(facts$boot_distributions$absences.por$t_ci$ci_high, 2),
        fmt_num(facts$boot_distributions$absences.por$boot_ci$ci_low, 2),
        fmt_num(facts$boot_distributions$absences.por$boot_ci$ci_high, 2)
      ),
      theme = annotation_theme()
    )
}

# 3. Alcohol group contrasts (High 4-5 vs Low 1-2): Welch vs Bootstrap 95% CIs
chart_alcohol_contrasts <- function(facts) {
  wc <- facts$walc_contrasts
  df_g <- data.frame(
    Asignatura = rep(c("Matemáticas", "Portugués"), each = 2),
    Metodo = rep(c("Paramétrico (Welch)", "No paramétrico (Bootstrap)"), 2),
    diff = rep(wc$grade_diff, each = 2),
    low = c(
      wc$grade_welch_low[1], wc$grade_boot_low[1],
      wc$grade_welch_low[2], wc$grade_boot_low[2]
    ),
    high = c(
      wc$grade_welch_high[1], wc$grade_boot_high[1],
      wc$grade_welch_high[2], wc$grade_boot_high[2]
    )
  )

  df_a <- data.frame(
    Asignatura = rep(c("Matemáticas", "Portugués"), each = 2),
    Metodo = rep(c("Paramétrico (Welch)", "No paramétrico (Bootstrap)"), 2),
    diff = rep(wc$abs_diff, each = 2),
    low = c(
      wc$abs_welch_low[1], wc$abs_boot_low[1],
      wc$abs_welch_low[2], wc$abs_boot_low[2]
    ),
    high = c(
      wc$abs_welch_high[1], wc$abs_boot_high[1],
      wc$abs_welch_high[2], wc$abs_boot_high[2]
    )
  )

  dodge <- position_dodge(width = 0.45)
  pal <- c(
    "Paramétrico (Welch)" = "#0284c7",
    "No paramétrico (Bootstrap)" = "#e11d48"
  )

  p1 <- ggplot(df_g, aes(x = Asignatura, y = diff, color = Metodo)) +
    geom_hline(
      yintercept = 0, linetype = "dashed", color = "#64748b", linewidth = 0.9
    ) +
    geom_errorbar(
      aes(ymin = low, ymax = high),
      width = 0.2, linewidth = 1, position = dodge
    ) +
    geom_point(size = 3.2, position = dodge) +
    coord_flip() +
    scale_color_manual(values = pal, name = "Método de IC 95%") +
    theme_minimal() +
    labs(
      title = "A. Diferencia en nota final (G3: Alto − Bajo)",
      subtitle = paste0(
        "Portugués excluye el 0 (nota menor en consumo alto); ",
        "Matemáticas incluye el 0"
      ),
      x = NULL,
      y = "Diferencia de medias en puntos G3 (IC 95%)"
    )

  p2 <- ggplot(df_a, aes(x = Asignatura, y = diff, color = Metodo)) +
    geom_hline(
      yintercept = 0, linetype = "dashed", color = "#64748b", linewidth = 0.9
    ) +
    geom_errorbar(
      aes(ymin = low, ymax = high),
      width = 0.2, linewidth = 1, position = dodge
    ) +
    geom_point(size = 3.2, position = dodge) +
    coord_flip() +
    scale_color_manual(values = pal, name = "Método de IC 95%") +
    theme_minimal() +
    labs(
      title = "B. Diferencia en inasistencias (Alto − Bajo)",
      subtitle = "Ambas materias excluyen el 0 (más faltas en consumo alto)",
      x = NULL,
      y = "Diferencia de medias en faltas (IC 95%)"
    )

  wrap_plots(p1, p2, ncol = 2, guides = "collect") +
    plot_annotation(
      title = paste0(
        "Diferencias según consumo de alcohol fin de semana ",
        "(Alto 4-5 vs. Bajo 1-2)"
      ),
      subtitle = paste0(
        "Línea gris discontinua = diferencia cero. ",
        "Si el IC 95% no cruza el 0, la diferencia es estadísticamente clara."
      ),
      caption = sprintf(
        paste0(
          "Faltas en Matemáticas (n_alto = %d, n_bajo = %d): ",
          "Welch [%s, %s] | Bootstrap [%s, %s]. ",
          "Datos observacionales: asociación, no causalidad."
        ),
        wc$n_high[1], wc$n_low[1],
        fmt_num(wc$abs_welch_low[1], 2), fmt_num(wc$abs_welch_high[1], 2),
        fmt_num(wc$abs_boot_low[1], 2), fmt_num(wc$abs_boot_high[1], 2)
      ),
      theme = annotation_theme()
    ) &
    theme(legend.position = "bottom")
}

# 4. Pass-rate proportions (G3 >= 10) across alcohol strata with 95% Wilson CIs
chart_pass_proportions <- function(df, facts) {
  subjects <- c(mat = "Matemáticas", por = "Portugués")
  rows <- list()
  for (s in names(subjects)) {
    strat <- walc_stratum(df[[paste0("Walc.", s)]])
    grade <- df[[paste0("G3.", s)]]
    for (lvl in levels(strat)) {
      sub_g <- grade[strat == lvl]
      ci_p <- ci_proportion(sum(sub_g >= 10), length(sub_g))
      rows[[length(rows) + 1L]] <- data.frame(
        Asignatura = subjects[[s]],
        Estrato = lvl,
        prop = 100 * ci_p$prop,
        low = 100 * ci_p$wilson_low,
        high = 100 * ci_p$wilson_high,
        n = ci_p$n,
        stringsAsFactors = FALSE
      )
    }
  }
  df_p <- do.call(rbind, rows)
  df_p$Estrato <- factor(
    df_p$Estrato,
    levels = c("Bajo (1-2)", "Moderado (3)", "Alto (4-5)")
  )
  dodge <- position_dodge(width = 0.35)

  ggplot(
    df_p,
    aes(x = Estrato, y = prop, color = Asignatura, group = Asignatura)
  ) +
    geom_errorbar(
      aes(ymin = low, ymax = high),
      width = 0.16, linewidth = 1, position = dodge
    ) +
    geom_point(size = 3.4, position = dodge) +
    scale_color_manual(
      values = c("Matemáticas" = "#0284c7", "Portugués" = "#10b981")
    ) +
    theme_minimal() +
    labs(
      title = paste0(
        "Proporción de aprobación (G3 >= 10) ",
        "según nivel de alcohol (IC 95%)"
      ),
      subtitle = sprintf(
        paste0(
          "Aprobación global: Portugués %s%% [%s%%, %s%%] ",
          "vs. Matemáticas %s%% [%s%%, %s%%]"
        ),
        fmt_num(100 * facts$pass_rates$por$prop, 1),
        fmt_num(100 * facts$pass_rates$por$wald_low, 1),
        fmt_num(100 * facts$pass_rates$por$wald_high, 1),
        fmt_num(100 * facts$pass_rates$mat$prop, 1),
        fmt_num(100 * facts$pass_rates$mat$wald_low, 1),
        fmt_num(100 * facts$pass_rates$mat$wald_high, 1)
      ),
      caption = paste0(
        "IC de Wilson al 95%. Tamaño por estrato (Mat / Por): ",
        paste(
          sprintf(
            "%s n = %d / %d",
            levels(df_p$Estrato),
            df_p$n[df_p$Asignatura == "Matemáticas"],
            df_p$n[df_p$Asignatura == "Portugués"]
          ),
          collapse = "; "
        ),
        ". Menor n implica intervalos más anchos."
      ),
      x = "Consumo de alcohol en fin de semana (Walc)",
      y = "Porcentaje de aprobación (% con IC 95%)"
    ) +
    annotation_theme() +
    theme(legend.position = "bottom")
}

#' Generate and export the 4 inferential charts at 300 DPI
#'
#' @param res Result list returned by `run_inferential_pipeline()`.
#' @param out_dir Output directory for PNG files.
#' @return A named list of ggplot/patchwork objects.
#' @export
generate_and_export_charts <- function(
  res,
  out_dir = here("actividades", "03-intervalos-confianza", "export_graficos")
) {
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  p1 <- chart_grades_ci(res$facts)
  p2 <- chart_absences_ci(res$merged, res$facts)
  p3 <- chart_alcohol_contrasts(res$facts)
  p4 <- chart_pass_proportions(res$merged, res$facts)

  ggsave(
    file.path(out_dir, "ic_medias_comparativo.png"),
    plot = p1, width = 10, height = 5.2, dpi = 300, bg = "white"
  )
  ggsave(
    file.path(out_dir, "bootstrap_distribucion_inasistencias.png"),
    plot = p2, width = 10, height = 5.2, dpi = 300, bg = "white"
  )
  ggsave(
    file.path(out_dir, "contraste_alcohol_ic.png"),
    plot = p3, width = 10, height = 5.2, dpi = 300, bg = "white"
  )
  ggsave(
    file.path(out_dir, "proporciones_aprobacion_ic.png"),
    plot = p4, width = 9.5, height = 5.2, dpi = 300, bg = "white"
  )

  list(
    p1 = p1, p2 = p2, p3 = p3, p4 = p4,
    means_forest = p1,
    bootstrap_dist = p2,
    contrasts = p3,
    proportions = p4
  )
}
