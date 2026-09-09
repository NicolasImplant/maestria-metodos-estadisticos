#!/usr/bin/env Rscript
# Scaffold a new activity folder under actividades/ from the standard layout.
#
#   Rscript scripts/new_activity.R 02 "Regresion lineal multiple"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) {
  stop("Usage: Rscript scripts/new_activity.R <number> <title>", call. = FALSE)
}

number <- sprintf("%02d", as.integer(args[[1]]))
title <- args[[2]]

ascii <- tolower(iconv(title, to = "ASCII//TRANSLIT"))
slug <- gsub("[^a-z0-9]+", "-", ascii)
slug <- gsub("(^-|-$)", "", slug)
dir_name <- sprintf("%s-%s", number, slug)
target <- file.path("actividades", dir_name)

if (dir.exists(target)) {
  stop(sprintf("'%s' already exists.", target), call. = FALSE)
}

dir.create(file.path(target, "R"), recursive = TRUE)
dir.create(file.path(target, "datos"))
writeLines(character(), file.path(target, "datos", ".gitkeep"))

readme <- c(
  sprintf("# Actividad %s — %s", number, title),
  "",
  "## Enunciado",
  "",
  "_Pega aquí el enunciado de la actividad._",
  "",
  "## Objetivos",
  "",
  "- ...",
  "",
  "## Cómo reproducir",
  "",
  "```bash",
  sprintf("quarto render actividades/%s/informe.qmd", dir_name),
  "```",
  ""
)
writeLines(readme, file.path(target, "README.md"))

qmd <- readLines("actividades/_plantilla/informe.qmd")
qmd <- gsub("\\{\\{NUMBER\\}\\}", number, qmd)
qmd <- gsub("\\{\\{TITLE\\}\\}", title, qmd)
writeLines(qmd, file.path(target, "informe.qmd"))

solution <- c(
  "#' @title Solución — código de análisis",
  "#'",
  sprintf("#' Actividad %s — %s", number, title),
  "",
  "box::use(",
  "  app/pipelines/summary_stats[numeric_summary],",
  ")",
  "",
  "# Escribe aquí funciones puras y llámalas desde informe.qmd.",
  ""
)
writeLines(solution, file.path(target, "R", "solucion.R"))

message(sprintf("Created %s", target))
