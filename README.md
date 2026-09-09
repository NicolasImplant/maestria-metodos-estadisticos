# Maestría — Métodos Estadísticos Avanzados

[![Licencia: MIT](https://img.shields.io/badge/Licencia-MIT-blue.svg)](LICENSE)
[![Lenguaje: R](https://img.shields.io/badge/R-%E2%89%A5%204.6-276DC3.svg)](https://www.r-project.org/)

Repositorio de trabajo de la asignatura **Métodos Estadísticos Avanzados**.
Consolida, actividad por actividad, el código de análisis y los informes
reproducibles en R. Todas las soluciones se publican abiertamente bajo
licencia MIT.

## Estándar de ingeniería

El proyecto sigue un estándar formal de ingeniería de software para R
(detalle completo en [`CLAUDE.md`](CLAUDE.md) y en el documento de stack del
curso):

- **Entorno reproducible:** Positron + [`renv`](https://rstudio.github.io/renv/)
  (`renv.lock` como fuente de verdad) + `radian`.
- **Arquitectura modular:** importaciones granulares con
  [`box`](https://klmr.me/box/); nada de `library()` global ni scripts
  planos.
- **POO y patrones:** clases [`R6`](https://r6.r-lib.org/) para lógica con
  estado e infraestructura; Factory Method, SRP y Dependency Inversion.
- **Contratos y fail-fast:** aserciones con
  [`checkmate`](https://mllg.github.io/checkmate/) en toda función pública.
- **Procesamiento funcional puro:** transformaciones sin efectos
  secundarios, orquestadas con el pipe nativo `|>`.
- **Calidad:** `testthat` (pruebas), `lintr` (análisis estático),
  `styler` (formato), `roxygen2` (documentación).

## Estructura

```
app/            Código compartido y reutilizable (box::use(app/...))
  core/         Contratos abstractos y fábricas
  services/     Implementaciones concretas R6 (infraestructura)
  pipelines/    Transformaciones analíticas puras
actividades/    Una carpeta autocontenida por actividad (NN-slug/)
tests/testthat/ Pruebas unitarias
config/         Configuración (settings.yml)
scripts/        Automatización (bootstrap, style, check, new_activity)
```

## Puesta en marcha

Requisitos: R ≥ 4.6, Quarto, y (opcional) Positron y `radian`.

```bash
git clone git@github.com:NicolasImplant/maestria-metodos-estadisticos.git
cd maestria-metodos-estadisticos
Rscript scripts/bootstrap.R      # instala renv y restaura la librería
```

## Flujo de trabajo

```bash
# 1. Crear una actividad nueva
Rscript scripts/new_activity.R 02 "Regresión lineal múltiple"

# 2. Trabajar el análisis en app/ y en actividades/<NN-slug>/R/

# 3. Verificar calidad antes de commitear
Rscript scripts/style.R
Rscript scripts/check.R

# 4. Renderizar el informe
quarto render actividades/<NN-slug>/informe.qmd
```

## Actividades

Ver el índice en [`actividades/README.md`](actividades/README.md).

| # | Actividad | Estado |
|---|---|---|
| 01 | Manejo de bases de datos en R | 🚧 En curso |

## Licencia

Publicado bajo la licencia [MIT](LICENSE). Uso libre conservando la
atribución de autoría.

Autor: **Juan Nicolás Patiño Rodríguez**
