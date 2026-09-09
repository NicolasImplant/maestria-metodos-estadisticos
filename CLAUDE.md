# CLAUDE.md — maestria-metodos-estadisticos

Project guide for AI assistants. Read before writing or editing code.
This repository consolidates the coursework of **Métodos Estadísticos
Avanzados** (master's degree). All work is in **R**. Every solution is
published on GitHub under the MIT license.

The authoritative stack definition is
`../Stack_Tecnologico_R_Profesional.md`. The rules below are its operational
summary — keep both in sync.

---

## Non-negotiable stack verticals

### 1. Environment

- **IDE:** Positron (Code OSS + `ark` kernel).
- **Dependencies:** `renv` — deterministic, project-local library.
  `renv.lock` is the source of truth. Never `install.packages()` into the
  system library; use `renv::install()` then `renv::snapshot()`.
- **Terminal REPL:** `radian`.

### 2. Architecture

- **No monolithic flat scripts. No `library()` in the global workspace.**
  Import granularly with `box::use(...)`:
  ```r
  box::use(
    dplyr[filter, mutate, select],
    app/core/factory[data_source_factory],
    checkmate[assert_data_frame, assert_string],
  )
  ```
- **OOP with `R6`** for stateful logic, services, and infrastructure
  (DB/API clients). Strict `public` / `private` split. Reference semantics.
  Formal `initialize()` that validates preconditions and injects
  dependencies.
- **Design patterns / SOLID:**
  - Factory Method for complex instantiation (data sources, connectors).
  - SRP: one `R6` class or one functional module per file.
  - DIP: high-level modules depend on abstract `R6` base classes
    (abstract methods call `stop()`), never on concrete classes. Inject
    dependencies through constructors.
- **Type safety — fail fast:** `checkmate` assertions at the head of every
  public function/method (`assert_string`, `assert_int`, `assert_data_frame`,
  …). `typed` only if a change explicitly needs declarative signatures.
- **Pure functional data processing:** analytical transforms are pure, with
  no side effects, composed with the native pipe `|>`. They live in
  `app/pipelines/` and stay decoupled from service code.

### 3. Quality

| Concern | Tool | How |
|---|---|---|
| Unit tests | `testthat` (edition 3) | `Rscript scripts/check.R` or `testthat::test_dir("tests/testthat")` |
| Static analysis | `lintr` | config in `.lintr` (line length 80, cyclocomp ≤ 15, unused vars) |
| Formatting | `styler` | `Rscript scripts/style.R` (tidyverse style) |
| API docs | `roxygen2` | markdown roxygen on every exported function/class |

Run `Rscript scripts/check.R` before every commit. It must pass.

---

## Layout

```
app/            # shared reusable code, imported via box::use(app/...)
  core/         # abstract contracts (interfaces.R) + factories (factory.R)
  services/     # concrete R6 infrastructure (csv_source.R, ...)
  pipelines/    # pure analytical transforms (summary_stats.R, ...)
actividades/    # one self-contained folder per assignment (NN-slug/)
  _plantilla/   # template used by scripts/new_activity.R
tests/testthat/ # unit tests; setup.R wires box.path to the project root
config/         # settings.yml (config::get()) — never secrets
scripts/        # bootstrap.R, style.R, check.R, new_activity.R
```

`box::use(app/...)` resolves because `box.path` is set to the project root
(in `tests/testthat/setup.R` and in each `informe.qmd` setup chunk via
`here::here()`).

---

## Working rules

- **Language:** code, identifiers, comments and roxygen in **English**.
  Report prose (`*.qmd` bodies), `README.md` files and the assignment
  statements are in **Spanish** (that is the deliverable language).
- **Reproducibility:** set `set.seed(2026)` in any script/report with
  randomness. Keep `renv.lock` committed and current.
- **Data:** raw inputs under `actividades/**/datos/` are git-ignored. Keep a
  curated file with `git add -f` and document its origin in the activity
  README.
- **Deliverables:** commit the rendered `informe.html` for each activity.
- **New activity:** `Rscript scripts/new_activity.R <NN> "<Title>"`.
- **Commits:** Conventional Commits, imperative mood, English. No AI
  attribution in commit messages.
- **Memory:** Engram is active. Record decisions, conventions and gotchas
  proactively.

---

## Common commands

```bash
Rscript scripts/bootstrap.R    # restore the renv library (once per clone)
Rscript scripts/style.R        # format in place
Rscript scripts/check.R        # style check + lint + tests (CI gate)
quarto render actividades/<NN-slug>/informe.qmd
radian                         # interactive R session
```
