# SDD-02: Unit 2 Descriptive Statistics Specification

This document provides the detailed software design and functional requirements for the Unit 2 assignment **'02-estadistica-descriptiva'** in the Master's Program course "Métodos Estadísticos Avanzados".

---

## 1. Context & Scope

The purpose of this assignment is to perform a comprehensive descriptive statistical analysis of the **Student Performance Dataset** (comprising student records for Mathematics and Portuguese courses). The system must load, clean, merge, and analyze these datasets using professional R programming standards, structured pipelines, strict validation, and reproducible reports.

### Pipeline Architecture Workflow

```mermaid
flowchart TD
    subgraph Ingestion ["1. Data Ingestion"]
        mat_csv["student-mat.csv (Raw)"] -->|CsvSource (sep = ';')| df_mat_raw["Raw Math Data Frame"]
        por_csv["student-por.csv (Raw)"] -->|CsvSource (sep = ';')| df_por_raw["Raw Portuguese Data Frame"]
    end

    subgraph Preprocessing ["2. Preprocessing & Merging"]
        df_mat_raw -->|clean_student_data| df_mat_clean["Cleaned Math (G1, G2 converted)"]
        df_por_raw -->|clean_student_data| df_por_clean["Cleaned Portuguese (G1, G2 converted)"]
        df_mat_clean & df_por_clean -->|merge_student_data (13 Keys)| df_merged["Merged Dataset (N=382)"]
    end

    subgraph Analysis ["3. Analytical Pipelines"]
        df_merged -->|numeric_summary| dt_quant["Descriptive Stats (mean, median, mode, var, sd, range)"]
        df_merged -->|categorical_summary| dt_qual["Frequency Tables (counts, proportions)"]
    end

    subgraph Presentation ["4. Reproducible Report"]
        dt_quant & dt_qual -->|box::use modular imports| qmd_report["informe.qmd"]
        qmd_report -->|quarto render| html_output["informe.html (Deliverable)"]
    end

    style Ingestion fill:#f9f,stroke:#333,stroke-width:2px
    style Preprocessing fill:#bbf,stroke:#333,stroke-width:2px
    style Analysis fill:#dfd,stroke:#333,stroke-width:2px
    style Presentation fill:#fdd,stroke:#333,stroke-width:2px
```

---

## 2. Functional Requirements

### FR-1: Data Ingestion
* **FR-1.1**: The system MUST load the raw datasets `student-mat.csv` and `student-por.csv` from the local `datos/` directory.
* **FR-1.2**: The ingestion mechanism MUST utilize the R6-based `CsvSource` class or a conforming concrete implementation of the `DataSource` interface.
* **FR-1.3**: The ingestion service MUST parse the datasets using a semi-colon delimiter (`;`) as the primary field separator.
* **FR-1.4**: All text columns MUST be read as character vectors rather than factors during the initial ingestion phase.

### FR-2: Data Preprocessing & Cleaning
* **FR-2.1**: The preprocessing pipeline MUST resolve the quoted string numbers in columns `G1` and `G2` (e.g. converting `"5"` to `5L` and `"12"` to `12L`).
* **FR-2.2**: String-cleaning operations MUST strip any double quotes (`"`) and extraneous whitespace before type casting.
* **FR-2.3**: Columns `G1` and `G2` MUST be explicitly cast to integers in R (`integer` type).
* **FR-2.4**: Column `G3` MUST be validated as integer or numeric, and any type discrepancies MUST fail-fast during cleaning.

### FR-3: Data Merging
* **FR-3.1**: The merging pipeline MUST combine common student records from the cleaned Math and Portuguese datasets.
* **FR-3.2**: The merge operation MUST use exactly the following thirteen (13) key fields to identify common students:
  `c("school", "sex", "age", "address", "famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob", "reason", "nursery", "internet")`
* **FR-3.3**: The merging pipeline MUST result in exactly **382 rows** for the merged cohort.
* **FR-3.4**: Non-common columns that overlap between both datasets (e.g., `failures`, `G1`, `G2`, `G3`, `absences`) MUST be assigned distinct, readable suffixes in the output dataset, specifically `.mat` (for Mathematics) and `.por` (for Portuguese) to avoid default `.x` and `.y` suffixes.

### FR-4: Analytical Summary - Quantitative Variables
* **FR-4.1**: The system MUST compute a comprehensive suite of descriptive statistics for selected quantitative variables (such as `absences`, `G1`, `G2`, and `G3` for both subjects).
* **FR-4.2**: The computed descriptive statistics for each column MUST include:
  1. **Mean** (arithmetic average)
  2. **Median** (middle value)
  3. **Mode** (custom statistical mode - the most frequent value)
  4. **Minimum** (minimum value)
  5. **Maximum** (maximum value)
  6. **Standard Deviation** (`sd`)
  7. **Variance** (`var`)
  8. **Range** (the difference: `max - min`)
* **FR-4.3**: Since R has no built-in statistical mode function, the pipeline MUST implement a custom, pure, deterministic mode-finding function.
* **FR-4.4**: The mode function MUST handle ties deterministically: if there are multiple values with the maximum frequency, it MUST select the smallest numeric value (or first alphabetical value).
* **FR-4.5**: The quantitative summary function MUST validate input preconditions using `checkmate`:
  - Input `data` MUST be a non-empty `data.frame`.
  - Input `cols` MUST be a character vector representing valid, existing columns in the data frame.
  - Every specified column in `cols` MUST be numeric or integer.

### FR-5: Analytical Summary - Qualitative Variables
* **FR-5.1**: The analytical pipeline MUST generate frequency tables for specified qualitative variables (e.g. `sex`, `address`, `Mjob`, `Fjob`).
* **FR-5.2**: Each frequency table MUST contain the following fields:
  - `variable`: Name of the qualitative column.
  - `value`: Categorical value/level.
  - `count`: Absolute frequency (integer count).
  - `proportion`: Relative frequency (value between `0` and `1`).
* **FR-5.3**: The qualitative summary function MUST validate input preconditions using `checkmate`:
  - Input `data` MUST be a non-empty `data.frame`.
  - Input `cols` MUST be a character vector of valid, existing columns.
  - Every specified column in `cols` MUST be a character vector or factor.

### FR-6: Quarto Report & Reproducibility
* **FR-6.1**: The deliverable MUST be a beautifully structured Quarto report located at `actividades/02-estadistica-descriptiva/informe.qmd`.
* **FR-6.2**: The report MUST set a global random seed using `set.seed(2026)` in its setup block to ensure perfect reproducibility.
* **FR-6.3**: The report MUST load all business logic, data loaders, and analytical pipelines via **`box` modular imports** (e.g., `box::use(app/...)`). It MUST NOT call `library()` in the global workspace.
* **FR-6.4**: The report MUST contain the seven standard sections:
  1. **Contexto y objetivo**
  2. **Datos**
  3. **Metodología**
  4. **Desarrollo**
  5. **Resultados**
  6. **Conclusiones**
  7. **Referencias**
* **FR-6.5**: Tables generated in the report MUST be rendered using a clear, human-readable format (e.g., using `df-print: paged` or custom markdown tables).

---

## 3. Non-Functional Requirements

### NFR-1: Type Safety & Validation (Fail-Fast)
* All public pipeline functions and service methods MUST assert their argument types and structures at the very first line of execution using `checkmate` assertions.
* Precondition failures MUST throw clear, informative error messages immediately.

### NFR-2: Pure Functional Pipelines
* All data transform functions in `app/pipelines/` MUST be pure functions, devoid of side-effects, allowing safe execution and straightforward composition using the native R pipe operator `|>`.

### NFR-3: Quality Standards & Clean Code
* All code MUST conform to the tidyverse style guide and pass formatting via `styler`.
* Static analysis via `lintr` MUST pass with zero warnings (verified by `scripts/check.R`).
* Every exported function and public module contract MUST be fully documented using `roxygen2` syntax.

---

## 4. Testable Scenarios (Given-When-Then)

### Scenario 1: Loading raw student data with semi-colon delimiter
* **Given** the raw student dataset `student-mat.csv` located at the correct path
* **And** the file uses a semi-colon separator (`;`)
* **When** the `CsvSource` instance is initialized with `sep = ";"` and its `read()` method is executed
* **Then** it MUST successfully parse the columns without merging lines
* **And** the returned data frame MUST contain exactly **33 columns**
* **And** the row count MUST be exactly **395** (excluding header).

### Scenario 2: Preprocessing G1 and G2 variables to resolve quotes
* **Given** a raw dataset with string-represented grades in columns `G1` and `G2`
* **When** the cleaning pipeline function `clean_student_data` is invoked on the data frame
* **Then** it MUST remove any quotation marks around numeric values
* **And** it MUST cast the cleaned values to R `integer` format
* **And** `checkmate::assert_integer()` on `G1` and `G2` MUST pass without error.

### Scenario 3: Merging Math and Portuguese datasets on common keys
* **Given** a cleaned Math data frame of size $395 \times 33$
* **And** a cleaned Portuguese data frame of size $649 \times 33$
* **When** the merge pipeline function `merge_student_data` is invoked using the 13 specified keys
* **Then** the resulting merged data frame MUST have exactly **382 rows**
* **And** non-common fields like `G3` and `absences` MUST exist as separate columns with `.mat` and `.por` suffixes (e.g., `G3.mat`, `G3.por`).

### Scenario 4: Computing quantitative statistics
* **Given** a valid data frame with numeric columns
* **When** the pipeline function `numeric_summary` is executed with columns `c("absences", "G3")`
* **Then** it MUST return a data frame with exactly 2 rows
* **And** the column names of the output MUST be exactly `c("variable", "n", "n_missing", "mean", "median", "mode", "min", "max", "sd", "var", "range")`
* **And** the value of `range` MUST equal `max - min`
* **And** the calculated `mode` MUST match the most frequent value (resolving ties by returning the lowest value).

### Scenario 5: Generating frequency tables for qualitative columns
* **Given** a valid data frame containing categorical columns
* **When** the pipeline function `categorical_summary` is executed with column `"address"`
* **Then** it MUST return a data frame containing the unique categories (`U` and `R`)
* **And** the table columns MUST be exactly `c("variable", "value", "count", "proportion")`
* **And** the sum of `proportion` across all levels of the variable MUST equal exactly `1.0` (allowing for standard floating-point precision).

### Scenario 6: Quarto report generation uses modular imports
* **Given** the Quarto report template and source code
* **When** the file `informe.qmd` is inspected
* **Then** it MUST contain modular imports for data pipelines and summaries (e.g., `box::use(app/pipelines/...)`)
* **And** it MUST NOT contain any global package attachments (`library()` or `require()`)
* **And** running `quarto render actividades/02-estadistica-descriptiva/informe.qmd` MUST compile successfully to a valid, clean HTML file.

---

## 5. Directory Layout & Proposed Files

To implement this assignment conforming to the established architecture, the following files SHALL be created or modified:

```
maestria-metodos-estadisticos/
├── actividades/
│   └── 02-estadistica-descriptiva/
│       ├── R/
│       │   └── solucion.R            # Activity-specific workflow orchestrator
│       ├── datos/
│       │   ├── README.md              # Documentation of source files origin
│       │   ├── student-mat.csv        # Symlinked or copied (git-ignored)
│       │   └── student-por.csv        # Symlinked or copied (git-ignored)
│       ├── README.md                  # Activity description and build instructions
│       └── informe.qmd                # The Quarto document using modular imports
├── app/
│   ├── pipelines/
│   │   ├── preprocessing.R            # NEW: Handles cleaning (G1/G2) and merging
│   │   └── summary_stats.R            # MODIFIED: Expanded to include custom mode, var, range, and categorical summaries
└── tests/testthat/
    ├── test-preprocessing.R           # NEW: Unit tests for preprocessing & merging
    └── test-summary_stats.R           # MODIFIED: Unit tests for new stats and categorical frequency tables
```

> [!IMPORTANT]
> The raw data files `student-mat.csv` and `student-por.csv` MUST be copied from the root workspace folder `Modulo 2/student_performance/extracted/` into the activity's local `datos/` directory, which is git-ignored by default. A clear reference explaining this step MUST be written in `actividades/02-estadistica-descriptiva/datos/README.md`.

> [!TIP]
> The custom mode implementation should be optimized and pure. A vector-based approach using `table()` or `tabulate()` can be implemented cleanly:
> ```r
> get_mode <- function(x) {
>   if (length(x) == 0L) return(NA)
>   tbl <- table(x)
>   modes <- names(tbl)[tbl == max(tbl)]
>   # Cast back to original class or numeric if applicable, resolving ties
>   val <- as.numeric(modes)
>   if (any(is.na(val))) val <- modes # fallback to character
>   min(val) # deterministic tie-break
> }
> ```
