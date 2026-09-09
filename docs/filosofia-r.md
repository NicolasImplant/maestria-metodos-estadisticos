# Filosofía de uso de R en este proyecto

Este documento resume **por qué** trabajamos R de esta forma, **cómo aporta**
cada decisión y **qué beneficios concretos** obtenemos de la configuración
montada. Es el complemento narrativo de `CLAUDE.md` (que es la referencia
operativa) y de `Stack_Tecnologico_R_Profesional.md` (que es el estándar).

---

## 1. La idea central

> Tratar R como un **lenguaje de ingeniería**, no como una libreta de scripts.

R nació para análisis interactivo: tipado dinámico, evaluación perezosa,
todo en un único espacio de trabajo global. Eso es cómodo para explorar y
frágil para construir. Nuestra apuesta es trasladar a R la disciplina que ya
damos por sentada en otros lenguajes —modularidad, responsabilidad única,
inversión de dependencias, contratos explícitos, pruebas automatizadas— sin
perder su potencia analítica.

El resultado no es "más burocracia": es que el código **sobreviva al
crecimiento**, sea **reproducible** por terceros y **revisable** de un
vistazo.

---

## 2. El problema que resuelve

El "R por defecto" de un trabajo académico suele verse así:

```r
# analisis.R  --  800 lines, one file
library(tidyverse)
library(caret)
library(readxl)
# ... 15 more library() calls

datos <- read_excel("C:/Users/yo/Desktop/base_final_v3_ESTA.xlsx")
datos2 <- datos %>% filter(...) %>% mutate(...)   # global vars everywhere
# ... 780 more lines, copy-pasted from a previous assignment
modelo <- lm(y ~ ., datos2)
```

Síntomas predecibles:

| Síntoma | Causa raíz |
|---|---|
| "En mi máquina funciona" | dependencias y rutas no declaradas |
| Un cambio rompe algo tres pantallas más abajo | estado global compartido |
| Error críptico a mitad de ejecución | sin validación de supuestos |
| Copiar y pegar entre actividades | no hay unidades reutilizables |
| Revisar el código = volver a ejecutarlo entero | sin pruebas ni contratos |

Cada pilar del stack ataca una de esas causas raíz.

---

## 3. Los pilares

### 3.1 Entorno reproducible — `renv` + Positron + `radian`

**Restricción:** nada se instala en la librería global del sistema; el
`renv.lock` es la única fuente de verdad y se versiona.

**Cómo aporta:** `renv` registra la versión exacta de cada paquete (y de R).
Cualquiera clona el repo y ejecuta:

```bash
Rscript scripts/bootstrap.R
```

y obtiene el **mismo entorno, bit a bit**.

**Beneficio:** "funciona en mi máquina" deja de ser una categoría. El
evaluador, un compañero o tú mismo dentro de seis meses ejecutan el informe
y obtienen idénticos resultados. La reproducibilidad pasa de promesa a
propiedad verificable.

---

### 3.2 Modularidad sin contaminación global — `box`

**Restricción:** prohibido `library()` en el espacio de trabajo global y
prohibidos los scripts monolíticos. Solo importaciones granulares y
explícitas con `box::use(...)`.

```r
# styler: off
box::use(
  checkmate[assert_data_frame, assert_subset],
  stats[median, sd],
)
# styler: on
```

> Nota: `box::use(app/core/x)` usa `/` como separador de ruta. `styler` lo
> confunde con una división y le mete espacios, por eso **todo bloque
> `box::use()` va entre `# styler: off` / `# styler: on`**.

**Cómo aporta:** cada archivo declara en su cabecera exactamente qué
funciones usa y de dónde vienen. No hay 20 `library()` acumulados, no hay
colisiones de nombres (`filter` de `dplyr` vs. `filter` de `stats`), y el
namespace es explícito.

**Beneficio:** abrís un archivo y en cinco líneas sabés todo lo que
necesita. Las dependencias reales quedan a la vista, no escondidas en un
`source()` lejano.

---

### 3.3 POO encapsulada y patrones de diseño — `R6`

**Restricción:** la lógica con estado o de infraestructura (conexiones,
lectura de archivos, clientes de API) vive en clases `R6`, una por archivo.
Los módulos de alto nivel dependen de **abstracciones**, no de
implementaciones concretas (Dependency Inversion).

```r
# app/core/interfaces.R  --  the abstract contract
DataSource <- R6Class(
  "DataSource",
  public = list(
    connect    = function() abstract_method("connect"),
    read       = function() abstract_method("read"),
    disconnect = function() abstract_method("disconnect")
  )
)

# app/services/csv_source.R  --  one concrete implementation
CsvSource <- R6Class("CsvSource", inherit = DataSource, public = list(
  read = function() read.csv(private$path, stringsAsFactors = FALSE)
  # ...
))
```

El **Factory Method** centraliza la creación: el consumidor pide por tipo y
recibe el objeto listo, sin conocer la clase concreta.

```r
src <- data_source_factory("csv", path = "datos/base.csv")
datos <- src$connect()$read()
```

**Cómo aporta:** separa la infraestructura (de dónde vienen los datos) del
análisis (qué hacés con ellos). Cambiar de CSV a una base SQL es escribir
una clase nueva y registrarla en la fábrica; el código de análisis **no se
toca**.

**Beneficio:** podés probar el análisis con un doble de prueba en memoria,
sustituir el origen de datos sin efecto dominó, y el diseño se explica solo.

---

### 3.4 Contratos y *fail-fast* — `checkmate`

**Restricción:** aserciones sobre los argumentos en la cabecera de toda
función y método público.

```r
numeric_summary <- function(data, cols) {
  assert_data_frame(data, min.rows = 1L)
  assert_subset(cols, choices = names(data))
  # ... safe from here on
}
```

**Cómo aporta:** en un lenguaje de tipado dinámico, un argumento mal formado
puede propagarse decenas de líneas y explotar lejos de su origen con un
mensaje incomprensible (`$ operator is invalid for atomic vectors`). La
aserción hace que el error salte **en el punto de entrada**, con un mensaje
que nombra el parámetro y lo que esperaba.

**Beneficio:** depuración en segundos en vez de minutos. Y como efecto
secundario, los supuestos de cada función quedan **documentados y
verificados** en el propio código.

---

### 3.5 Procesamiento funcional puro — funciones sin efectos + pipe nativo

**Restricción:** las transformaciones analíticas son funciones **puras**
(mismo input → mismo output, sin efectos secundarios), desacopladas de la
capa de servicios, y se orquestan con el pipe nativo `|>`.

```r
resumen <- datos |>
  limpiar_faltantes() |>
  numeric_summary(cols = c("edad", "ingreso"))
```

**Cómo aporta:** una función pura se entiende leyéndola, se prueba en
aislamiento (no necesita base de datos ni red) y se compone con otras como
piezas de Lego.

**Beneficio:** el análisis es predecible y reutilizable entre actividades.
Si el `numeric_summary` funciona en el módulo 1, funciona igual en el 8.

---

### 3.6 Calidad automatizada — `testthat` + `lintr` + `styler` + `roxygen2`

**Restricción:** `Rscript scripts/check.R` debe pasar antes de cada commit.
Ese único comando encadena:

1. **`styler`** — formato consistente (nadie discute dónde va un espacio).
2. **`lintr`** — análisis estático: complejidad ciclomática, nombres,
   longitud de línea, funciones no deseables.
3. **`testthat`** — el comportamiento está verificado con pruebas.

```r
test_that("numeric_summary computes the expected statistics", {
  df <- data.frame(x = c(1, 2, 3, NA))
  out <- numeric_summary(df, "x")

  expect_equal(out$n_missing, 1L)
  expect_equal(out$mean, 2)
})
```

Y **`roxygen2`** documenta cada función pública con su contrato (parámetros,
retorno, ejemplos) al lado del código.

**Cómo aporta:** convierte "confío en que está bien" en "está verificado".
El gate corre igual en tu máquina y en CI.

**Beneficio:** los informes que publicás en GitHub tienen respaldo
demostrable. Revisar el código es **leerlo**, no reconstruir mentalmente qué
hace.

---

## 4. Qué ganás, en concreto

| Sin el stack | Con el stack |
|---|---|
| Un script plano que crece sin control | Módulos con responsabilidad única |
| "Ejecutá esto y cruzá los dedos" | `bootstrap.R` reproducible y determinista |
| Error críptico a mitad de ejecución | Aserción clara en el punto de entrada |
| Copiar código entre trabajos | `box::use(app/...)` reutilizable |
| Revisión manual, subjetiva | Gate automático (`scripts/check.R`) |
| Resultados sin trazabilidad | Informe Quarto + `renv.lock` = reproducible |

---

## 5. Cómo se traduce en la maestría

- **Cada actividad reusa `app/`** en lugar de reinventar la carga y limpieza
  de datos.
- **El informe Quarto es reproducible:** el evaluador ejecuta
  `quarto render` y obtiene exactamente lo mismo que viste tú.
- **El repositorio público comunica criterio de ingeniería**, no solo el
  resultado numérico de un ejercicio.
- **Escala:** lo que construís en el módulo 1 sostiene al módulo 8. El
  esfuerzo se acumula en vez de repetirse.

---

## 6. El costo honesto

Ninguna decisión es gratis:

- **Curva de entrada:** `box` y `R6` tienen fricción las primeras semanas.
- **Overhead en tareas mínimas:** para un análisis de 20 líneas, la
  estructura puede parecer desproporcionada.
- **Primera restauración lenta:** `renv::restore()` compila/descarga todo
  una vez por máquina.

**Mitigación:** la plantilla (`actividades/_plantilla/`) y el generador
(`scripts/new_activity.R`) absorben casi todo el *boilerplate*. El costo se
paga una vez; el beneficio se cobra en cada actividad siguiente.

---

## 7. Regla mental de una línea

> **Dirigimos, la herramienta ejecuta. Primero entender el concepto; el
> código viene después.** El stack existe para que esa disciplina sea el
> camino más fácil, no el más difícil.
