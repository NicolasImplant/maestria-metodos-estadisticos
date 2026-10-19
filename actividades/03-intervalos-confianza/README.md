# Actividad 03 — Introducción a la Inferencia Estadística: Intervalos de Confianza

Segundo avance del proyecto de **S1 Métodos Estadísticos Avanzados 1** (Maestría en Inteligencia Artificial — Universidad de La Sabana).

## Alcance ("Milla Extra")

Construye sobre la cohorte de $N = 382$ estudiantes de la Unidad 2 (*Student Performance Dataset*) e incorpora inferencia estadística por intervalos de confianza al 95% (paramétricos y no paramétricos vía Bootstrap Percentil con $B = 5000$ réplicas) sobre las **3 variables clave**:

1. **`G3` (`G3.mat`, `G3.por`)**: Rendimiento académico final (escala 0 a 20), comparando $t$-Student clásico vs. Bootstrap Percentil, diferencia pareada intra-sujeto y proporción de aprobación ($G3 \ge 10$).
2. **`absences` (`absences.mat`, `absences.por`)**: Inasistencias escolares con fuerte asimetría positiva ($g_1 = 4.01$ en Matemáticas), demostrando por qué el método Bootstrap evita depender de supuestos de normalidad en colas largas.
3. **`Walc` (`Walc.mat`, `Walc.por`)**: Consumo de alcohol en fin de semana (escala ordinal 1 a 5), estimando intervalos de confianza para la diferencia de medias ($\Delta G3$ y $\Delta \text{absences}$ entre consumo alto 4–5 y bajo 1–2) mediante Welch-Satterthwaite y Bootstrap de dos muestras.

## Entregables

* `informe.qmd` $\rightarrow$ `informe.html`: Reporte técnico paso a paso en RStudio con justificación de variables, preguntas de investigación cuantificables, desarrollo matemático/computacional, gráficas e interpretaciones.
* `presentacion.qmd` $\rightarrow$ `presentacion.html`: Presentación Reveal.js de alto contraste con cámara flotante PiP y cronómetro HUD calibrado a **150 segundos (2:30 min)** (`overlay_presentador.html`).
* `guia_exponente.tex` $\rightarrow$ `guia_exponente.pdf`: Guion cronometrado segundo a segundo para la grabación del video de YouTube (máximo 2:30 min, autorizado por el profesor; la guía indica 1:30).

## Comandos de compilación

Desde la raíz de `maestria-metodos-estadisticos`:

```bash
Rscript scripts/check.R
quarto render actividades/03-intervalos-confianza/informe.qmd
quarto render actividades/03-intervalos-confianza/presentacion.qmd
```
