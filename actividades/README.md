# Actividades

Una carpeta por actividad, con el prefijo `NN-` y un slug descriptivo.
Cada carpeta es autocontenida: enunciado, código de análisis, informe
reproducible y datos locales.

| # | Actividad | Estado | Informe |
|---|---|---|---|
| 01 | [Manejo de bases de datos](01-manejo-de-bases-de-datos/) | 🚧 En curso | — |

## Crear una actividad nueva

```bash
Rscript scripts/new_activity.R 02 "Regresión lineal múltiple"
```

Genera la estructura estándar a partir de `actividades/_plantilla/`.

## Estructura de cada actividad

```
NN-slug/
├── README.md        # enunciado, objetivos, cómo reproducir
├── informe.qmd      # informe Quarto (entregable)
├── informe.html     # render versionado
├── R/
│   └── solucion.R   # código de análisis (funciones puras)
└── datos/           # insumos crudos (no versionados)
```
