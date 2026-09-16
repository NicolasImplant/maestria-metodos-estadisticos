# Datos de Student Performance (Rendimiento Escolar)

Este directorio contiene las bases de datos crudas utilizadas para el análisis descriptivo de la Unidad 2 de Métodos Estadísticos Avanzados.

## Origen de los Datos
Los datos provienen del repositorio de Machine Learning de la UC Irvine (UCI) y fueron descargados en formato comprimido dentro de la carpeta `Modulo 2` de la maestría.

* **Fichero de Matemáticas:** `student-mat.csv` (395 observaciones, 33 variables)
* **Fichero de Portugués:** `student-por.csv` (649 observaciones, 33 variables)

## Cohorte Común
Siguiendo las indicaciones del archivo `student-merge.R` adjunto en la base de datos, existe una intersección de **382 estudiantes** comunes a ambas asignaturas que pueden identificarse unívocamente mediante 13 atributos demográficos:

`c("school", "sex", "age", "address", "famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob", "reason", "nursery", "internet")`

## Estado en Git
Este directorio y los archivos `.csv` están omitidos del repositorio Git mediante `.gitignore` para cumplir con las políticas de control de versiones y almacenamiento de grandes ficheros de datos crudos.
