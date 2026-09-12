# Análisis de Calidad del Agua - Ibagué (IBAL)

Análisis Exploratorio de Datos (EDA) sobre la calidad del agua del municipio de Ibagué, Colombia, utilizando datos de la Empresa Ibaguereña de Acueducto y Alcantarillado (IBAL).

## Descripción

Este proyecto realiza un análisis estadístico de los parámetros fisicoquímicos y microbiológicos del agua potable en Ibagué. Los datos provienen del portal [Datos Abiertos Colombia](https://www.datos.gov.co/Vivienda-Ciudad-y-Territorio/CALIDAD-DE-AGUA/syfm-bqhq/about_data).

## Variables Analizadas

| Variable             | Tipo        | Unidad        | Descripción                                    |
|----------------------|-------------|---------------|------------------------------------------------|
| año / mes            | Temporal    | -             | Componentes temporales de la fecha de medición |
| nuqfus_fct           | Cualitativa | -             | Punto de muestreo (NUQFUS)                     |
| ph                   | Continua    | unidades      | Potencial de Hidrógeno (0-14)                  |
| temperatura          | Continua    | °C            | Temperatura de la muestra                      |
| turbiedad            | Continua    | UNT           | Opacidad o claridad del agua                   |
| color_real           | Continua    | UPC (Pt-Co)   | Unidades de Color Real                         |
| conductividad        | Continua    | µS/cm         | Capacidad para conducir electricidad           |
| oxigeno_disuelto     | Continua    | mg/L          | Concentración de oxígeno disuelto              |
| cloruros             | Continua    | mg/L          | Concentración de cloruros                      |
| dureza               | Continua    | mg/L CaCO₃    | Dureza total                                   |
| coliformes_totales   | Discreta    | NMP/100 mL    | Conteo de coliformes totales                   |
| e_coli               | Discreta    | NMP/100 mL    | Conteo de *E. coli*                            |

## Estructura del Proyecto

```
bioestadistica/
├── data/
│   └── CALIDAD_DE_AGUA.csv      # Dataset original
├── src/
│   └── Functions_agua.R         # Funciones de análisis y visualización
├── EDA_Agua_ibague.Rmd          # Documento R Markdown principal
├── EDA_Agua_ibague.html         # Reporte HTML generado
├── renv.lock                    # Lockfile de dependencias (renv)
├── renv/                        # Ambiente virtual de R
└── bioestadistica.Rproj         # Proyecto de RStudio
```

## Requisitos

- **R** >= 4.4.2
- **RStudio** (recomendado)
- Paquetes principales:
  - tidyverse
  - janitor
  - lubridate
  - scales
  - patchwork
  - corrplot
  - knitr / kableExtra
  - RColorBrewer

## Instalación

1. Clonar el repositorio:

   ```bash
   git clone <url-del-repositorio>
   cd bioestadistica
   ```

2. Abrir el proyecto en RStudio (`bioestadistica.Rproj`)

3. Restaurar el ambiente con `renv`:

   ```r
   renv::restore()
   ```

4. Ejecutar el análisis:

   ```r
   rmarkdown::render("EDA_Agua_ibague.Rmd")
   ```

## Contenido del Análisis

### Análisis Univariado

- Distribución temporal (año, mes)
- Frecuencia por punto de muestreo
- Histogramas, densidades y boxplots para variables continuas
- Estadísticos descriptivos (media, mediana, cuartiles, desviación estándar)

### Análisis Bivariado

- Matriz de correlación de Pearson (escala lineal y logarítmica)
- Relación entre turbiedad y contaminación microbiológica
- Comportamiento estacional de parámetros clave
- Variación por punto de muestreo

### Tratamiento de Datos

- Limpieza automática de nombres de columnas
- Conversión de fechas y extracción de componentes temporales
- Corrección de artefactos (ej: oxígeno disuelto > 20 mg/L → NA)
- Transformaciones logarítmicas para variables con sesgo extremo

## Autor

**Daniel Alvarado** - 145310

Proyecto desarrollado para el curso de Bioestadística.

## Fuente de Datos

- **Proveedor**: Alcaldía de Ibagué / IBAL
- **Portal**: [Datos Abiertos Colombia](https://www.datos.gov.co)
- **Dataset**: [CALIDAD DE AGUA](https://www.datos.gov.co/Vivienda-Ciudad-y-Territorio/CALIDAD-DE-AGUA/syfm-bqhq/about_data)

## Licencia

Este proyecto es de uso académico. Los datos son de dominio público según la política de datos abiertos de Colombia.
