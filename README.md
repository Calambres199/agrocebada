# Reto AgroCebada 2026: prediccion del rendimiento de cebada

Modelo para estimar el rendimiento (t/ha) de 197 parcelas de cebada de temporal en Hidalgo, Tlaxcala y Puebla, ciclo abril-octubre 2025, a partir de indices satelitales, clima y topografia. Desarrollado para el **Reto AgroCebada 2026 de FIRA**.

**Aplicacion en linea:** https://calambres199.github.io/agrocebada/

**Video:** https://youtu.be/1GEeJDZ8yp4?si=VbJI8RiiMweC-vOk

> La aplicacion corre directamente en el navegador. La primera vez puede tardar unos segundos en cargar.

---

## Estructura del repositorio

```
agrocebada/
├── README.md        <- este archivo
├── codigos/         <- codigos en R del analisis (se corren en orden)
├── app/
│   ├── app.R        <- codigo de la aplicacion (shiny)
│   └── datos/       <- base del modelo y predicciones que usa la app
└── docs/            <- la app convertida en pagina web con shinylive (no se edita a mano)
```

---

## Datos

| Base | Fuente | Contenido |
|---|---|---|
| Dataset basico | Sentinel-2 | Indices satelitales por parcela y fecha, 2022-2025 |
| Dataset PRO | Planet | Indices satelitales por parcela y fecha, 2025 |
| Precipitacion | CHIRPS | Precipitacion mensual, 2022-2025 (archivos .tif) |
| Temperatura minima y maxima | CHIRTS-ERA5 | Promedios mensuales, 2022-2025 (archivos .tif) |
| Elevacion y pendiente | INEGI, CEM 4.0 | Valor fijo por parcela (archivos .tif) |
| Rendimiento | Datos del reto | Rendimiento (t/ha) de 138 parcelas; las otras 59 se predicen |

---

## Requisitos

- R y RStudio
- Paquetes:

```r
install.packages(c("terra", "dplyr", "tidyr", "zoo", "readr", "tseries",
                   "forecast", "corrplot", "shiny", "bslib", "shinylive"))
```

- Los datos del reto (DATASET_RETO_AGRO_2026). En cada codigo hay que cambiar el `setwd()` por la carpeta donde esten los datos en su computadora.

---

## Orden para correr los codigos y que debe salir

### 1. Extraccion de clima y topografia
Saca de los archivos .tif el valor de precipitacion, temperatura minima, temperatura maxima, elevacion y pendiente de cada parcela usando el shapefile de las parcelas.

**Debe salir:** una base por variable con las 197 parcelas y un dato por mes de 2022 a 2025 (48 meses).

### 2. Filtro de nubes y seleccion de indices
Quita las imagenes con mas del 20% de nubes y se queda con el EVI, MSI y NDSVI de Sentinel-2.

**Debe salir:** `BASICO_seleccion.csv`, con las 197 parcelas y solo el sensor Sentinel-2.

### 3. Series de tiempo (precipitacion, tmin, tmax, EVI, MSI y NDSVI)
Para cada variable: regulariza la serie (mediana por mes y relleno de meses vacios con `na.approx`), hace las pruebas ADF, KPSS y Phillips-Perron, ajusta un modelo con `auto.arima` y revisa los residuos con Ljung-Box, para cada una de las 197 parcelas.

**Debe salir:** la grafica de las series, el boxplot por mes, el histograma de residuos y una tabla de resultados por parcela. Algunos resultados que deben coincidir:

| Variable | Resultado principal |
|---|---|
| Precipitacion | 100% de las parcelas estacionarias; todos los modelos con deriva |
| Temperatura minima | La prueba Phillips-Perron no indico estacionariedad en ninguna parcela (0%) |
| Temperatura maxima | Todas las parcelas con el modelo ARIMA(0,1,1)(1,1,0)[12] |
| EVI | 31% estacionarias; 38 modelos distintos, 27 con deriva |
| MSI | 1% estacionarias (2 parcelas); 15 con deriva |
| NDSVI | 76.1% estacionarias; 79.7% con parte estacional; 12 con deriva |

### 4. Etapas fenologicas con el EVI de Planet
Grafica la curva del EVI de Planet 2025 (mediana de las 197 parcelas) para ubicar las etapas de la escala de Zadoks.

**Debe salir:** la grafica `curva_evi_planet_2025.png`, con el maximo del EVI el **12 de septiembre de 2025**. Las etapas quedan asi:

| Etapa (Zadoks) | Meses |
|---|---|
| Germinacion (Z0-Z1) | abril a junio |
| Formacion de tallos (Z2-Z3) | julio |
| Espigado y llenado del grano (Z4-Z8) | agosto y septiembre |
| Madurez (Z9) | septiembre y octubre |

### 5. Construccion de variables
Resume cada variable de 2025 en un solo valor por parcela segun la etapa del cultivo (la lluvia se suma, las temperaturas se promedian, a los indices se les saca la mediana) y une todo con `left_join()`.

**Debe salir:** `base_modelo_rendimiento.csv`, con las 197 parcelas y las 14 variables.

### 6. Correlacion
**Debe salir:** `corrplot.png` y `dispersion_top4.png`. Las variables mas relacionadas con el rendimiento son la lluvia en la siembra (-0.66) y la temperatura minima del ciclo (0.54). Se quitan tres variables de lluvia por tener correlacion mayor a 0.8 con la lluvia en la siembra.

### 7. Regresion lineal multiple y prediccion
Ajusta el modelo con las 138 parcelas, elige las variables con `step()`, hace la validacion cruzada de 5 grupos (`set.seed(123)`) y predice las 59 parcelas.

**Debe salir:**

- Modelo con 11 variables: R² = 0.621
- `step()` quita evi_llenado, AREA, msi, evi_max, dia_pico y ndsvi (el AIC baja de -155.35 a -165.91)
- Modelo final con 5 variables: prec_siembra, tmin_ciclo, tmax_llenado, elevacion_m y pendiente_grados
- R² = 0.617, R² ajustado = 0.602, error estandar residual = 0.537
- Validacion cruzada: RMSE = 0.560, MAE = 0.420, R² = 0.565
- Predicciones de las 59 parcelas entre 2.63 y 5.02 t/ha (promedio 3.90), guardadas en `predicciones_rendimiento.csv`
- Graficas `residuos_modelo.png` y `real_vs_predicho.png`

Ecuacion del modelo final:

```
Rendimiento = 28.378 - 0.0046 prec_siembra + 0.647 tmin_ciclo - 1.186 tmax_llenado
              - 0.0014 elevacion_m + 0.053 pendiente_grados
```

---

## Aplicacion web

Para abrirla en la computadora, abrir `app/app.R` en RStudio y presionar **Run App**, o escribir:

```r
shiny::runApp("app")
```

Para actualizar la version web despues de hacer cambios:

```r
shinylive::export("app", "docs")
```

y subir los cambios a GitHub.

La app tiene 5 pestanas: consulta por parcela, predicciones (con descarga en .csv), simulador, informacion del modelo y acerca del proyecto.

---

## Equipo

Universidad Autonoma del Estado de Hidalgo - Licenciatura en Matematicas Aplicadas

- Alan Yael Diaz Vizzuett
- Bryan Uriel Islas Sanchez
- Brenda Esmeralda Martinez de la Cruz
- Chac de Jesus Moreno Torres
- Jared Sanchez Martinez

---

## Uso de inteligencia artificial

De acuerdo con las bases del reto, se declara el uso de inteligencia artificial (Claude, de Anthropic) como apoyo en el codigo en R (extracción de datos de las imágenes .tif), el desarrollo de la aplicacion. Los prompts utilizados se incluyen en los anexos del reporte tecnico.
