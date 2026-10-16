# Limpiamos el ambiente de trabajo
rm(list = ls())

# Instalamos las librerias a utilizar
install.packages("dplyr")           # Para manejar tablas
install.packages("tidyr")           # Para cambiar la forma de las tablas
install.packages("readr")           # Para guardar tablas en csv
install.packages("forecast")        # Para estimar diferentes modelos
install.packages("tseries")         # Para el análisis de series de tiempo

# Cargamos las librerias
library(dplyr)
library(tidyr)
library(readr)
library(forecast)
library(tseries)

# Le digo a R en que carpeta voy a estar trabajando
setwd('/Users/rebec/Downloads/DataSet/DATASET_RETO_AGRO_2026')

# Visualizo que tiene la carpeta en la que voy a trabajar
list.files()

# PRECIPITACIÓN -----------------------------------------------------------

# Llamamos a la base de datos que se extrajo de las imagenes .tif de precipitacion
precipitacion1 = read.csv("1_Precipitacion_Extraida.csv")

# Visualizamos las primeras filas de la base de datos precipitacion1
head(precipitacion1)

# Convertimos la columna mes a tipo numerico
precipitacion1$mes = as.numeric(precipitacion1$mes)

# Creamos la columna fecha agregandole el día 1 de  cada mes
precipitacion1$fecha = as.Date(paste(precipitacion1$anio, precipitacion1$mes, 1, sep = "-"))

# Visualizamos las primeras filas
head(precipitacion1)

# Realizamos la lista de las 197 filas
ids = sort(unique(precipitacion1$ID_POLIGON))

# Pedimos el conteo de las filas, deben de ser 197
length(ids)                           

# Realizamos la estadistica descriptiva de la columna de precipitacion_mm
summary(precipitacion1$precipitacion_mm)

# Realizamos un boxplot por mes de la precipitacion en mm
boxplot(precipitacion_mm ~ mes, 
        data = precipitacion1, 
        main = "Precipitacion por mes (197 parcelas)", 
        xlab = "Mes", 
        ylab = "Precipitación en mm")

# Verficamos los nombres de las columnas
names(precipitacion1)

# Pasaremos nuestra base de datos de formato ancho a formato largo, esto para poder realizar la serie de datos
precipitacion_ancho = pivot_wider(select(precipitacion1, ID_POLIGON, fecha, precipitacion_mm), 
                                  names_from = ID_POLIGON, values_from = precipitacion_mm)

# Ordenaremos las base de datos precipitacion_ancho por fechas
precipitacion_ancho = arrange(precipitacion_ancho, fecha)

# Visualizamos las primeras filas de la base precipitacion_ancho
head(precipitacion_ancho)

# Creamos una matriz de datos, donde le quitaremos la columna de fecha
matriz_precipitacion = as.matrix(precipitacion_ancho[, -1]) 
matriz_precipitacion

#Descargamos la base de datos precipitacion_ancho
write_csv(precipitacion_ancho, "5_Preciptación_Ancho.csv")

# Graficaremos varias series a la vez
matplot(
  precipitacion_ancho$fecha,                         # eje X: las fechas (vector de fechas)
  matriz_precipitacion,                              # eje Y: matriz de precipitaciones (cada columna = una parcela)
  type = "l",                                        # tipo de gráfico
  lty = 1,                                           # tipo de línea1
  col = rgb(0, 0, 1, 0.15),                          # color azul (0,0,1) con transparencia 0.15
  xlab = "Fecha",                                    # etiqueta eje X
  ylab = "mm",                                       # etiqueta eje Y
  main = "Precipitación Mensual de las Parcelas"     # título
)

# Realizamos el análisis de las 197 parcelas

# Creamos una base de datos vacia en donde iremos guardando
resultados_precipitacion = data.frame()

# Creamos un vector vacio en donde iremos guardando todos los residuos de las parcelas
todos_residuos_precipitacion = c()  

# Realizaremos las series de tiempo de las 197 parcelas
for (i in ids){
  
  # Me muestra en la consola que parcela va procesando 
  print(i)    
  serie_de_tiempo_precipitacion = ts(precipitacion_ancho[[i]], start = c(2022,1), frequency = 12)
  
  # Realizaremos las pruebas de estacionariedad
  # Prueba ADF
  adf_precipitacion  = adf.test(serie_de_tiempo_precipitacion)
  # Prueba KPSS
  kpss_precipitacion = kpss.test(serie_de_tiempo_precipitacion, null = "Level")
  # Prueba Phillips -Perron
  pp_precipitacion   = pp.test(serie_de_tiempo_precipitacion)
  
  
  # La serie es estacionaria si las tres pruebas coinciden
  # ADF si p < 0.05 es estacioanria
  # PP si p < 0.05 es estacionaria
  # KPSS si p > 0.05 es estacionaria
  estacionaria_precipitacion = adf_precipitacion$p.value < 0.05 & pp_precipitacion$p.value < 0.05 & kpss_precipitacion$p.value > 0.05
  
  # Modelo de la serie : auto.arima, elige el modelo
  modelo_precipitacion = auto.arima(serie_de_tiempo_precipitacion)
  
  # Se analizaran los residuos; si el modelo es bueno los residuos deben de parecerse a una normal
  # con media 0, varianza constante (aprox.) y sin autocorrelacion
  residuos_precipitacion = residuals(modelo_precipitacion)
  
  # Agregamos los residuos de la parcela al vector con los de todas
  todos_residuos_precipitacion = c(todos_residuos_precipitacion, residuos_precipitacion)
  
  # Realizamos la prueba de Ljung - Box, la cual nos ayuda a ver si quedo autocorrelacion en los residuos del modelo
  # Si p > 0.05 los residuos no tienen patron y el modelo es adecuado
  ljung_box_precipitacion = Box.test(residuals(modelo_precipitacion), 
                                     lag = 12, 
                                     type = "Ljung-Box")
  
  # Para la autocorrelacion se hace una prueba de Ljung-Box, o en pocas palabras un Box.test de los residuos
  # (tambien se puede ver con checkresiduals(modelo))
  # Si p > 0.05 entonces no hay autocorrelacion, es decir, no quedo un patron que el modelo no haya capturado
  ljung_precipitacion = Box.test(residuos_precipitacion, 
                                 lag = 12, 
                                 type = "Ljung-Box")
  
  # Guardamos los resultados de las pruebas previamente realizada
  resultados_precipitacion = rbind(resultados_precipitacion, data.frame(
    ID_POLIGONO                                 = i,                                                  # nombre de la parcela
    No_de_Meses                                 = length(serie_de_tiempo_precipitacion),              # cuantos meses tiene la serie (debe ser 48)
    Promedio_de_mm                              = round(mean(serie_de_tiempo_precipitacion), 1),      # precipitacion promedio, con 1 decimal
    Desviacion_Estandar_de_mm                   = round(sd(serie_de_tiempo_precipitacion), 1),        # desviacion estandar (que tanto varia), con 1 decimal
    ADF_pvalor                                  = round(adf_precipitacion$p.value, 3),                # p-valor de la prueba ADF
    KPSS_pvalor                                 = round(kpss_precipitacion$p.value, 3),               # p-valor de la prueba KPSS
    PP_pvalor                                   = round(pp_precipitacion$p.value, 3),                 # p-valor de la prueba Phillips-Perron
    Estacionaria                                = estacionaria_precipitacion,                         # TRUE si las tres pruebas dicen que es estacionaria
    Modelo                                      = as.character(modelo_precipitacion),                 # modelo elegido por auto.arima
    AIC                                         = round(modelo_precipitacion$aic, 1),                 # AIC del modelo
    BIC                                         = round(modelo_precipitacion$bic, 1),                 # BIC del modelo
    Media_de_Residuos                           = round(mean(residuos_precipitacion), 2),             # promedio de los residuos (debe ser cercano a 0)
    LJung_Box_pvalor                            = round(ljung_precipitacion$p.value, 3)               # p-valor de Ljung-Box
  ))
}

# Ver cuántas filas quedaron
nrow(resultados_precipitacion)

# Conclusiones de cada prueba
# ADF y PP: si p < 0.05 entonces podria ser estacionaria
# KPSS: si p > 0.05 entonces podria ser estacionaria
# Ljung: si p > 0.05 los residuos no tienen autocorrelacion (el modelo es adecuado)
resultados_precipitacion$adf_estacionaria  = resultados_precipitacion$ADF_pvalor  < 0.05
resultados_precipitacion$kpss_estacionaria = resultados_precipitacion$KPSS_pvalor > 0.05
resultados_precipitacion$pp_estacionaria   = resultados_precipitacion$PP_pvalor   < 0.05
resultados_precipitacion$modelo_ok         = resultados_precipitacion$LJung_Box_pvalor > 0.05

# Realizamos los porcentaje de parcelas estacionarias segun cada prueba
round(mean(resultados_precipitacion$adf_estacionaria) * 100, 1)
round(mean(resultados_precipitacion$kpss_estacionaria) * 100, 1)
round(mean(resultados_precipitacion$pp_estacionaria) * 100, 1)

# Realizamos los porcentaje de parcelas estacionarias segun las tres pruebas a la vez
round(mean(resultados_precipitacion$Estacionaria) * 100, 1)

# Parcelas que quedaron con cada modelo
sort(table(resultados_precipitacion$Modelo), decreasing = TRUE)

# Porcentaje de parcelas sin autocorrelacion en los residuos (Ljung-Box)
round(mean(resultados_precipitacion$modelo_ok) * 100, 1)

# Promedio de los residuos en todas las parcelas (deben ser cercanos a 0)
summary(resultados_precipitacion$Media_de_Residuos)

# Realizamos un solo histograma con los residuos de las 197 parcelas juntas
# Observación: Si tiene forma de campana los residuos son aproximadamente normales

hist(todos_residuos_precipitacion,
     breaks = 40,
     main = "Residuos de las 197 parcelas",
     xlab = "Residuo",
     col = "lightblue")
abline(v = 0,
       col = "red",
       lwd = 2)

# Para ver el diagnostico compelto de una parcela
checkresiduals(auto.arima(ts(precipitacion_ancho[["AGC_001"]], start = c(2022, 1), frequency = 12)))

# Guardar como CSV 
write_csv(resultados_precipitacion, "6_Resultados Serie de Tiempo de Precipitacion.csv")


# TEMPERATURA MINIMA ------------------------------------------------------

# Llamamos la base de datos que se extrajo de las imagenes tif de Temperatura Minima
temperatura_minima1 = read.csv("2_Temperatura_Minima_Extraida.csv")

# Visualizamoslas primeras filas de la base de datos de temperatura_minima
head(temperatura_minima1)

# Convertimos la columna mes a tipo númerico
temperatura_minima1$mes = as.numeric(temperatura_minima1$mes)

# Creamos la columna fecha agregandole el 1 de cada mes
temperatura_minima1$fecha = as.Date(paste(temperatura_minima1$anio, temperatura_minima1$mes, 1, sep = "-"))

# Visualizamos las primeras filas
head(temperatura_minima1)

names(temperatura_minima1)

# Realizamos la lista de las 197 filas
id = sort(unique(temperatura_minima1$ID_POLIGON))

# Pedimos el conteo de las filas, deben de ser 197
length(ids)

# Realizamos la estadistica descriptiva de la columna de tmin_c
summary(temperatura_minima1$tmin_c)

#  Realizamos un boxplot por mes de la temperatura en centigrados
boxplot (tmin_c ~ mes,
         data = temperatura_minima1,
         main = "Temperatura Minima por mes (197 parcelas)",
         xlab = "Mes",
         ylab = "Temperatura Minima en c")

# Pasaremos nuestra base de datos de formato ancho a formato largo, esto para poder realizar la serie de datos
temperatura_minima_ancho = pivot_wider(select(temperatura_minima1, ID_POLIGON, fecha, tmin_c),
                                       names_from = ID_POLIGON, values_from = tmin_c)

# Ordenamos la base de datos temperatura_minima_ancho por fechas
temperatura_minima_ancho = arrange(temperatura_minima_ancho, fecha)

# Visualizamos las primeras filas de la base temperatura_minima_ancho
head(temperatura_minima_ancho)

# Creamos una matriz de datos, donde le quitaremos la columna de fecha
matriz_temperatura_minima = as.matrix(temperatura_minima_ancho[, -1])
matriz_temperatura_minima

# Descargamos la base de datos temperatura_maximo_ancho
write.csv(temperatura_minima_ancho, "7_Temperatura_Minima_Ancho.csv")

# Graficaremos varias series a la vez
matplot(
  temperatura_minima_ancho$fecha,                                       # eje X: las fechas (vector de fechas)
  matriz_temperatura_minima,                                            # eje Y: matriz de precipitaciones (cada columna = una parcela)
  type = "l",                                                           # tipo de gráfico
  lty = 1,                                                              # tipo de línea1
  col = rgb(0, 0, 1, 0.15),                                             # color azul (0,0,1) con transparencia 0.15
  xlab = "Fecha",                                                       # etiqueta eje X
  ylab = "c",                                                           # etiqueta eje Y
  main = "Temperatura Minima Mensual de las Parcelas"                   # título
)

# Realizamos el análisis de las 197 parcelas

# Creamos una base de datos vacia en donde iremos guardando
resultados_temperatura_minima = data.frame()

# Creamos un vector vacio en donde iremos guardando todos los residuos de las parcelas
todos_residuos_temperatura_minima = c()  

# Realizaremos las series de tiempo de las 197 parcelas
for (i in ids){
  
  # Me muestra en la consola que parcela va procesando 
  print(i)    
  serie_de_tiempo_temperatura_minima = ts(temperatura_minima_ancho[[i]], start = c(2022,1), frequency = 12)
  
  # Realizaremos las pruebas de estacionariedad
  # Prueba ADF
  adf_temperatura_minima = adf.test(serie_de_tiempo_temperatura_minima)
  # Prueba KPSS
  kpss_temperatura_minima = kpss.test(serie_de_tiempo_temperatura_minima, null = "Level")
  # Prueba Phillips -Perron
  pp_temperatura_minima   = pp.test(serie_de_tiempo_temperatura_minima)
  
  # La serie es estacionaria si las tres pruebas coinciden
  # ADF si p < 0.05 es estacioanria
  # PP si p < 0.05 es estacionaria
  # KPSS si p > 0.05 es estacionaria
  estacionaria_temperatura_minima = adf_temperatura_minima$p.value < 0.05 & pp_temperatura_minima$p.value < 0.05 & kpss_temperatura_minima$p.value > 0.05
  
  # Modelo de la serie : auto.arima, elige el modelo
  modelo_temperatura_minima = auto.arima(serie_de_tiempo_temperatura_minima)
  
  # Se analizaran los residuos; si el modelo es bueno los residuos deben de parecerse a una normal
  # con media 0, varianza constante (aprox.) y sin autocorrelacion
  residuos_temperatura_minima = residuals(modelo_temperatura_minima)
  
  # Agregamos los residuos de la parcela al vector con los de todas
  todos_residuos_temperatura_minima = c(todos_residuos_temperatura_minima, residuos_temperatura_minima)
  
  # Realizamos la prueba de Ljung - Box, la cual nos ayuda a ver si quedo autocorrelacion en los residuos del modelo
  # Si p > 0.05 los residuos no tienen patron y el modelo es adecuado
  ljung_box_temperatura_minima = Box.test(residuals(modelo_temperatura_minima), 
                                          lag = 12, 
                                          type = "Ljung-Box")
  
  # Para la autocorrelacion se hace una prueba de Ljung-Box, o en pocas palabras un Box.test de los residuos
  # (tambien se puede ver con checkresiduals(modelo))
  # Si p > 0.05 entonces no hay autocorrelacion, es decir, no quedo un patron que el modelo no haya capturado
  ljung_temperatura_minima = Box.test(residuos_temperatura_minima, 
                                      lag = 12, 
                                      type = "Ljung-Box")
  
  # Guardamos los resultados de las pruebas previamente realizada
  resultados_temperatura_minima = rbind(resultados_temperatura_minima, data.frame(
    ID_POLIGONO                      = i,                                                     # nombre de la parcela
    No_de_Meses                      = length(serie_de_tiempo_temperatura_minima),            # cuantos meses tiene la serie (debe ser 48)
    Promedio_de_c                    = round(mean(serie_de_tiempo_temperatura_minima), 1),    # temperatura minima promedio, con 1 decimal
    Desviacion_Estandar_de_c         = round(sd(serie_de_tiempo_temperatura_minima), 1),      # desviacion estandar (que tanto varia), con 1 decimal
    ADF_pvalor                       = round(adf_temperatura_minima$p.value, 3),              # p-valor de la prueba ADF
    KPSS_pvalor                      = round(kpss_temperatura_minima$p.value, 3),             # p-valor de la prueba KPSS
    PP_pvalor                        = round(pp_temperatura_minima$p.value, 3),               # p-valor de la prueba Phillips-Perron
    Estacionaria                     = estacionaria_temperatura_minima,                       # TRUE si las tres pruebas dicen que es estacionaria
    Modelo                           = as.character(modelo_temperatura_minima),               # modelo elegido por auto.arima
    AIC                              = round(modelo_temperatura_minima$aic, 1),               # AIC del modelo
    BIC                              = round(modelo_temperatura_minima$bic, 1),               # BIC del modelo
    Media_de_Residuos                = round(mean(residuos_temperatura_minima), 2),           # promedio de los residuos (debe ser cercano a 0)
    LJung_Box_pvalor                 = round(ljung_temperatura_minima$p.value, 3)             # p-valor de Ljung-Box
  ))
}

# Ver cuántas filas quedaron
nrow(resultados_temperatura_minima)

# Conclusiones de cada prueba
# ADF y PP: si p < 0.05 entonces podria ser estacionaria
# KPSS: si p > 0.05 entonces podria ser estacionaria
# Ljung: si p > 0.05 los residuos no tienen autocorrelacion (el modelo es adecuado)
resultados_temperatura_minima$adf_estacionaria  = resultados_temperatura_minima$ADF_pvalor  < 0.05
resultados_temperatura_minima$kpss_estacionaria = resultados_temperatura_minima$KPSS_pvalor > 0.05
resultados_temperatura_minima$pp_estacionaria   = resultados_temperatura_minima$PP_pvalor   < 0.05
resultados_temperatura_minima$modelo_ok         = resultados_temperatura_minima$LJung_Box_pvalor > 0.05

# Realizamos los porcentaje de parcelas estacionarias segun cada prueba
round(mean(resultados_temperatura_minima$adf_estacionaria) * 100, 1)
round(mean(resultados_temperatura_minima$kpss_estacionaria) * 100, 1)
round(mean(resultados_temperatura_minima$pp_estacionaria) * 100, 1)

# Realizamos los porcentaje de parcelas estacionarias segun las tres pruebas a la vez
round(mean(resultados_temperatura_minima$Estacionaria) * 100, 1)

# Parcelas que quedaron con cada modelo
sort(table(resultados_temperatura_minima$Modelo), decreasing = TRUE)

# Porcentaje de parcelas sin autocorrelacion en los residuos (Ljung-Box)
round(mean(resultados_temperatura_minima$modelo_ok) * 100, 1)

# Promedio de los residuos en todas las parcelas (deben ser cercanos a 0)
summary(resultados_temperatura_minima$Media_de_Residuos)

# Realizamos un solo histograma con los residuos de las 197 parcelas juntas
# Observación: Si tiene forma de campana los residuos son aproximadamente normales

hist(todos_residuos_temperatura_minima,
     breaks = 40,
     main = "Residuos de las 197 parcelas",
     xlab = "Residuo",
     col = "lightblue")
abline(v = 0,
       col = "red",
       lwd = 2)

# Para ver el diagnostico completo de una parcela
checkresiduals(auto.arima(ts(temperatura_minima_ancho[["AGC_001"]], start = c(2022, 1), frequency = 12)))

# Guardar como CSV 
write_csv(resultados_temperatura_minima, "8_Resultados Serie de Tiempo de Temperatura Minima.csv")

# TEMPERATURA MÁXIMA ------------------------------------------------------

# Llamamos la base de datos que se extrajo de las imagenes tif de Temperatura Máxima
temperatura_maxima1 = read.csv("3_Temperatura_Maxima_Extraida.csv")

# Visualizamoslas primeras filas de la base de datos de temperatura_maxima1
head(temperatura_maxima1)

# Convertimos la columna mes a tipo númerico
temperatura_maxima1$mes = as.numeric(temperatura_maxima1$mes)

# Creamos la columna fecha agregandole el 1 de cada mes
temperatura_maxima1$fecha = as.Date(paste(temperatura_maxima1$anio, temperatura_maxima1$mes, 1, sep = "-"))

# Visualizamos las primeras filas
head(temperatura_maxima1)

# Realizamos la lista de las 197 filas
id = sort(unique(temperatura_maxima1$ID_POLIGON))

# Pedimos el conteo de las filas, deben de ser 197
length(ids)

# Realizamos la estadistica descriptiva de la columna de tmax_c
summary(temperatura_maxima1$tmax_c)

#  Realizamos un boxplot por mes de la temperatura en centigrados
boxplot (tmax_c ~ mes,
         data = temperatura_maxima1,
         main = "Temperatura Máxima por mes (197 parcelas)",
         xlab = "Mes",
         ylab = "Temperatura Máxima en c")

names(temperatura_maxima1)

# Pasaremos nuestra base de datos de formato ancho a formato largo, esto para poder realizar la serie de datos
temperatura_maxima_ancho = pivot_wider(select(temperatura_maxima1, ID_POLIGON, fecha, tmax_c),
                                       names_from = ID_POLIGON, values_from = tmax_c)

# Ordenamos la base de datos temperatura_maxima_ancho por fechas
temperatura_maxima_ancho = arrange(temperatura_maxima_ancho, fecha)

# Visualizamos las primeras filas de la base temperatura_maxima_ancho
head(temperatura_maxima_ancho)

# Creamos una matriz de datos, donde le quitaremos la columna de fecha
matriz_temperatura_maxima = as.matrix(temperatura_maxima_ancho[, -1])
matriz_temperatura_maxima

# Descargamos la base de datos temperatura_maximo_ancho
write.csv(temperatura_maxima_ancho, "9_Temperatura_Maxima_Ancho.csv")

# Graficaremos varias series a la vez
matplot(
  temperatura_maxima_ancho$fecha,                                       # eje X: las fechas (vector de fechas)
  matriz_temperatura_maxima,                                            # eje Y: matriz de precipitaciones (cada columna = una parcela)
  type = "l",                                                           # tipo de gráfico
  lty = 1,                                                              # tipo de línea1
  col = rgb(0, 0, 1, 0.15),                                             # color azul (0,0,1) con transparencia 0.15
  xlab = "Fecha",                                                       # etiqueta eje X
  ylab = "c",                                                           # etiqueta eje Y
  main = "Temperatura Máxima Mensual de las Parcelas"                   # título
)

# Realizamos el análisis de las 197 parcelas

# Creamos una base de datos vacia en donde iremos guardando
resultados_temperatura_maxima = data.frame()

# Creamos un vector vacio en donde iremos guardando todos los residuos de las parcelas
todos_residuos_temperatura_maxima = c()  

# Realizaremos las series de tiempo de las 197 parcelas
for (i in ids){
  
  # Me muestra en la consola que parcela va procesando 
  print(i)    
  serie_de_tiempo_temperatura_maxima = ts(temperatura_maxima_ancho[[i]], start = c(2022,1), frequency = 12)
  
  # Realizaremos las pruebas de estacionariedad
  # Prueba ADF
  adf_temperatura_maxima  = adf.test(serie_de_tiempo_temperatura_maxima)
  # Prueba KPSS
  kpss_temperatura_maxima = kpss.test(serie_de_tiempo_temperatura_maxima, null = "Level")
  # Prueba Phillips -Perron
  pp_temperatura_maxima   = pp.test(serie_de_tiempo_temperatura_maxima)
  
  # La serie es estacionaria si las tres pruebas coinciden
  # ADF si p < 0.05 es estacioanria
  # PP si p < 0.05 es estacionaria
  # KPSS si p > 0.05 es estacionaria
  estacionaria_temperatura_maxima = adf_temperatura_maxima$p.value < 0.05 & pp_temperatura_maxima$p.value < 0.05 & kpss_temperatura_maxima$p.value > 0.05
  
  # Modelo de la serie : auto.arima, elige el modelo
  modelo_temperatura_maxima = auto.arima(serie_de_tiempo_temperatura_maxima)
  
  # Se analizaran los residuos; si el modelo es bueno los residuos deben de parecerse a una normal
  # con media 0, varianza constante (aprox.) y sin autocorrelacion
  residuos_temperatura_maxima = residuals(modelo_temperatura_maxima)
  
  # Agregamos los residuos de la parcela al vector con los de todas
  todos_residuos_temperatura_maxima = c(todos_residuos_temperatura_maxima, residuos_temperatura_maxima)
  
  # Realizamos la prueba de Ljung - Box, la cual nos ayuda a ver si quedo autocorrelacion en los residuos del modelo
  # Si p > 0.05 los residuos no tienen patron y el modelo es adecuado
  ljung_box_temperatura_maxima = Box.test(residuals(modelo_temperatura_maxima), 
                                          lag = 12, 
                                          type = "Ljung-Box")
  
  # Para la autocorrelacion se hace una prueba de Ljung-Box, o en pocas palabras un Box.test de los residuos
  # (tambien se puede ver con checkresiduals(modelo))
  # Si p > 0.05 entonces no hay autocorrelacion, es decir, no quedo un patron que el modelo no haya capturado
  ljung_temperatura_maxima = Box.test(residuos_temperatura_maxima, 
                                      lag = 12, 
                                      type = "Ljung-Box")
  
  # Guardamos los resultados de las pruebas previamente realizada
  resultados_temperatura_maxima     = rbind(resultados_temperatura_maxima, data.frame(
    ID_POLIGONO                      = i,                                                     # nombre de la parcela
    No_de_Meses                      = length(serie_de_tiempo_temperatura_maxima),            # cuantos meses tiene la serie (debe ser 48)
    Promedio_de_c                    = round(mean(serie_de_tiempo_temperatura_maxima), 1),    # temperatura maxima promedio, con 1 decimal
    Desviacion_Estandar_de_c         = round(sd(serie_de_tiempo_temperatura_maxima), 1),      # desviacion estandar (que tanto varia), con 1 decimal
    ADF_pvalor                       = round(adf_temperatura_maxima$p.value, 3),              # p-valor de la prueba ADF
    KPSS_pvalor                      = round(kpss_temperatura_maxima$p.value, 3),             # p-valor de la prueba KPSS
    PP_pvalor                        = round(pp_temperatura_maxima$p.value, 3),               # p-valor de la prueba Phillips-Perron
    Estacionaria                     = estacionaria_temperatura_maxima,                       # TRUE si las tres pruebas dicen que es estacionaria
    Modelo                           = as.character(modelo_temperatura_maxima),               # modelo elegido por auto.arima
    AIC                              = round(modelo_temperatura_maxima$aic, 1),               # AIC del modelo
    BIC                              = round(modelo_temperatura_maxima$bic, 1),               # BIC del modelo
    Media_de_Residuos                = round(mean(residuos_temperatura_maxima), 2),           # promedio de los residuos (debe ser cercano a 0)
    LJung_Box_pvalor                 = round(ljung_temperatura_maxima$p.value, 3)             # p-valor de Ljung-Box
  ))
}

# Ver cuántas filas quedaron
nrow(resultados_temperatura_maxima)

# Conclusiones de cada prueba
# ADF y PP: si p < 0.05 entonces podria ser estacionaria
# KPSS: si p > 0.05 entonces podria ser estacionaria
# Ljung: si p > 0.05 los residuos no tienen autocorrelacion (el modelo es adecuado)
resultados_temperatura_maxima$adf_estacionaria  = resultados_temperatura_maxima$ADF_pvalor  < 0.05
resultados_temperatura_maxima$kpss_estacionaria = resultados_temperatura_maxima$KPSS_pvalor > 0.05
resultados_temperatura_maxima$pp_estacionaria   = resultados_temperatura_maxima$PP_pvalor   < 0.05
resultados_temperatura_maxima$modelo_ok         = resultados_temperatura_maxima$LJung_Box_pvalor > 0.05

# Realizamos los porcentaje de parcelas estacionarias segun cada prueba
round(mean(resultados_temperatura_maxima$adf_estacionaria) * 100, 1)
round(mean(resultados_temperatura_maxima$kpss_estacionaria) * 100, 1)
round(mean(resultados_temperatura_maxima$pp_estacionaria) * 100, 1)

# Realizamos los porcentaje de parcelas estacionarias segun las tres pruebas a la vez
round(mean(resultados_temperatura_maxima$Estacionaria) * 100, 1)

# Parcelas que quedaron con cada modelo
sort(table(resultados_temperatura_maxima$Modelo), decreasing = TRUE)

# Porcentaje de parcelas sin autocorrelacion en los residuos (Ljung-Box)
round(mean(resultados_temperatura_maxima$modelo_ok) * 100, 1)

# Promedio de los residuos en todas las parcelas (deben ser cercanos a 0)
summary(resultados_temperatura_maxima$Media_de_Residuos)

# Realizamos un solo histograma con los residuos de las 197 parcelas juntas
# Observación: Si tiene forma de campana los residuos son aproximadamente normales

hist(todos_residuos_temperatura_maxima,
     breaks = 40,
     main = "Residuos de las 197 parcelas",
     xlab = "Residuo",
     col = "lightblue")
abline(v = 0,
       col = "red",
       lwd = 2)

# Para ver el diagnostico completo de una parcela
checkresiduals(auto.arima(ts(temperatura_maxima_ancho[["AGC_001"]], start = c(2022, 1), frequency = 12)))

# Guardar como CSV 
write_csv(resultados_temperatura_maxima, "10_Resultados Serie de Tiempo de Temperatura Maxima.csv")
