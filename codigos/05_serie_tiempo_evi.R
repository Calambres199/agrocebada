# Cargamos las librerias a utilizar
library(dplyr)      # select, group_by, summarise y arrange
library(tidyr)      # complete y pivot_wider
library(zoo)        # na.approx (rellenar meses vacios)
library(readr)      # write_csv
library(tseries)    # adf.test, kpss.test y pp.test
library(forecast)   # auto.arima y checkresiduals

# Le digo a R en que carpeta voy a estar trabajando
setwd('/Users/rebec/Downloads/DataSet/DATASET_RETO_AGRO_2026')

# Visualizo que tiene la carpeta en la que voy a trabajar
list.files()

# Trabajaremos con la base de datos básica filtrada
base_basica = read.csv("12_Base_Basica_Filtrada.csv")

head(base_basica)
names(base_basica)
length(unique(base_basica$ID_POLIGONO))            # Debe de dar 197
table(base_basica$sensor)                          # Debe de salir sentinel - 2

# Seleccionaremos solo el EVI
evi = select(base_basica,
             ID_POLIGONO,
             fecha_captura,
             anio,
             mes,
             evi_promedio)

# Nos aseguramos de que el mes sea de tipo numerico
evi$mes = as.numeric(evi$mes)

# Lista de las 197 parcelas
ids = sort(unique(evi$ID_POLIGONO))
length(ids)             # Debe de dar 197

# Realizamos una estadistica descriptiva de los datos del EVBI
summary(evi$evi_promedio)

# Realizamos un diagrama de caja por mes
# Se pretende comparar valores con los del correspondiente mes y observar los valores atipicos por mes
boxplot(evi_promedio ~ mes,
        data = evi,
        col = "palegreen",
        main = "EVI por mes de las parcelas",
        xlab = "Mes",
        ylab = "EVI")
# Vamos a regularizar la serie
# Donde habrá un dato por mes y sin huecos
# En caso de que en algun mes haya varias imágenes, le sacaremos la mediana
# En este caso la mediana se vera menos afectada por los valores atipidos
evi_mensual = summarise(group_by(evi,
                                 ID_POLIGONO,
                                 anio,
                                 mes),
                        evi = median(evi_promedio),
                        .groups = "drop")

# Los meses que no cuenten con imagen van a aparecer como NA 
evi_mensual = complete(evi_mensual,
                       ID_POLIGONO,
                       anio = 2022:2025,
                       mes = 1:12)

# Creamos la columna fecha con el día 1 de cada mes 
evi_mensual$fecha = as.Date(paste(evi_mensual$anio,
                                  evi_mensual$mes,
                                  1,
                                  sep = "-"))

# Vamos a realizar el cambio de columnas a filas 
evi_ancho = pivot_wider(select(evi_mensual,
                               ID_POLIGONO,
                               fecha,
                               evi),
                        names_from = ID_POLIGONO,
                        values_from = evi)

# Ordenamos las fechas de la base
evi_ancho = arrange(evi_ancho,
                    fecha)

head(evi_ancho)
names(evi_ancho)
dim(evi_ancho)

# Guardamos la base ancha con los meses vacios (NA), antes de rellenar
write_csv(evi_ancho, "14_EVI_Ancho_sin_Rellenar.csv")

# Verificaremos que sea regulr
# Que no haya meses vacios (NA)
vacios =colSums(is.na(evi_ancho[, -1]))

# Cantidad de pareclas que tiene 0, 1, 2,... meses
table(vacios)

# Parcelas regulares
sum(vacios == 0)

# Parcelas no regulares
sum(vacios >0)

# Rellenaremos cada mes vacio con una linea recta entre el mes anterior y el siguiente
evi_ancho[, -1] =lapply(evi_ancho[, -1],
                        na.approx,
                        rule =2)

sum(is.na(evi_ancho[, -1]))   # Debe de dar 0

# Guardamos la base evi ancho ya rellena
write_csv(evi_ancho, "15_EVI_Ancho_Rellena.csv")

# Análisis de las 197 series de tiempo ------------------------------------

# Creamos una matriz de datos, donde le quitaremos la columna de fecha
matriz_EVI = as.matrix(evi_ancho[, -1])
matriz_EVI

# Graficaremos varias series a la vez
matplot(
  evi_ancho$fecha,                                   # eje X: las fechas (vector de fechas)
  matriz_EVI,                                        # eje Y: matriz de precipitaciones (cada columna = una parcela)
  type = "l",                                        # tipo de gráfico
  lty = 1,                                           # tipo de línea1
  col = rgb(0, 0.5, 0, 0.15),                        # color verde
  xlab = "Fecha",                                    # etiqueta eje X
  ylab = "EVI",                                      # etiqueta eje Y
  main = "EVI Mensual de las Parcelas"               # título
)

# Realizamos el análisis de las 197 parcelas

# Creamos una base de datos vacia en donde iremos guardando
resultados_EVI = data.frame()

# Creamos un vector vacio en donde iremos guardando todos los residuos de las parcelas
todos_residuos_EVI = c()  

# Realizaremos las series de tiempo de las 197 parcelas
for (i in ids){
  
  # Me muestra en la consola que parcela va procesando 
  print(i)    
  serie_de_tiempo_EVI = ts(evi_ancho[[i]], 
                           start = c(2022,1), 
                           frequency = 12)
  
  # Realizaremos las pruebas de estacionariedad
  # Prueba ADF
  adf_EVI  = adf.test(serie_de_tiempo_EVI)
  # Prueba KPSS
  kpss_EVI = kpss.test(serie_de_tiempo_EVI, null = "Level")
  # Prueba Phillips -Perron
  pp_EVI   = pp.test(serie_de_tiempo_EVI)
  
  
  # La serie es estacionaria si las tres pruebas coinciden
  # ADF si p < 0.05 es estacioanria
  # PP si p < 0.05 es estacionaria
  # KPSS si p > 0.05 es estacionaria
  estacionaria_EVI = adf_EVI$p.value < 0.05 & pp_EVI$p.value < 0.05 & kpss_EVI$p.value > 0.05
  
  # Modelo de la serie : auto.arima, elige el modelo
  modelo_EVI = auto.arima(serie_de_tiempo_EVI)
  
  # Se analizaran los residuos; si el modelo es bueno los residuos deben de parecerse a una normal
  # con media 0, varianza constante (aprox.) y sin autocorrelacion
  residuos_EVI = residuals(modelo_EVI)
  
  # Agregamos los residuos de la parcela al vector con los de todas
  todos_residuos_EVI = c(todos_residuos_EVI, residuos_EVI)
  
  # Realizamos la prueba de Ljung - Box, la cual nos ayuda a ver si quedo autocorrelacion en los residuos del modelo
  # Si p > 0.05 los residuos no tienen patron y el modelo es adecuado
  ljung_box_EVI = Box.test(residuals(modelo_EVI), 
                           lag = 12, 
                           type = "Ljung-Box")
  
  # Para la autocorrelacion se hace una prueba de Ljung-Box, o en pocas palabras un Box.test de los residuos
  # (tambien se puede ver con checkresiduals(modelo))
  # Si p > 0.05 entonces no hay autocorrelacion, es decir, no quedo un patron que el modelo no haya capturado
  ljung_EVI = Box.test(residuos_EVI, 
                       lag = 12, 
                       type = "Ljung-Box")
  
  # Guardamos los resultados de las pruebas previamente realizada
  resultados_EVI = rbind(resultados_EVI, data.frame(
    ID_POLIGONO                                 = i,                                                  # nombre de la parcela
    No_de_Meses                                 = length(serie_de_tiempo_EVI),                        # cuantos meses tiene la serie (debe ser 48)
    Promedio_EVI                              = round(mean(serie_de_tiempo_EVI), 3),                # EVI promedio, con 3 decimal
    Desviacion_Estandar_EVI                   = round(sd(serie_de_tiempo_EVI), 3),                  # desviacion estandar (que tanto varia), con 3 decimal
    ADF_pvalor                                  = round(adf_EVI$p.value, 3),                          # p-valor de la prueba ADF
    KPSS_pvalor                                 = round(kpss_EVI$p.value, 3),                         # p-valor de la prueba KPSS
    PP_pvalor                                   = round(pp_EVI$p.value, 3),                           # p-valor de la prueba Phillips-Perron
    Estacionaria                                = estacionaria_EVI,                                   # TRUE si las tres pruebas dicen que es estacionaria
    Modelo                                      = as.character(modelo_EVI),                           # modelo elegido por auto.arima
    AIC                                         = round(modelo_EVI$aic, 3),                           # AIC del modelo
    BIC                                         = round(modelo_EVI$bic, 3),                           # BIC del modelo
    Media_de_Residuos                           = round(mean(residuos_EVI), 3),                       # promedio de los residuos (debe ser cercano a 0)
    LJung_Box_pvalor                            = round(ljung_EVI$p.value, 3)                         # p-valor de Ljung-Box
  ))
}

# Ver cuántas filas quedaron
nrow(resultados_EVI)

# Conclusiones de cada prueba
# ADF y PP: si p < 0.05 entonces podria ser estacionaria
# KPSS: si p > 0.05 entonces podria ser estacionaria
# Ljung: si p > 0.05 los residuos no tienen autocorrelacion (el modelo es adecuado)
resultados_EVI$adf_estacionaria = resultados_EVI$ADF_pvalor  < 0.05
resultados_EVI$kpss_estacionaria = resultados_EVI$KPSS_pvalor > 0.05
resultados_EVI$pp_estacionaria   = resultados_EVI$PP_pvalor   < 0.05
resultados_EVI$modelo_ok         = resultados_EVI$LJung_Box_pvalor > 0.05

# Realizamos los porcentaje de parcelas estacionarias segun cada prueba
round(mean(resultados_EVI$adf_estacionaria) * 100, 1)
round(mean(resultados_EVI$kpss_estacionaria) * 100, 1)
round(mean(resultados_EVI$pp_estacionaria) * 100, 1)

# Realizamos los porcentaje de parcelas estacionarias segun las tres pruebas a la vez
round(mean(resultados_EVI$Estacionaria) * 100, 1)

# Parcelas que quedaron con cada modelo
sort(table(resultados_EVI$Modelo), decreasing = TRUE)

# Porcentaje de parcelas sin autocorrelacion en los residuos (Ljung-Box)
round(mean(resultados_EVI$modelo_ok) * 100, 1)

# Promedio de los residuos en todas las parcelas (deben ser cercanos a 0)
summary(resultados_EVI$Media_de_Residuos)

# Realizamos un solo histograma con los residuos de las 197 parcelas juntas
# Observación: Si tiene forma de campana los residuos son aproximadamente normales

hist(todos_residuos_EVI,
     breaks = 40,
     main = "Residuos de las 197 parcelas (EVI)",
     xlab = "Residuo",
     col = "lightblue")
abline(v = 0,
       col = "red",
       lwd = 2)

# Para ver el diagnostico compelto de una parcela
checkresiduals(auto.arima(ts(evi_ancho[["AGC_001"]], start = c(2022, 1), frequency = 12)))

# Guardar como CSV 
write_csv(resultados_EVI, "16_Resultados Serie de Tiempo de EVI.csv")