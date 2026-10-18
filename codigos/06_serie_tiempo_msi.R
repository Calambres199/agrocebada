# Cargamos las librerias a utilizar
library(dplyr)
library(tidyr)
library(zoo)
library(readr)
library(tseries)
library(forecast)

# Le digo a R en que carpeta voy a estar trabajando
setwd('/Users/rebec/Downloads/DataSet/DATASET_RETO_AGRO_2026')

# Visualizo que tiene la carpeta en la que voy a trabajar
list.files()

# Trabajaremos con la base de datos básica filtrada
base_basica1 = read.csv("12_Base_Basica_Filtrada.csv")

head(base_basica1)
names(base_basica1)
length(unique(base_basica1$ID_POLIGONO))            # Debe de dar 197
table(base_basica1$sensor)                          # Debe de salir sentinel - 2

# Seleccionamos el MSI
MSI_sentinel = select(base_basica1,
                      ID_POLIGONO,
                      fecha_captura,
                      anio,
                      mes,
                      msi_promedio)

# Nos aseguramos de que el mes sea de tipo numerico
MSI_sentinel$mes = as.numeric(MSI_sentinel$mes)

# Lista de las 197 parcelas
ids = sort(unique(MSI_sentinel$ID_POLIGONO))
length(ids)             # Debe de dar 197

# Realizamos una estadistica descriptiva de los datos del EVBI
summary(MSI_sentinel$msi_promedio)

# Realizamos un diagrama de caja por mes para observar los valores atipicos del MSI en cada mes
boxplot(msi_promedio ~ mes,
        data = MSI_sentinel,
        main = "MSI por mes (197 parcelas)", 
        xlab = "Mes", 
        ylab = "MSI")

# Vamos a regularizar la serie
# Donde habrá un dato por mes y sin huecos
# En caso de que en algun mes haya varias imágenes, le sacaremos la mediana
# En este caso la mediana se vera menos afectada por los valores atipidos
msi_mensual = summarise(group_by(MSI_sentinel, 
                                 ID_POLIGONO, 
                                 anio, mes),
                        msi = median(msi_promedio), 
                        .groups = "drop")

# Los meses que no cuenten con imagen van a aparecer como NA
msi_mensual = complete(msi_mensual, 
                       ID_POLIGONO, 
                       anio = 2022:2025, 
                       mes = 1:12)

# Creamos la columna fecha con el día 1 de cada mes
msi_mensual$fecha = as.Date(paste(msi_mensual$anio, 
                                  msi_mensual$mes, 
                                  1, 
                                  sep = "-"))

# Vamos a realizar el cambio de columna a filas 848 x 197)
msi_ancho = pivot_wider(select(msi_mensual, 
                               ID_POLIGONO, 
                               fecha, 
                               msi),
                        names_from = ID_POLIGONO, 
                        values_from = msi)

# Ordenamos las fechas de la base
msi_ancho = arrange(msi_ancho, 
                    fecha)

head(msi_ancho)
names(msi_ancho)
dim(msi_ancho)

# Guardamos la base ancha con los meses vacios (NA), antes de rellenar
write_csv(msi_ancho, "17_MSI_Ancho_sin_Rellenar.csv")

# Verificaremos que sea regulr
# Que no haya meses vacios (NA)
vacios1 =colSums(is.na(msi_ancho[, -1]))

# Cantidad de pareclas que tiene 0, 1, 2,... meses
table(vacios1)

# Parcelas regulares
sum(vacios1 == 0)

# Parcelas no regulares
sum(vacios1 > 0)

# Rellenaremos cada mes vacio con una linea recta entre el mes anterior y el siguiente
msi_ancho[, -1] = lapply(msi_ancho[, -1], 
                         na.approx, 
                         rule = 2)

sum(is.na(msi_ancho[, -1]))           # debe dar 0

# Guardamos la base MSI ancho ya rellena
write_csv(msi_ancho, "18_MSI_Ancho_Relleno.csv")

# Análisis de las 197 series de tiempo ------------------------------------

# Creamos una matriz de datos, donde le quitaremos la columna de fecha
matriz_MSI = as.matrix(msi_ancho[, -1])
matriz_MSI

# Graficaremos varias series a la vez
matplot(
  msi_ancho$fecha,                                   # eje X las fechas (vector de fechas)
  matriz_MSI,                                        # eje Y matriz de precipitaciones (cada columna = una parcela)
  type = "l",                                        # tipo de gráfico
  lty = 1,                                           # tipo de línea1
  col = rgb(0, 0, 1, 0.15),                        # color azul (0,0,1) con transparencia 0.15
  xlab = "Fecha",                                    # etiqueta eje X
  ylab = "MSI",                                      # etiqueta eje Y
  main = "MSI Mensual de las Parcelas"               # título
)

# Realizamos el análisis de las 197 parcelas

# Creamos una base de datos vacia en donde iremos guardando
resultados_MSI = data.frame()

# Creamos un vector vacio en donde iremos guardando todos los residuos de las parcelas
todos_residuos_MSI = c()

# Realizaremos las series de tiempo de las 197 parcelas
for (i in ids){
  
  # Me muestra en la consola que parcela va procesando 
  print(i)    
  serie_de_tiempo_MSI = ts(msi_ancho[[i]], 
                           start = c(2022,1), 
                           frequency = 12)
  # Realizaremos las pruebas de estacionariedad
  # Prueba ADF
  adf_MSI  = adf.test(serie_de_tiempo_MSI)
  # Prueba KPSS
  kpss_MSI = kpss.test(serie_de_tiempo_MSI, null = "Level")
  # Prueba Phillips -Perron
  pp_MSI   = pp.test(serie_de_tiempo_MSI)
  
  # La serie es estacionaria si las tres pruebas coinciden
  # ADF si p < 0.05 es estacioanria
  # PP si p < 0.05 es estacionaria
  # KPSS si p > 0.05 es estacionaria
  estacionaria_MSI = adf_MSI$p.value < 0.05 & pp_MSI$p.value < 0.05 & kpss_MSI$p.value > 0.05
  
  # Modelo de la serie  auto.arima, elige el modelo
  modelo_MSI = auto.arima(serie_de_tiempo_MSI)
  
  # Se analizaran los residuos; si el modelo es bueno los residuos deben de parecerse a una normal
  # con media 0, varianza constante (aprox.) y sin autocorrelacion
  residuos_MSI = residuals(modelo_MSI)
  
  # Agregamos los residuos de la parcela al vector con los de todas
  todos_residuos_MSI = c(todos_residuos_MSI, residuos_MSI)
  
  # Realizamos la prueba de Ljung - Box, la cual nos ayuda a ver si quedo autocorrelacion en los residuos del modelo
  # Si p  0.05 los residuos no tienen patron y el modelo es adecuado
  ljung_box_MSI = Box.test(residuals(modelo_MSI), 
                           lag = 12, 
                           type = "Ljung-Box")
  
  # Para la autocorrelacion se hace una prueba de Ljung-Box, o en pocas palabras un Box.test de los residuos
  # (tambien se puede ver con checkresiduals(modelo))
  # Si p  0.05 entonces no hay autocorrelacion, es decir, no quedo un patron que el modelo no haya capturado
  ljung_MSI = Box.test(residuos_MSI, 
                       lag = 12, 
                       type = "Ljung-Box")
  
  # Guardamos los resultados de las pruebas previamente realizada
  resultados_MSI = rbind(resultados_MSI, data.frame(
    ID_POLIGONO                                 = i,                                                  # nombre de la parcela
    No_de_Meses                                 = length(serie_de_tiempo_MSI),                        # cuantos meses tiene la serie (debe ser 48)
    Promedio_de_MSI                             = round(mean(serie_de_tiempo_MSI), 3),                # MSI promedio, con 3 decimal
    Desviacion_Estandar_de_MSI                  = round(sd(serie_de_tiempo_MSI), 3),                  # desviacion estandar (que tanto varia), con 3 decimal
    ADF_pvalor                                  = round(adf_MSI$p.value, 3),                          # p-valor de la prueba ADF
    KPSS_pvalor                                 = round(kpss_MSI$p.value, 3),                         # p-valor de la prueba KPSS
    PP_pvalor                                   = round(pp_MSI$p.value, 3),                           # p-valor de la prueba Phillips-Perron
    Estacionaria                                = estacionaria_MSI,                                   # TRUE si las tres pruebas dicen que es estacionaria
    Modelo                                      = as.character(modelo_MSI),                           # modelo elegido por auto.arima
    AIC                                         = round(modelo_MSI$aic, 1),                           # AIC del modelo
    BIC                                         = round(modelo_MSI$bic, 1),                           # BIC del modelo
    Media_de_Residuos                           = round(mean(residuos_MSI), 2),                       # promedio de los residuos (debe ser cercano a 0)
    LJung_Box_pvalor                            = round(ljung_MSI$p.value, 3)                         # p-valor de Ljung-Box
  ))
}

# Ver cuántas filas quedaron
nrow(resultados_MSI)

# Conclusiones de cada prueba
# ADF y PP si p  0.05 entonces podria ser estacionaria
# KPSS si p  0.05 entonces podria ser estacionaria
# Ljung si p  0.05 los residuos no tienen autocorrelacion (el modelo es adecuado)
resultados_MSI$adf_estacionaria  = resultados_MSI$ADF_pvalor  < 0.05
resultados_MSI$kpss_estacionaria = resultados_MSI$KPSS_pvalor > 0.05
resultados_MSI$pp_estacionaria   = resultados_MSI$PP_pvalor   < 0.05
resultados_MSI$modelo_ok         = resultados_MSI$LJung_Box_pvalor > 0.05

# Realizamos los porcentaje de parcelas estacionarias segun cada prueba
round(mean(resultados_MSI$adf_estacionaria) * 100, 1)
round(mean(resultados_MSI$kpss_estacionaria) * 100, 1)
round(mean(resultados_MSI$pp_estacionaria) * 100, 1)

# Realizamos los porcentaje de parcelas estacionarias segun las tres pruebas a la vez
round(mean(resultados_MSI$Estacionaria) * 100, 1)

# Parcelas que quedaron con cada modelo
sort(table(resultados_MSI$Modelo), decreasing = TRUE)

# Porcentaje de parcelas sin autocorrelacion en los residuos (Ljung-Box)
round(mean(resultados_MSI$modelo_ok) * 100, 1)

# Promedio de los residuos en todas las parcelas (deben ser cercanos a 0)
summary(resultados_MSI$Media_de_Residuos)

# Realizamos un solo histograma con los residuos de las 197 parcelas juntas
# Observación Si tiene forma de campana los residuos son aproximadamente normales

hist(todos_residuos_MSI,
     breaks = 40,
     main = "Residuos de las 197 parcelas (MSI)",
     xlab = "Residuo",
     col = "lightblue")
abline(v = 0,
       col = "red",
       lwd = 2)

# Para ver el diagnostico completo de una parcela
checkresiduals(auto.arima(ts(msi_ancho[["AGC_001"]], start = c(2022, 1), frequency = 12)))

# Guardar como CSV 
write_csv(resultados_MSI, "19_Resultados Serie de Tiempo de MSI.csv")