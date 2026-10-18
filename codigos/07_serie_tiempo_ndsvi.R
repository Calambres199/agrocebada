
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

# Trabajaremos con la base de datos básica filtrada (Asegúrate de que este sea el nombre exacto)
base_basica2 = read.csv("12_Base_Basica_Filtrada.csv")

head(base_basica2)
names(base_basica2)
length(unique(base_basica2$ID_POLIGONO))            # Debe dar 197
table(base_basica2$sensor)                          # Debe salir Sentinel-2

# Seleccionamos el NDSVI
NDSVI_sentinel = select(base_basica2,
                        ID_POLIGONO,
                        fecha_captura,
                        anio,
                        mes,
                        ndsvi_promedio)

names(NDSVI_sentinel)

# Nos aseguramos de que el mes sea de tipo numerico
NDSVI_sentinel$mes = as.numeric(NDSVI_sentinel$mes)

# Lista de las 197 parcelas
ids = sort(unique(NDSVI_sentinel$ID_POLIGONO))
length(ids)             # Debe dar 197

# Realizamos una estadistica descriptiva de los datos del NDSVI
summary(NDSVI_sentinel$ndsvi_promedio)

# Realizamos un diagrama de caja por mes para observar los valores atipicos del NDSVI en cada mes
boxplot(ndsvi_promedio ~ mes,
        data = NDSVI_sentinel,
        main = "NDSVI por mes (197 parcelas)", 
        xlab = "Mes", 
        ylab = "NDSVI")

# Vamos a regularizar la serie
# Donde habrá un dato por mes y sin huecos
# En caso de que en algun mes haya varias imágenes, le sacaremos la mediana
# En este caso la mediana se vera menos afectada por los valores atipicos
ndsvi_mensual = summarise(group_by(NDSVI_sentinel, 
                                   ID_POLIGONO, 
                                   anio, 
                                   mes),
                          ndsvi = median(ndsvi_promedio), 
                          .groups = "drop")

# Los meses que no cuenten con imagen van a aparecer como NA 
ndsvi_mensual = complete(ndsvi_mensual,
                         ID_POLIGONO,
                         anio = 2022:2025,
                         mes = 1:12)

# Creamos la columna fecha con el día 1 de cada mes 
ndsvi_mensual$fecha = as.Date(paste(ndsvi_mensual$anio,
                                    ndsvi_mensual$mes,
                                    1,
                                    sep = "-"))

# Vamos a realizar el cambio de columnas a filas 
ndsvi_ancho = pivot_wider(select(ndsvi_mensual,
                                 ID_POLIGONO,
                                 fecha,
                                 ndsvi),
                          names_from = ID_POLIGONO,
                          values_from = ndsvi)

# Ordenamos las fechas de la base
ndsvi_ancho = arrange(ndsvi_ancho,
                      fecha)

head(ndsvi_ancho)
names(ndsvi_ancho)
dim(ndsvi_ancho)

# Guardamos la base ancha con los meses vacios (NA), antes de rellenar
write_csv(ndsvi_ancho, "20_NDSVI_Ancho_sin_Rellenar.csv")

# Verificaremos que sea regular
# Que no haya meses vacios (NA)
vacios = colSums(is.na(ndsvi_ancho[, -1]))

# Cantidad de parcelas que tiene 0, 1, 2,... meses vacios
table(vacios)

# Parcelas regulares
sum(vacios == 0)

# Parcelas no regulares
sum(vacios > 0)

# Rellenaremos cada mes vacio con una linea recta entre el mes anterior y el siguiente
ndsvi_ancho[, -1] = lapply(ndsvi_ancho[, -1],
                           na.approx,
                           rule = 2)

sum(is.na(ndsvi_ancho[, -1]))   # Debe dar 0

# Guardamos la base NDSVI ancho ya rellena
write_csv(ndsvi_ancho, "21_NDSVI_Ancho_Rellena.csv")

# Análisis de las 197 series de tiempo ------------------------------------

# Creamos una matriz de datos, donde le quitaremos la columna de fecha
matriz_NDSVI = as.matrix(ndsvi_ancho[, -1])
matriz_NDSVI

# Graficaremos varias series a la vez
matplot(
  ndsvi_ancho$fecha,                                 # eje X: las fechas (vector de fechas)
  matriz_NDSVI,                                      # eje Y: matriz (cada columna = una parcela)
  type = "l",                                        # tipo de gráfico
  lty = 1,                                           # tipo de línea
  col = rgb(0.6, 0.3, 0, 0.15),                        # color verde transparente
  xlab = "Fecha",                                    # etiqueta eje X
  ylab = "NDSVI",                                    # etiqueta eje Y
  main = "NDSVI Mensual de las Parcelas"             # título
)

# Realizamos el análisis de las 197 parcelas

# Creamos una base de datos vacia en donde iremos guardando
resultados_NDSVI = data.frame()

# Creamos un vector vacio en donde iremos guardando todos los residuos de las parcelas
todos_residuos_NDSVI = c()  

# Realizaremos las series de tiempo de las 197 parcelas
for (i in ids){
  
  # Me muestra en la consola que parcela va procesando 
  print(i)    
  serie_de_tiempo_NDSVI = ts(ndsvi_ancho[[i]], 
                             start = c(2022,1), 
                             frequency = 12)
  
  # Realizaremos las pruebas de estacionariedad
  # Prueba ADF
  adf_NDSVI  = adf.test(serie_de_tiempo_NDSVI)
  # Prueba KPSS
  kpss_NDSVI = kpss.test(serie_de_tiempo_NDSVI, null = "Level")
  # Prueba Phillips-Perron
  pp_NDSVI   = pp.test(serie_de_tiempo_NDSVI)
  
  # La serie es estacionaria si las tres pruebas coinciden
  estacionaria_NDSVI = adf_NDSVI$p.value < 0.05 & pp_NDSVI$p.value < 0.05 & kpss_NDSVI$p.value > 0.05
  
  # Modelo de la serie : auto.arima, elige el modelo
  modelo_NDSVI = auto.arima(serie_de_tiempo_NDSVI)
  
  # Se analizaran los residuos
  residuos_NDSVI = residuals(modelo_NDSVI)
  
  # Agregamos los residuos de la parcela al vector con los de todas
  todos_residuos_NDSVI = c(todos_residuos_NDSVI, residuos_NDSVI)
  
  # Prueba de Ljung-Box para ver si quedó autocorrelación
  ljung_NDSVI = Box.test(residuos_NDSVI, 
                         lag = 12, 
                         type = "Ljung-Box")
  
  # Guardamos los resultados de las pruebas previamente realizadas
  resultados_NDSVI = rbind(resultados_NDSVI, data.frame(
    ID_POLIGONO                                 = i,                                                    
    No_de_Meses                                 = length(serie_de_tiempo_NDSVI),                        
    Promedio_de_NDSVI                           = round(mean(serie_de_tiempo_NDSVI), 3),                
    Desviacion_Estandar_de_NDSVI                = round(sd(serie_de_tiempo_NDSVI), 3),                  
    ADF_pvalor                                  = round(adf_NDSVI$p.value, 3),                          
    KPSS_pvalor                                 = round(kpss_NDSVI$p.value, 3),                         
    PP_pvalor                                   = round(pp_NDSVI$p.value, 3),                           
    Estacionaria                                = estacionaria_NDSVI,                                   
    Modelo                                      = as.character(modelo_NDSVI),                           
    AIC                                         = round(modelo_NDSVI$aic, 3),                           
    BIC                                         = round(modelo_NDSVI$bic, 3),                           
    Media_de_Residuos                           = round(mean(residuos_NDSVI), 3),                       
    LJung_Box_pvalor                            = round(ljung_NDSVI$p.value, 3)                         
  ))
}

# Ver cuántas filas quedaron
nrow(resultados_NDSVI)

# Conclusiones de cada prueba
resultados_NDSVI$adf_estacionaria  = resultados_NDSVI$ADF_pvalor  < 0.05
resultados_NDSVI$kpss_estacionaria = resultados_NDSVI$KPSS_pvalor > 0.05
resultados_NDSVI$pp_estacionaria   = resultados_NDSVI$PP_pvalor   < 0.05
resultados_NDSVI$modelo_ok         = resultados_NDSVI$LJung_Box_pvalor > 0.05

# Realizamos los porcentaje de parcelas estacionarias segun cada prueba
round(mean(resultados_NDSVI$adf_estacionaria) * 100, 1)
round(mean(resultados_NDSVI$kpss_estacionaria) * 100, 1)
round(mean(resultados_NDSVI$pp_estacionaria) * 100, 1)

# Realizamos los porcentaje de parcelas estacionarias segun las tres pruebas a la vez
round(mean(resultados_NDSVI$Estacionaria) * 100, 1)

# Parcelas que quedaron con cada modelo
sort(table(resultados_NDSVI$Modelo), decreasing = TRUE)

# Porcentaje de parcelas sin autocorrelacion en los residuos (Ljung-Box)
round(mean(resultados_NDSVI$modelo_ok) * 100, 1)

# Promedio de los residuos en todas las parcelas (deben ser cercanos a 0)
summary(resultados_NDSVI$Media_de_Residuos)

# Realizamos un solo histograma con los residuos de las 197 parcelas juntas
hist(todos_residuos_NDSVI,
     breaks = 40,
     main = "Residuos de las 197 parcelas (NDSVI)",
     xlab = "Residuo",
     col = "lightblue")
abline(v = 0,
       col = "red",
       lwd = 2)

# Para ver el diagnostico completo de una parcela
checkresiduals(auto.arima(ts(ndsvi_ancho[["AGC_001"]], start = c(2022, 1), frequency = 12)))

# Guardar como CSV 
write_csv(resultados_NDSVI, "22_Resultados_Serie_de_Tiempo_de_NDSVI.csv")