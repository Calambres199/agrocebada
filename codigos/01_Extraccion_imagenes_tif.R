####### CODIGO COMPLETO QUE VA A ESTAR EN GITHUB CON EXPLICACIÓN DE TODO LO QUE HACE ######
############### INSTALAR PAQUETES
install.packages("terra")
install.packages("dplyr")
install.packages("tidyr")
install.packages("read")
install.packages("tseries")
install.packages("forescast")
install.packages("terra")
#######################33
library(terra)   # para leer imagenes .tif (raster) y shapefiles (poligonos) y cruzarlos
library(dplyr)   # para manejar tablas: select (escoger columnas) y arrange (ordenar)
library(tidyr)   # para cambiar la forma de las tablas
library(readr)   # para guardar tablas en csv: write_csv
library(tseries) # analisis de series de tiempo
library(forecast) #para estimar diferentes modelos
library(tidyr)
# setwd: le dice a R en que carpeta estan mis archivos
setwd('/Users/rebec/Downloads/DataSet/DATASET_RETO_AGRO_2026')


# el shapefile de las parcelas esta en la carpeta Parcelas_Reto_AGC_CONJUNTO_70_30
# vect: lee los poligonos de las 197 parcelas
parcelas = vect("Parcelas_Reto_AGC_CONJUNTO_70_30/Parcelas_Reto_AGC_CONJUNTO.shp")
parcelas   # debe decir 197 geometrias

# PRECIPITACION
# los 48 .tif estan repartidos en carpetas por año (2022, 2023, 2024, 2025)
# list.files los busca todos de una vez:
#   pattern = "^PREC_.*\\.tif$" -> archivos que empiezan con PREC_ y terminan en .tif
#   recursive = TRUE            -> busca dentro de todas las subcarpetas
#   full.names = TRUE           -> guarda la ruta completa
archivos_prec = list.files(".", pattern = "^PREC_.*\\.tif$",
                           recursive = TRUE, full.names = TRUE)
length(archivos_prec)   # debe dar 48 (4 años x 12 meses)

# rast: junta los 48 .tif en un solo objeto con 48 capas (una por mes)
prec_rast = rast(archivos_prec)
# a cada capa le pongo el nombre de su archivo sin el .tif (PREC_2022_01...)
names(prec_rast) = sub("\\.tif$", "", basename(archivos_prec))

# extract: saca el valor de cada parcela en cada mes
#   terra:: -> uso el extract de terra (tidyr tiene otro extract con el mismo nombre)
#   fun = mean   -> promedio si la parcela toca varios pixeles
#   exact = TRUE -> el promedio pesa segun cuanto de cada pixel cubre la parcela
#   na.rm = TRUE -> ignora pixeles vacios
prec_valores = terra::extract(prec_rast, parcelas, fun = mean, exact = TRUE, na.rm = TRUE)


# agrego el nombre de la parcela (en el shapefile la columna se llama ID_POLIGON)
prec_valores$ID_POLIGONO = parcelas$ID_POLIGON
# quito la columna ID numerica que agrega terra
prec_valores$ID = NULL

# pivot_longer: paso de una columna por mes a una fila por parcela y mes
prec_largo = pivot_longer(prec_valores, cols = starts_with("PREC"),
                          names_to = "archivo", values_to = "precipitacion_mm")

# del nombre PREC_2022_01 saco el año (letras 6 a 9) y el mes (letras 11 a 12)
prec_largo$anio = as.numeric(substr(prec_largo$archivo, 6, 9))
prec_largo$mes  = as.numeric(substr(prec_largo$archivo, 11, 12))

# me quedo con las columnas que sirven y ordeno por parcela, año y mes
prec_largo = select(prec_largo, ID_POLIGONO, anio, mes, precipitacion_mm)
prec_largo = arrange(prec_largo, ID_POLIGONO, anio, mes)

#########################3
nrow(prec_largo)                        # debe dar 9456 (197 x 48)
sum(is.na(prec_largo$precipitacion_mm)) # debe dar 0. Debe dar 0 porque eso significa que cada parcela obtuvo un valor en cada uno de los 48 meses, es decir, que la extracción funcionó bien.
head(prec_largo)                        # primeras 6 filas

# guardar en csv
write_csv(prec_largo, "precipitacion_extraida.csv")

################# TEMPERATURA MAXIMA Y MINIMA ############################
##### TEMPERATURA MINIMA #############
# busco los 48 .tif que empiezan con Tmin_ (uno por mes)
archivos_tmin = list.files(".", pattern = "^Tmin_.*\\.tif$",
                           recursive = TRUE, full.names = TRUE)
length(archivos_tmin)   # debe dar 48

# junto los 48 .tif en un solo objeto con 48 capas y les pongo su nombre (Tmin_2022_01...)
tmin_rast = rast(archivos_tmin)
names(tmin_rast) = sub("\\.tif$", "", basename(archivos_tmin))

# saco el valor de cada parcela en cada mes (terra:: para no usar el extract de tidyr)
tmin_valores = terra::extract(tmin_rast, parcelas, fun = mean, exact = TRUE, na.rm = TRUE)


# agrego el nombre de la parcela y quito la columna ID numerica
tmin_valores$ID_POLIGONO = parcelas$ID_POLIGON
tmin_valores$ID = NULL


# paso de una columna por mes a una fila por parcela y mes
tmin_largo = pivot_longer(tmin_valores, cols = starts_with("Tmin"),
                          names_to = "archivo", values_to = "tmin_c")


# del nombre Tmin_2022_01 saco el año (letras 6 a 9) y el mes (letras 11 a 12)
tmin_largo$anio = as.numeric(substr(tmin_largo$archivo, 6, 9))
tmin_largo$mes  = as.numeric(substr(tmin_largo$archivo, 11, 12))


# columna fecha con el dia 1 de cada mes (2022-01-01 = enero 2022)
tmin_largo$fecha = as.Date(paste(tmin_largo$anio, tmin_largo$mes, 1, sep = "-"))

# me quedo con las columnas que sirven y ordeno por parcela y fecha
tmin_largo = select(tmin_largo, ID_POLIGONO, fecha, anio, mes, tmin_c)
tmin_largo = arrange(tmin_largo, ID_POLIGONO, fecha)


# revisar
nrow(tmin_largo)                                          # debe dar 9456
sum(is.na(tmin_largo$tmin_c))                             # debe dar 0
table(table(tmin_largo$ID_POLIGONO))                      # 48 meses por parcela
sum(duplicated(tmin_largo[, c("ID_POLIGONO", "fecha")]))  # debe dar 0
head(tmin_largo)

# guardar en csv
write_csv(tmin_largo, "tmin_extraida.csv")


######## TEMPERATURA MAXIMA ##########


# mismo proceso, ahora con los archivos que empiezan con Tmax_
archivos_tmax = list.files(".", pattern = "^Tmax_.*\\.tif$",
                           recursive = TRUE, full.names = TRUE)
length(archivos_tmax)   # debe dar 48

tmax_rast = rast(archivos_tmax)
names(tmax_rast) = sub("\\.tif$", "", basename(archivos_tmax))

tmax_valores = terra::extract(tmax_rast, parcelas, fun = mean, exact = TRUE, na.rm = TRUE)

tmax_valores$ID_POLIGONO = parcelas$ID_POLIGON
tmax_valores$ID = NULL

tmax_largo = pivot_longer(tmax_valores, cols = starts_with("Tmax"),
                          names_to = "archivo", values_to = "tmax_c")

tmax_largo$anio = as.numeric(substr(tmax_largo$archivo, 6, 9))
tmax_largo$mes  = as.numeric(substr(tmax_largo$archivo, 11, 12))
tmax_largo$fecha = as.Date(paste(tmax_largo$anio, tmax_largo$mes, 1, sep = "-"))

tmax_largo = select(tmax_largo, ID_POLIGONO, fecha, anio, mes, tmax_c)
tmax_largo = arrange(tmax_largo, ID_POLIGONO, fecha)

# revisar
nrow(tmax_largo)                                          # debe dar 9456
sum(is.na(tmax_largo$tmax_c))                             # debe dar 0
table(table(tmax_largo$ID_POLIGONO))                      # 48 meses por parcela
sum(duplicated(tmax_largo[, c("ID_POLIGONO", "fecha")]))  # debe dar 0
head(tmax_largo)

# guardar en csv
write_csv(tmax_largo, "tmax_extraida.csv")

#################### TOPOGRAFIA #####################
parcelas = vect("Parcelas_Reto_AGC_CONJUNTO_70_30/Parcelas_Reto_AGC_CONJUNTO.shp")
parcelas   # debe decir 197 geometrias

# Buscar los archivos
# busco los .tif de elevacion y pendiente en todas las subcarpetas
archivo_elev = list.files(".", pattern = "^Elevacion_INEGI_CEM4_120m\\.tif$",
                          recursive = TRUE, full.names = TRUE)
archivo_pend = list.files(".", pattern = "^Pendiente_INEGI_CEM4_120m_grados\\.tif$",
                          recursive = TRUE, full.names = TRUE)
archivo_elev   # debe mostrar una sola ruta
archivo_pend   # debe mostrar una sola ruta

# rast: leo cada imagen (aqui es una sola capa, no 48)
elev_rast = rast(archivo_elev)
pend_rast = rast(archivo_pend)

# igualar coordenadas
# crs: muestra el sistema de coordenadas de cada cosa
crs(parcelas, describe = TRUE)    # WGS84 (grados)
crs(elev_rast, describe = TRUE)   # EPSG:6372 (metros)

# project: convierte las parcelas al mismo sistema de coordenadas que la imagen
parcelas_6372 = project(parcelas, crs(elev_rast))

# Extraer
# saco el valor de cada parcela (terra:: para no usar el extract de tidyr)
#   fun = mean   -> promedio de los pixeles que toca la parcela
#   exact = TRUE -> pesa segun cuanto de cada pixel cubre la parcela
#   na.rm = TRUE -> ignora pixeles sin dato
elev_valores = terra::extract(elev_rast, parcelas_6372, fun = mean, exact = TRUE, na.rm = TRUE)
pend_valores = terra::extract(pend_rast, parcelas_6372, fun = mean, exact = TRUE, na.rm = TRUE)

# Armar la tabla
# una fila por parcela con su id, municipio, estado, elevacion y pendiente
# (la columna 2 de cada resultado es el valor; la 1 es el ID numerico de terra)
# el municipio y el estado no vienen en los .tif, vienen en la tabla del shapefile de las parcelas
topografia = data.frame(ID_POLIGONO      = parcelas$ID_POLIGON,
                        Municipio        = parcelas$Municipio,
                        Estado           = parcelas$Estado,
                        elevacion_m      = elev_valores[, 2],
                        pendiente_grados = pend_valores[, 2])

# ordeno por parcela
topografia = arrange(topografia, ID_POLIGONO)

# ===== REVISAR =====
nrow(topografia)                          # debe dar 197
sum(is.na(topografia$elevacion_m))        # debe dar 0
sum(is.na(topografia$pendiente_grados))   # debe dar 0
sum(is.na(topografia$Municipio))          # debe dar 0
table(topografia$Estado)                  # parcelas por estado (Puebla 100, Hidalgo 51, Tlaxcala 46)
summary(topografia)                       # rangos de elevacion y pendiente
head(topografia)

# guardar en csv
write_csv(topografia, "topografia_extraida.csv")

##############################################
##############################################
##############################################


setwd('/Users/rebec/Downloads/DataSet/DATASET_RETO_AGRO_2026')

# Llamamos la base que se extrajo de las imagenes .tif de precipitacion
prec = read.csv("precipitacion_extraida.csv")

# el mes puede venir como texto, asi que lo pasamos a numero
prec$mes = as.numeric(prec$mes)
# creo la columna fecha con el dia 1 de cada mes (ejemplo: 2022-01-01 = enero 2022)
prec$fecha = as.Date(paste(prec$anio, prec$mes, 1, sep = "-"))

# lista de las 197 parcelas
ids = sort(unique(prec$ID_POLIGONO))
length(ids)   # debe dar 197

# revisar los datos de precipitacion con un summary, ya que da el max, min, mediana, media y los cuartiles
summary(prec$precipitacion_mm)

# boxplot por mes, vamos a comparar valores con los de su mes y ver por mes valores atipicos
boxplot(precipitacion_mm ~ mes, data = prec, main = "Precipitacion por mes (197 parcelas)", xlab = "Mes", ylab = "mm")

# Vamos a graficar las 197 series de tiempo, para eso vamos a pasar de tener una columna con todos los datos,
# a tener una columna por parcela y dejaremos las filas por meses.
prec_ancho = pivot_wider(select(prec, ID_POLIGONO, fecha, precipitacion_mm), names_from = ID_POLIGONO, values_from = precipitacion_mm)
prec_ancho = arrange(prec_ancho, fecha) # arrange ordena las fechas de la base prec_ancho
matriz = as.matrix(prec_ancho[, -1])    # creamos una matriz de datos quitandole la columna de fechas

# si se desea descargar esta base de datos ya a lo ancho se usa el siguiente comando: write_csv(prec_ancho, "prec_ancho.csv")

matplot(prec_ancho$fecha, matriz, type = "l", lty = 1, col = rgb(0, 0, 1, 0.15), xlab = "Fecha", ylab = "mm", main = "Precipitacion mensual de las parcelas")
# col = rgb(0, 0, 1, 0.15) lo que hace es que le da el color azul con (0,0,1) con transparencia alpha = 0.15
# matplot es una funcion que deja graficar varias series a la vez, donde recibe valores en forma de vector o matriz, matplot(x,y,...)
# en este caso agarramos la columna de las fechas (eje X) y graficamos la precipitacion de cada parcela, que esa info la tiene matriz.


# Analisis de las 197 series de tiempo

resultados_prec = data.frame() # Una base de datos vacia para ir guardando

todos_residuos = c()           # vector vacio donde se van a juntar los residuos de todas las parcelas

for (i in ids){
  print(i)   # muestra en la consola que parcela va procesando, para ver el avance
  
  serie = ts(prec_ancho[[i]], start = c(2022,1), frequency = 12) # que de la base ancho tome las columnas y me las haga en series de tiempo
  
  # pruebas de estacionariedad: ADF, KPSS y la de Phillips-Perron
  adf  = adf.test(serie)
  kpss = kpss.test(serie, null = "Level")
  pp   = pp.test(serie)
  
  # la serie es estacionaria si las tres pruebas coinciden
  # ADF y PP: si p < 0.05 entonces podemos decir que es estacionaria; KPSS: si p > 0.05 entonces podemos decir que es estacionaria
  estacionaria = adf$p.value < 0.05 & kpss$p.value > 0.05 & pp$p.value < 0.05
  
  # modelo de la serie: auto.arima elige el modelo solo
  modelo = auto.arima(serie)
  
  # Analizaremos los residuos tambien: si el modelo es bueno, deben parecerse a una normal
  # con media 0, varianza constante (aprox.) y sin autocorrelacion
  residuos = residuals(modelo)
  
  # agrego los residuos de esta parcela al vector con los de todas
  todos_residuos = c(todos_residuos, residuos)
  
  
  
  # Ljung-Box nos ayuda a ver si quedo autocorrelacion en los residuos del modelo
  # si p > 0.05 los residuos no tienen patron y el modelo es adecuado
  ljung = Box.test(residuals(modelo), lag = 12, type = "Ljung-Box")
  
  # Para la autocorrelacion se hace una prueba de Ljung-Box, o en pocas palabras un Box.test de los residuos
  # (tambien se puede ver con checkresiduals(modelo))
  # Si p > 0.05 entonces no hay autocorrelacion, es decir, no quedo un patron que el modelo no haya capturado
  ljung = Box.test(residuos, lag = 12, type = "Ljung-Box")
  
  
  
  # Aqui guardaremos los resultados de las pruebas de atras
  resultados_prec = rbind(resultados_prec, data.frame(
    ID_POLIGONO    = i,                          # nombre de la parcela
    n_meses        = length(serie),              # cuantos meses tiene la serie (debe ser 48)
    promedio_mm    = round(mean(serie), 1),      # precipitacion promedio, con 1 decimal
    desv_mm        = round(sd(serie), 1),        # desviacion estandar (que tanto varia), con 1 decimal
    adf_p          = round(adf$p.value, 3),      # $p.value saca solo el p-valor de la prueba ADF
    kpss_p         = round(kpss$p.value, 3),     # p-valor de la prueba KPSS
    pp_p           = round(pp$p.value, 3),       # p-valor de la prueba Phillips-Perron
    estacionaria   = estacionaria,               # TRUE si las tres pruebas dicen que es estacionaria
    modelo         = as.character(modelo),       # modelo elegido por auto.arima
    aic            = round(modelo$aic, 1),       # AIC del model
    aicc	   = round(modelo$aicc, 1),   	 # AICc: criterio con el que auto.arima eligio el modelo
    bic            = round(modelo$bic, 1),       # BIC del modelo
    media_residuos = round(mean(residuos), 2),   # promedio de los residuos (debe ser cercano a 0)
    ljung_p        = round(ljung$p.value, 3)     # p-valor de Ljung-Box (sin coma, es la ultima)
  ))
}


# Conclusiones de cada prueba
# ADF y PP: si p < 0.05 entonces podria ser estacionaria
# KPSS: si p > 0.05 entonces podriamos decir que es estacionaria
resultados_prec$adf_estac  = resultados_prec$adf_p  < 0.05
resultados_prec$kpss_estac = resultados_prec$kpss_p > 0.05
resultados_prec$pp_estac   = resultados_prec$pp_p   < 0.05

# Ljung: si p > 0.05 los residuos no tienen autocorrelacion (el modelo es adecuado)
resultados_prec$modelo_ok  = resultados_prec$ljung_p > 0.05

# porcentaje de parcelas estacionarias segun cada prueba
round(mean(resultados_prec$adf_estac) * 100, 1)
round(mean(resultados_prec$kpss_estac) * 100, 1)
round(mean(resultados_prec$pp_estac) * 100, 1)
# porcentaje de parcelas estacionarias segun las tres pruebas a la vez
round(mean(resultados_prec$estacionaria) * 100, 1)
# cuantas parcelas quedaron con cada modelo
sort(table(resultados_prec$modelo), decreasing = TRUE)
# porcentaje de parcelas sin autocorrelacion en los residuos (Ljung-Box)
round(mean(resultados_prec$modelo_ok) * 100, 1)
# promedio de los residuos en todas las parcelas (deben ser cercanos a 0)
summary(resultados_prec$media_residuos)

# un solo histograma con los residuos de las 197 parcelas juntas
# si tiene forma de campana, los residuos son aproximadamente normales
hist(todos_residuos, breaks = 40, main = "Residuos de las 197 parcelas",
     xlab = "Residuo", col = "lightblue")
abline(v = 0, col = "red", lwd = 2)   # linea en 0: la campana debe estar centrada ahi

# si quiero ver el diagnostico completo de UNA parcela (grafica de residuos, su ACF e histograma)
checkresiduals(auto.arima(ts(prec_ancho[["AGC_001"]], start = c(2022, 1), frequency = 12)))

# guardar la tabla con los resultados de las 197 parcelas
write_csv(resultados_prec, "resultados_series_precipitacion.csv")
