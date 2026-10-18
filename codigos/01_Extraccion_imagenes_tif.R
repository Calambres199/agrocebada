####### CODIGO COMPLETO QUE VA A ESTAR EN GITHUB CON EXPLICACIÓN DE TODO LO QUE HACE ######
############### INSTALAR PAQUETES
## Solo es necesario instalar los paquetes una vez
install.packages("terra")
install.packages("dplyr")
install.packages("tidyr")
install.packages("readr")
install.packages("tseries")
install.packages("forecast")
install.packages("tidyverse")
#######################33
library(terra)   # para leer imagenes .tif (raster) y shapefiles (poligonos) y cruzarlos
library(dplyr)   # para manejar tablas: select (escoger columnas) y arrange (ordenar)
library(tidyr)   # para cambiar la forma de las tablas
library(readr)   # para guardar tablas en csv: write_csv
library(tseries) # analisis de series de tiempo
library(forecast) #para estimar diferentes modelos

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
write_csv(prec_largo, "1_precipitacion_extraida.csv")

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
write_csv(tmin_largo, "2_tmin_extraida.csv")


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
write_csv(tmax_largo, "3_tmax_extraida.csv")

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
write_csv(topografia, "4_topografia_extraida.csv")

##############################################
##############################################
