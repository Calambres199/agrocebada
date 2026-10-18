# Una fila por parcela (197) con sus variables del ciclo 2025 y su rendimiento
# Las etapas del ciclo se definieron con la escala de Zadoks y se ubicaron en el tiempo con la curva del EVI de 2025 (Planet):
#   Z0-Z1 germinacion y plantula ......... abril-junio
#   Z2-Z3 amacollamiento y encañe ........ julio
#   Z4-Z8 embuche a llenado del grano .... agosto-septiembre
#   Z9    madurez ........................ septiembre-octubre

# Cargamos las librerias a utilizar
library(dplyr)      # filter, group_by, summarise, select y left_join
library(tidyr)      # pivot_longer
library(readr)      # write_csv

# Le digo a R en que carpeta voy a estar trabajando
setwd('/Users/rebec/Downloads/DataSet/DATASET_RETO_AGRO_2026')

# Visualizo que tiene la carpeta en la que voy a trabajar
list.files()

# LECTURA DE LAS BASES ----------------------------------------------------

# Llamamos las bases que se generaron en los scripts anteriores
# Las de MSI y NDSVI son las bases anchas ya regularizadas (rellenas), es decir, sin meses vacios
prec           = read.csv("1_precipitacion_extraida.csv")      # lluvia mensual por parcela (script 01)
tmin           = read.csv("2_tmin_extraida.csv")               # temperatura minima mensual por parcela (script 01)
tmax           = read.csv("3_tmax_extraida.csv")               # temperatura maxima mensual por parcela (script 01)
topo           = read.csv("4_topografia_extraida.csv")         # elevacion y pendiente (script 01)
planet         = read.csv("13_Base_Pro_Filtrada.csv")          # EVI de Planet, filtrado por nubes (script 04)
msi_completa   = read.csv("18_MSI_Ancho_Relleno.csv")          # MSI mensual por parcela, ya completa (script 06)
ndsvi_completa = read.csv("21_NDSVI_Ancho_Rellena.csv")        # NDSVI mensual por parcela, ya completa (script 07)
rendimiento    = read.csv("ID_area_rendimiento_70_30_Reto_AgroCebada.csv")

# Nos aseguramos de que el mes sea de tipo numerico
prec$mes   = as.numeric(prec$mes)
tmin$mes   = as.numeric(tmin$mes)
tmax$mes   = as.numeric(tmax$mes)
planet$mes = as.numeric(planet$mes)

# Revisamos que esten las 197 parcelas en cada base
length(unique(prec$ID_POLIGONO))     # Debe dar 197
length(unique(planet$ID_POLIGONO))   # Debe dar 197
ncol(msi_completa) - 1               # Debe dar 197 (todas las columnas menos la fecha)
ncol(ndsvi_completa) - 1             # Debe dar 197
table(rendimiento$CONJUNTO)          # Debe dar 138 ENTRENAMIENTO y 59 PREDICCION

# PRECIPITACIÓN -----------------------------------------------------------

# El analisis de series de tiempo mostro que la lluvia depende de la epoca del año (no del mes anterior)
# Por eso se mide por etapa del ciclo 2025
# Se usa la suma porque la lluvia se acumula: interesa cuanta agua recibio el cultivo en total

# Nos quedamos solo con el ciclo 2025
prec_2025 = filter(prec, anio == 2025)

variables_prec = summarise(group_by(prec_2025, ID_POLIGONO),
                           prec_siembra     = sum(precipitacion_mm[mes %in% c(4, 5, 6)]),   # germinacion y plantula, Z0-Z1 (abril-junio)
                           prec_crecimiento = sum(precipitacion_mm[mes == 7]),              # amacollamiento y encañe, Z2-Z3 (julio)
                           prec_llenado     = sum(precipitacion_mm[mes %in% c(8, 9)]),      # embuche a llenado del grano, Z4-Z8 (agosto-septiembre)
                           .groups = "drop")

# Anomalia de lluvia: temporada 2025 comparada con lo normal de cada parcela (2022-2024)
# La lluvia de un mes se relaciona con el mismo mes de años anteriores

# Temporada: abril a octubre
temporada = filter(prec, mes >= 4, mes <= 10)

# Lluvia total de cada temporada por parcela y año
lluvia_temporada = summarise(group_by(temporada, ID_POLIGONO, anio),
                             lluvia = sum(precipitacion_mm),
                             .groups = "drop")

# Lluvia normal de cada parcela: promedio de las temporadas 2022-2024
normal = summarise(group_by(filter(lluvia_temporada, anio < 2025), ID_POLIGONO),
                   lluvia_normal = mean(lluvia),
                   .groups = "drop")

# Unimos la lluvia de 2025 con la normal de cada parcela
anomalia = left_join(filter(lluvia_temporada, anio == 2025),
                     normal,
                     by = "ID_POLIGONO")

# Si es positiva llovio mas de lo normal, si es negativa llovio menos
anomalia$anomalia_prec = anomalia$lluvia - anomalia$lluvia_normal
anomalia = select(anomalia, ID_POLIGONO, anomalia_prec)

# Agregamos la anomalia a las variables de precipitacion
variables_prec = left_join(variables_prec, anomalia, by = "ID_POLIGONO")

# Revisamos
nrow(variables_prec)             # Debe dar 197
colSums(is.na(variables_prec))   # Todo debe dar 0
summary(variables_prec)

# TEMPERATURA -------------------------------------------------------------

# Se usa el promedio porque la temperatura no se acumula: interesa que tan frio o caliente estuvo

# Temperatura minima: promedio de todo el ciclo (abril a octubre de 2025)
# Las noches frias o las heladas retrasan el crecimiento en cualquier etapa
variables_tmin = summarise(group_by(filter(tmin, anio == 2025, mes >= 4, mes <= 10), ID_POLIGONO),
                           tmin_ciclo = mean(tmin_c),
                           .groups = "drop")

# Temperatura maxima: promedio del embuche al llenado del grano, Z4-Z8 (agosto-septiembre de 2025)
# Es la etapa mas sensible al calor
variables_tmax = summarise(group_by(filter(tmax, anio == 2025, mes %in% c(8, 9)), ID_POLIGONO),
                           tmax_llenado = mean(tmax_c),
                           .groups = "drop")

# Unimos las dos variables de temperatura en una tabla
variables_temp = left_join(variables_tmin, variables_tmax, by = "ID_POLIGONO")

# Revisamos
nrow(variables_temp)             # Debe dar 197
colSums(is.na(variables_temp))   # Todo debe dar 0
summary(variables_temp)

# MSI Y NDSVI (SENTINEL-2) ------------------------------------------------

# Para MSI y NDSVI usamos las bases que ya se completaron en el analisis de series de tiempo
# Esas bases estan en formato ancho (una columna por parcela), que sirvio para las series de tiempo
# Para sacar las variables por parcela se necesita formato largo: una fila por parcela y mes
# Asi tendremos una columna ID_POLIGONO para poder agrupar (group_by) y unir (left_join)
# cols = -fecha: todas las columnas menos la fecha (es decir, las 197 parcelas)
# names_to: los nombres de las columnas (AGC_001, AGC_002...) pasan a la columna ID_POLIGONO
# values_to: los valores pasan a la columna msi (o ndsvi)
msi_largo   = pivot_longer(msi_completa,   cols = -fecha, names_to = "ID_POLIGONO", values_to = "msi")
ndsvi_largo = pivot_longer(ndsvi_completa, cols = -fecha, names_to = "ID_POLIGONO", values_to = "ndsvi")

# Las bases completas solo traen la fecha, pero para filtrar 2025 y los meses de cada etapa necesitamos el año y el mes
# as.Date: convierte la fecha de texto ("2025-08-01") a fecha real
# format(..., "%Y"): toma solo el año  |  format(..., "%m"): toma solo el mes
# as.numeric: los convierte en numero (2025 y 8), para poder compararlos al filtrar
msi_largo$fecha = as.Date(msi_largo$fecha)
msi_largo$anio  = as.numeric(format(msi_largo$fecha, "%Y"))
msi_largo$mes   = as.numeric(format(msi_largo$fecha, "%m"))

ndsvi_largo$fecha = as.Date(ndsvi_largo$fecha)
ndsvi_largo$anio  = as.numeric(format(ndsvi_largo$fecha, "%Y"))
ndsvi_largo$mes   = as.numeric(format(ndsvi_largo$fecha, "%m"))

# filter: nos quedamos con los meses de la etapa en 2025
# group_by: agrupamos por parcela
# mean: sacamos el promedio de esos meses

# MSI del embuche al llenado del grano, Z4-Z8 (agosto-septiembre de 2025): agua en la planta
variables_msi = summarise(group_by(filter(msi_largo, anio == 2025, mes %in% c(8, 9)), ID_POLIGONO),
                          msi_llenado = mean(msi),
                          .groups = "drop")

# NDSVI en la madurez, Z9 (septiembre-octubre de 2025): transicion de verde a seco
variables_ndsvi = summarise(group_by(filter(ndsvi_largo, anio == 2025, mes %in% c(9, 10)), ID_POLIGONO),
                            ndsvi_madurez = mean(ndsvi),
                            .groups = "drop")

# Unimos en una tabla con una fila por parcela y sus dos variables (msi_llenado y ndsvi_madurez)
variables_sentinel = left_join(variables_msi, variables_ndsvi, by = "ID_POLIGONO")

# Revisamos
nrow(variables_sentinel)             # Debe dar 197
colSums(is.na(variables_sentinel))   # Todo debe dar 0
summary(variables_sentinel)

# EVI (PLANET) ------------------------------------------------------------

# Se usa el EVI de Planet por su mayor frecuencia de imagenes y resolucion
# Solo de junio a octubre de 2025, cuando el cultivo es visible desde el satelite
planet$fecha_captura = as.Date(planet$fecha_captura)
planet_ciclo = filter(planet, anio == 2025, mes >= 6, mes <= 10)

variables_planet = summarise(group_by(planet_ciclo, ID_POLIGONO),
                             evi_max_planet     = max(evi_promedio),                                                 # verdor maximo del ciclo
                             evi_llenado_planet = median(evi_promedio[mes %in% c(8, 9)]),                            # verdor del embuche al llenado, Z4-Z8
                             dia_pico_planet    = as.numeric(format(fecha_captura[which.max(evi_promedio)], "%j")),  # dia del año del verdor maximo
                             .groups = "drop")

# Revisamos
nrow(variables_planet)             # Debe dar 197
colSums(is.na(variables_planet))   # Todo debe dar 0
summary(variables_planet)

# TOPOGRAFIA Y UNION DE TODAS LAS VARIABLES -------------------------------

# Seleccionamos las variables de topografia que vamos a usar
variables_topo = select(topo, ID_POLIGONO, Municipio, Estado, elevacion_m, pendiente_grados)

# Empezamos con la base de rendimiento y le vamos pegando las variables de cada paso
base = select(rendimiento, ID_POLIGONO, AREA_HA, RENDIMIENTO_T_HA, CONJUNTO)
base = left_join(base, variables_prec,     by = "ID_POLIGONO")   # precipitacion
base = left_join(base, variables_temp,     by = "ID_POLIGONO")   # temperatura
base = left_join(base, variables_sentinel, by = "ID_POLIGONO")   # MSI y NDSVI
base = left_join(base, variables_planet,   by = "ID_POLIGONO")   # EVI de Planet
base = left_join(base, variables_topo,     by = "ID_POLIGONO")   # topografia

# Revisamos
nrow(base)               # Debe dar 197
colSums(is.na(base))     # Solo RENDIMIENTO_T_HA debe tener vacios (59: las de prediccion)
table(base$CONJUNTO)     # Debe dar 138 ENTRENAMIENTO y 59 PREDICCION
head(base)

# Guardamos la base lista para el modelo
write_csv(base, "23_Base_Modelo_Rendimiento.csv")