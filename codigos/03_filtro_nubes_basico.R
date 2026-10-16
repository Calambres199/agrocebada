# Cargamos las librerias a utilizar
library(dplyr)       # Para filtrar, seleccionar y ordenar tablas
library(readr)       # Para leer y guardar archivos csv
library(lubridate)   # Para trabajar con fechas (dmy, month, year)

# Le digo a R en que carpeta voy a estar trabajando
setwd('/Users/rebec/Downloads/DataSet/DATASET_RETO_AGRO_2026')

# Visualizo que tiene la carpeta en la que voy a trabajar
list.files()

# Cargamos la base de datos
datos_original_basico = read.csv("Conjunto_datos_BASICO_AgroCebada2026.csv")

# Visualizamos las primeras filas
head(datos_original_basico)

# Para ver los nombres de las columnas
names(datos_original_basico)

# Realizamos el filtro por nubosidad
# Nos quedamos con las imagenes con porcentaje_nubosidad menor a 20
datos_filtrados_sin_nubosidad_basico = datos_original_basico %>%
  filter(porcentaje_nubosidad < 20)

head(datos_filtrados_sin_nubosidad_basico)

# Separamos la fecha en mes y año
# No se filtra por mes, ya que las series de tiempo usan los 12 meses de cada año
datos_filtrados_basico = datos_filtrados_sin_nubosidad_basico %>%
  mutate(fecha_captura = dmy(fecha_captura),   # formato dd/mm/yyyy
         mes = month(fecha_captura),
         anio = year(fecha_captura))

# Visualizamos las primeras filas
head(datos_filtrados_basico)

# Guardo la base de datos con el filtro de nubes
# row.names = FALSE para que no agregue una columna con el numero de fila
write.csv(datos_filtrados_basico, "11_Base_Basica_Filtrada_por_Nubosidad.csv", row.names = FALSE)

# Leemos la base de datos basica filtrada por nubosidad
basico_filtrado_nubes = read.csv("11_Base_Basica_Filtrada_por_Nubosidad.csv")

# Visualizamos las primeras filas
head(basico_filtrado_nubes)

# Realizamos la selección de las variables que vamos a usar
basico_filtrado_nubes_seleccion = select(basico_filtrado_nubes,
                                         ID_POLIGONO,
                                         fecha_captura,
                                         anio,
                                         mes,
                                         sensor,
                                         evi_promedio,
                                         msi_promedio,
                                         ndsvi_promedio)

head(basico_filtrado_nubes_seleccion)
names(basico_filtrado_nubes_seleccion)

# Solo nos quedaremos con el satelite Sentinel-2
# Esto se debe a que las filas del satelite Landsat se encuentran vacias en estos indices
basico_filtrado_nubes_seleccion = filter(basico_filtrado_nubes_seleccion,
                                         sensor == "Sentinel-2")

head(basico_filtrado_nubes_seleccion)

# Ordenamos por parcela y fecha
basico_filtrado_nubes_seleccion = arrange(basico_filtrado_nubes_seleccion,
                                          ID_POLIGONO,
                                          fecha_captura)

head(basico_filtrado_nubes_seleccion)

# Revisamos que esten las 197 parcelas y los 12 meses
length(unique(basico_filtrado_nubes_seleccion$ID_POLIGONO))   # debe dar 197
sort(unique(basico_filtrado_nubes_seleccion$mes))             # deben salir del 1 al 12

# Guardamos la base con las variables ya seleccionadas
write.csv(basico_filtrado_nubes_seleccion, "12_Base_Basica_Filtrada.csv", row.names = FALSE)
