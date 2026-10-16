# Instalamos las librerias a utilizar
install.packages("dplyr")
install.packages("readr")
install.packages("tidyverse")

# Cargamos las librerias a utilizar
library(dplyr)
library(readr)
library(tidyverse)

# Le digo a R en que carpeta voy a estar trabajando
setwd('/Users/rebec/Downloads/DataSet/DATASET_RETO_AGRO_2026')

# Visualizo que tiene la carpeta en la que voy a trabajar
list.files()

# Cargamos la base de datos
datos_original_pro = read.csv("Conjunto_datos_PRO_AgroCebada.csv")

# Visualizamos las primeras filas
head(datos_original_pro)

# Como la fecha viene como dia/mes/año la vamos a convertir en fecha y obtenemos el año y el mes
datos_original_pro$fecha_captura =as.Date(datos_original_pro$fecha_captura,
                                          format = "%d/%m/%Y")
datos_original_pro$anio = as.numeric(format(datos_original_pro$fecha_captura, "%Y"))
datos_original_pro$mes = as.numeric(format(datos_original_pro$fecha_captura, "%m"))

# Filtraremos las imagenes con nubosidad de 20% o mas
datos_filtrados_sin_nubosidad_pro = filter(datos_original_pro, 
                                           porcentaje_nubosidad < 20)

head(datos_filtrados_sin_nubosidad_pro)

# Realizamos la seleccion de variables que vamos a utilizar
pro_filtrado_nubes_seleccion = select(datos_filtrados_sin_nubosidad_pro,
                                      ID_POLIGONO,
                                      fecha_captura,
                                      anio,
                                      mes, 
                                      sensor,
                                      evi_promedio)

head(pro_filtrado_nubes_seleccion)
names(pro_filtrado_nubes_seleccion)

# Ordenamos por parcela y fecha
pro_filtrado_nubes_seleccion = arrange(pro_filtrado_nubes_seleccion,
                                       ID_POLIGONO,
                                       fecha_captura)

head(pro_filtrado_nubes_seleccion)

# Realizamos una revision
nrow(pro_filtrado_nubes_seleccion)
length(unique(pro_filtrado_nubes_seleccion$ID_POLIGONO))        # Debe de dear 197
sum(is.na(pro_filtrado_nubes_seleccion$evi_promedio))           # debe de dar 0
range(pro_filtrado_nubes_seleccion$fecha_captura)               # Solo debe de haber de 2025

# Guardamos la base con las variables ya seleccionadas
write.csv(pro_filtrado_nubes_seleccion, "13_Base_Pro_Filtrada.csv", row.names = FALSE)

# Realizamos una gráfica para ver el comportamiento del EVI en el año 2025, año en donde se realizó la cosecha de la cebada
datos_pro = read.csv("13_Base_Pro_Filtrada.csv")

head(datos_pro)

datos_pro$fecha_captura = as.Date(datos_pro$fecha_captura)

ids = sort(unique(datos_pro$ID_POLIGONO))
length(ids)          # Deben de ser 197

# Realizamos una gráfica vacia con el rango de fechas y de EVI
# type = "n" no dibuja nada, solo prepara los ejes
plot(range(datos_pro$fecha_captura),
     range(datos_pro$evi_promedio),
     type = "n",
     xlab = "Fecha",
     ylab = "EVI",
     main = "EVI de Planet en 2025 de las 197 parcelas")

# Realizamos una linea por parcela
# Se realziara en verde transparente para no saturar
for (i in ids) {
  p = filter(datos_pro, ID_POLIGONO == i)
  p = arrange(p, fecha_captura)
  lines(p$fecha_captura, p$evi_promedio, col = rgb(0, 0.5, 0, 0.1))
}

# Realizamos una linea vertical al inicio del mes de noviembre
abline(v = as.Date("2025-11-01"), lty = 2, col = "red")

# Se observo que cada parcela tenia fechas distintas por lo que las agruparemos por semana para posteriormente obtener la mediana de todas
datos_pro$semana = as.Date(cut(datos_pro$fecha_captura, "week"))

curva = summarise(group_by(datos_pro, semana), evi = median(evi_promedio), .groups = "drop")
lines(curva$semana, curva$evi, col = "black", lwd = 2)

# Semana en la que la mediana del EVI llega a su maximo
curva$semana[which.max(curva$evi)]

# Para cada parcela: Obtenemos su EVI máximo y la fecha en la que lo alcanzo (solo de junio a octubre)
datos_pro_ciclo = filter(datos_pro,
                         mes >= 6,
                         mes <= 10)
revision = summarise(group_by(datos_pro_ciclo,
                              ID_POLIGONO),
                     evi_maximo = max(evi_promedio),
                     fecha_pico = fecha_captura[which.max(evi_promedio)],
                     .groups = "drop")

# Realizamos otro filtro, ahora para las parcelas que no alzaron un verde sufienciente (EVI maximo muy bajo)
filter(revision,
       evi_maximo < 0.04)

# Realizamos otro filtro para las parcelas con el pico fuera de la temporada esperada (antes de julio o despues de septiembre)
filter(revision,
       fecha_pico < as.Date("2025-07-01") | fecha_pico > as.Date("2025-09-30"))