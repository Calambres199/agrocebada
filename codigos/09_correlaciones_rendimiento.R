# Analisis de variables que estan correlacionadas, antes de hacer la regresion lineal

# Cargamos las librerias a utilizar
library(dplyr)      # filter
library(corrplot)   # grafica de la matriz de correlacion

# Le digo a R en que carpeta voy a estar trabajando
setwd('/Users/rebec/Downloads/DataSet/DATASET_RETO_AGRO_2026')

# Visualizo que tiene la carpeta en la que voy a trabajar
list.files()


# Leemos la base que armamos en el script 08
base = read.csv("23_Base_Modelo_Rendimiento.csv")

# Nos quedamos solo con las parcelas que tienen rendimiento (entrenamiento)
entrenamiento = filter(base, CONJUNTO == "ENTRENAMIENTO")
nrow(entrenamiento)   # Debe dar 138

# Nombres de las variables que pueden entrar al modelo
variables = c("prec_siembra", "prec_crecimiento", "prec_llenado", "anomalia_prec",
              "tmin_ciclo", "tmax_llenado",
              "msi_llenado", "ndsvi_madurez",
              "evi_max_planet", "evi_llenado_planet", "dia_pico_planet",
              "elevacion_m", "pendiente_grados", "AREA_HA")

# Revisamos que ninguna variable tenga vacios
colSums(is.na(entrenamiento[, variables]))   # Todo debe dar 0

# CORRELACION DE TODAS LAS VARIABLES VS EL RENDIMIENTO --------------------

# Creamos un vector vacio donde iremos guardando la correlacion de cada variable
correlacion_rendimiento = c()

# for: repite lo de adentro para cada variable de la lista
for (v in variables) {
  
  # Correlacion de Spearman entre esta variable y el rendimiento
  # method = "spearman": por si las relaciones no son lineales
  r = cor(entrenamiento[[v]], entrenamiento$RENDIMIENTO_T_HA, method = "spearman")
  
  # La guardamos en el vector, con el nombre de la variable
  correlacion_rendimiento[v] = r
}

# Visualizamos las correlaciones ordenadas de la mas negativa a la mas positiva
sort(round(correlacion_rendimiento, 2))

# CORRELACION ENTRE LAS VARIABLES -----------------------------------------

# Matriz de correlacion entre todas las variables
# Sirve para ver cuales se parecen entre si, ya que dos variables muy parecidas aportan casi lo mismo al modelo
correlacion_variables = cor(entrenamiento[, variables], method = "spearman")

# Graficamos la matriz de correlacion
# Azul oscuro: cerca de 1 (suben juntas) | Blanco: cerca de 0 (sin relacion) | Rojo oscuro: cerca de -1 (una sube, la otra baja)
# method = "color": cada cuadro se pinta segun su correlacion
# type = "upper": solo la mitad de arriba (la de abajo es igual, como un espejo)
# order = "hclust": acomoda juntas las variables parecidas, asi se ven los grupos
# addCoef.col = "black": escribe el numero de la correlacion dentro de cada cuadro
# tl.cex y number.cex: tamaño de los nombres y de los numeros
corrplot(correlacion_variables,
         method = "color",
         type = "upper",
         order = "hclust",
         addCoef.col = "black",
         number.cex = 0.6,
         tl.col = "black",
         tl.cex = 0.7)

# PARES DE VARIABLES MUY PARECIDAS ----------------------------------------

# Buscamos los pares de variables muy parecidas entre si (|r| > 0.8)
# which: busca en toda la matriz las posiciones que cumplen la condicion, en vez de usar un for
# upper.tri(): una matriz de correlacion es simetrica, por eso da igual buscar en la triangular superior o en la inferior
#              usamos solo la superior para que cada par no aparezca dos veces
# arr.ind = TRUE: nos dice en que fila y columna esta cada correlacion que cumple la condicion
pares = which(abs(correlacion_variables) > 0.8 & upper.tri(correlacion_variables), arr.ind = TRUE)

# variable1: nombre de la fila (indice pares[, 1]) de la matriz de correlacion
# variable2: nombre de la columna (indice pares[, 2]) de la matriz de correlacion
# correlacion: valor de correlacion del par, redondeado a 2 decimales
data.frame(variable1   = rownames(correlacion_variables)[pares[, 1]],
           variable2   = colnames(correlacion_variables)[pares[, 2]],
           correlacion = round(correlacion_variables[pares], 2))

# LAS 4 VARIABLES MAS RELACIONADAS CON EL RENDIMIENTO ---------------------

# Nos quedamos con las 4 variables con mayor correlacion (sin importar si es positiva o negativa) con el rendimiento
top4 = names(sort(abs(correlacion_rendimiento), decreasing = TRUE))[1:4]

# Graficamos cada una contra el rendimiento
# par(mfrow = c(2, 2)): 4 graficas en una sola ventana (2 filas y 2 columnas)
# mar: margenes pequeños para que quepan
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

for (v in top4) {
  plot(entrenamiento[[v]], entrenamiento$RENDIMIENTO_T_HA,
       xlab = v,
       ylab = "Rendimiento (t/ha)",
       main = paste(v, "| r =", round(correlacion_rendimiento[v], 2)),
       pch = 19,
       col = "darkgreen")
  
  # Linea de tendencia (regresion lineal simple) para ver hacia donde va la relacion
  # lm(y ~ x): ajusta una recta entre el rendimiento y la variable
  # abline: dibuja esa recta sobre la grafica
  abline(lm(entrenamiento$RENDIMIENTO_T_HA ~ entrenamiento[[v]]), col = "red", lwd = 2)
}

# Regresamos la ventana de graficas a una sola grafica
par(mfrow = c(1, 1))