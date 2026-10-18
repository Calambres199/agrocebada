# Regresion lineal para predecir el rendimiento de la cebada por parcela

# Cargamos las librerias a utilizar
library(dplyr)    # filter
library(readr)    # write_csv

# Le digo a R en que carpeta voy a estar trabajando
setwd('/Users/rebec/Downloads/DataSet/DATASET_RETO_AGRO_2026')

# Visualizo que tiene la carpeta en la que voy a trabajar
list.files()

# LECTURA DE LA BASE ------------------------------------------------------

# Leemos la base que armamos en el script 08
base = read.csv("23_Base_Modelo_Rendimiento.csv")

# Separamos entrenamiento (con rendimiento) y prediccion (sin rendimiento)
entrenamiento = filter(base, CONJUNTO == "ENTRENAMIENTO")   # 138 parcelas
prediccion    = filter(base, CONJUNTO == "PREDICCION")      # 59 parcelas

nrow(entrenamiento)   # Debe dar 138
nrow(prediccion)      # Debe dar 59

# MODELO COMPLETO ---------------------------------------------------------

# Ajustamos el modelo con las variables que pueden entrar
# (se dejaron las que no estaban muy correlacionadas entre si en el script 09)
modelo_completo = lm(RENDIMIENTO_T_HA ~ prec_siembra + tmin_ciclo + tmax_llenado +
                       msi_llenado + ndsvi_madurez +
                       evi_max_planet + evi_llenado_planet + dia_pico_planet +
                       elevacion_m + pendiente_grados + AREA_HA,
                     data = entrenamiento)

# summary: coeficientes, p-valores de cada variable y R2 del modelo
summary(modelo_completo)

# MODELO FINAL ------------------------------------------------------------

# step() va quitando una por una las variables que no aportan y se queda con el modelo de menor AIC
# (el AIC equilibra que tan bien se ajusta el modelo y que tan sencillo es)
# trace = 0: no muestra cada paso en la consola, solo el resultado final
modelo_final = step(modelo_completo, trace = 0)

# Variables con las que se quedo el modelo
formula(modelo_final)

summary(modelo_final)

# REVISION DE LOS RESIDUOS ------------------------------------------------

# Igual que en las series de tiempo: los residuos deben tener media cercana a 0,
# varianza constante y forma aproximadamente normal
residuos_final = residuals(modelo_final)

# Media de los residuos (en una regresion con intercepto siempre sale casi 0)
mean(residuos_final)

# Prueba de normalidad de Shapiro-Wilk: si p > 0.05 los residuos son aproximadamente normales
shapiro.test(residuos_final)

# Graficas de diagnostico del modelo (4 graficas en una ventana)
# 1. Residuos vs ajustados: no debe haber patron (varianza constante)
# 2. QQ: los puntos deben quedar cerca de la linea (normalidad)
# 3. Escala-ubicacion: otra forma de ver la varianza constante
# 4. Residuos vs influencia: detecta parcelas que pesan demasiado en el modelo
par(mfrow = c(2, 2))
plot(modelo_final)
par(mfrow = c(1, 1))

# Histograma de los residuos
# Si tiene forma de campana, los residuos son aproximadamente normales
hist(residuos_final,
     breaks = 20,
     main = "Residuos del modelo final",
     xlab = "Residuo",
     col = "lightblue")
abline(v = 0,
       col = "red",
       lwd = 2)

# VALIDACION CRUZADA ------------------------------------------------------

# Para saber que tan bien predice parcelas que NO vio, dividimos las 138 parcelas en 5 grupos
# Se entrena con 4 grupos y se predice el grupo que quedo fuera
# Se repite 5 veces, hasta que cada grupo se predijo una vez

# set.seed: para que los grupos salgan igual cada vez que se corre el codigo
set.seed(123)

# Asignamos cada parcela a un grupo al azar
grupo = sample(rep(1:5, length.out = nrow(entrenamiento)))

# Vector vacio para guardar las predicciones
prediccion_cv = numeric(nrow(entrenamiento))

for (g in 1:5) {
  
  # Entrenamos con 4 grupos (todas las parcelas menos las del grupo g)
  m = lm(formula(modelo_final), data = entrenamiento[grupo != g, ])
  
  # Predecimos las parcelas del grupo g, que el modelo no vio
  prediccion_cv[grupo == g] = predict(m, newdata = entrenamiento[grupo == g, ])
}

# Metricas: comparan el rendimiento real con el predicho
real = entrenamiento$RENDIMIENTO_T_HA

# RMSE: error promedio, en t/ha (castiga mas los errores grandes)
rmse = sqrt(mean((real - prediccion_cv)^2))

# MAE: error absoluto promedio, en t/ha
mae  = mean(abs(real - prediccion_cv))

# R2: proporcion del rendimiento que explica el modelo en parcelas que no vio
r2   = 1 - sum((real - prediccion_cv)^2) / sum((real - mean(real))^2)

round(c(RMSE = rmse, MAE = mae, R2 = r2), 3)

# Grafica: rendimiento real contra predicho
# Si el modelo predice bien, los puntos quedan cerca de la linea roja
plot(real, prediccion_cv,
     xlab = "Rendimiento real (t/ha)",
     ylab = "Rendimiento predicho (t/ha)",
     main = "Validacion cruzada",
     pch = 19,
     col = "darkgreen")
abline(0, 1, col = "red")

# PREDICCION DE LAS 59 PARCELAS QUE FALTAN --------------------------------

# Usamos el modelo final para predecir el rendimiento de las parcelas sin rendimiento
prediccion$RENDIMIENTO_PREDICHO = predict(modelo_final, newdata = prediccion)

# Revisamos
nrow(prediccion)                                # Debe dar 59
sum(is.na(prediccion$RENDIMIENTO_PREDICHO))     # Debe dar 0
summary(prediccion$RENDIMIENTO_PREDICHO)        # Rango de las predicciones (deben ser rendimientos razonables)

# Guardamos las predicciones
write_csv(prediccion[, c("ID_POLIGONO", "RENDIMIENTO_PREDICHO")], "24_Predicciones_Rendimiento.csv")